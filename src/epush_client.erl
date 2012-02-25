%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2012 Arjan Scherpenisse <arjan@scherpenisse.net>
%% Date: 2012-02-25

%% @doc Erlang PUbSubHubbub: client

%% Copyright 2012 Arjan Scherpenisse
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(epush_client).
-include("../include/epush.hrl").

-behaviour(gen_server).
-define(SERVER, ?MODULE).

-record(state, {pending, table}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, 
         subscribe/3,
         lookup_subscription/1
        ]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


-spec subscribe(string(), string(), [{atom(), string()}]) -> {ok, Token::string()}.
subscribe(URL, CallbackURL, PostData) ->
    gen_server:call(?SERVER, {subscribe, URL, CallbackURL, PostData}).

lookup_subscription(Identifier) ->
    gen_server:call(?SERVER, {lookup_subscription, Identifier}).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    {ok, Table} = dets:open_file(epush_subscriptions, [{auto_save, 10000}]),
    {ok, #state{pending=gb_trees:empty(),
               table=Table}}.


handle_call({lookup_subscription, Identifier}, _From, State=#state{table=T}) ->
    case dets:lookup(T, Identifier) of
        [Subscription] -> 
            {reply, {ok, Subscription}, State};
        [] ->
            {reply, {error, notfound}, State}
    end;

handle_call({subscribe, URL, CallbackURL, PostData}, From, State=#state{pending=P, table=T}) ->
    io:format("epush: Subscribing...: ~p~n", [URL]),
    VerifyToken = base64:encode(term_to_binary(erlang:now())),
    PostBody = encode_postdata([{callback_url, CallbackURL},
                                {verify_token, binary_to_list(VerifyToken)}
                                |PostData]),
    dets:insert(T, {VerifyToken, #epush_subscription{}}),
    {ok,RequestId} = httpc:request(post,{URL,[], "application/x-www-form-urlencoded", PostBody},[],[{sync,false}]),
    Pendings = gb_trees:insert(RequestId,{From,subscribe},P),
    {reply, ok, State#state{pending=Pendings}}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({http,{RequestId,HttpResponse}},State = #state{pending=P}) ->
    case gb_trees:lookup(RequestId,P) of
        {value,{Client,subscribe}} -> 
            {{_, 200, _}, _Headers, Body} = HttpResponse,
            JSON = mochijson2:decode(Body),
            io:format("~p~n", [JSON]),
            gen_server:reply(Client,{ok,JSON}),
            {noreply, State#state{pending=gb_trees:delete(RequestId,P)}};
        none -> 
            {noreply,State}
            %% the requestid isn't here, probably the request was deleted after a timeout
    end;

handle_info(_Info, State) ->
    io:format("epush: Unhandled message: ~p~n", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------




encode_postdata(List) ->
    string:join([atom_to_list(K)++"="++url_encode(V) || {K,V} <- List], "&"). 


%%%
                                                % URL encode - borrowed from CouchDB
                                                % borrowed again from http://weblog.plexobject.com/?p=1594
%%%
url_encode([H|T]) ->
    if
        H >= $a, $z >= H ->
            [H|url_encode(T)];
        H >= $A, $Z >= H ->
            [H|url_encode(T)];
        H >= $0, $9 >= H ->
            [H|url_encode(T)];
        H == $_; H == $.; H == $-; H == $: ->
            [H|url_encode(T)];
        true ->
            case lists:flatten(io_lib:format("~.16.0B", [H])) of
                [X, Y] ->
                    [$%, X, Y | url_encode(T)];
                [X] ->
                    [$%, $0, X | url_encode(T)]
            end
    end;
url_encode([]) ->
    [].

