-module(epush_client).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-record(state, {pending}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, subscribe/3]).

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


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    {ok, #state{pending=gb_trees:empty()}}.

handle_call({subscribe, URL, CallbackURL, PostData}, From, State=#state{pending=P}) ->
    io:format("epush: Subscribing...: ~p~n", [URL]),
    PostBody = encode_postdata([{callback_url, CallbackURL}|PostData]),
    {ok,RequestId} = httpc:request(post,{URL,[], "application/x-www-form-urlencoded", PostBody},[],[{sync,false}]),
    Pendings = gb_trees:insert(RequestId,{From,subscribe},P),
    {reply, ok, State#state{pending=Pendings}}.

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({http,{RequestId,HttpResponse}},State = #state{pending=P}) ->
    case gb_trees:lookup(RequestId,P) of
        {value,{Client,subscribe}} -> 
            JSON = mochijson2:decode(HttpResponse),
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

