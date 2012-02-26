%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2012 Arjan Scherpenisse <arjan@scherpenisse.net>
%% Date: 2012-02-25

%% @doc Erlang PUbSubHubbub: cowboy handler for subscription requests.

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

-module(epush_subscription_handler).

-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/2]).

-record(state, {callback}).

init({_Any, http}, Req, [{callback, C}]) ->
    {ok, Req, #state{callback=C}}.

handle(Req, State=#state{callback=C}) ->

    case cowboy_http_req:method(Req) of
        {'POST', Req2} ->
            %% Subscription items
            {ok, Body, Req3} = cowboy_http_req:body(Req2),
            %% Perform the callback
            C(Body),
            {ok, Req4} = cowboy_http_req:reply(200, [{'Content-Type', <<"text/plain">>}], "", Req3),
            {ok, Req4, State};

        {'GET', Req2} ->
            %% Subscription verification
            {Token, Req3} = cowboy_http_req:qs_val(<<"hub.verify_token">>, Req2),
            {Challenge, Req4} = cowboy_http_req:qs_val(<<"hub.challenge">>, Req3),
            %% Lookup the subscription
            Req5 = case epush_client:lookup_subscription(Token) of
                       {ok, _Subscription} ->
                           io:format("YAAAY~n"),
                           {ok, R} = cowboy_http_req:reply(200, [{'Content-Type', <<"text/plain">>}], Challenge, Req4),
                           R;
                       {error, notfound} ->
                           {ok, R} = cowboy_http_req:reply(404, [], "Subscription not found", Req4),
                           R
                   end,
            {ok, Req5, State}
    end.

terminate(_Req, _State) ->
    ok.
