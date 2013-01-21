%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the erlmedlib application.

-module(erlmedlib_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for erlmedlib.
start(_Type, _StartArgs) ->
    erlmedlib_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for erlmedlib.
stop(_State) ->
    ok.
