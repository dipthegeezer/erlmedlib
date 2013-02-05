-module(erlmedlib_event_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

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

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    {ok, Args}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({process_event, Data}, State) ->
  {noreply, do_process_event(Data, State)};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%% do_process_event({Path, file, create, Cookie, Name}, State) ->
%% run through exiftool
%% Send produced data to couchdb
%%    State;
do_process_event({Path, file, delete, Cookie, Name}, State) ->
%% Find on couchdb using path and Name
%% Delete from couchdb
    State;
%% A file or directory was moved into a watched directory. This event occurs even if the file is simply moved from and to the same directory.
do_process_event({Path, file, move_to, Cookie, Name}, State) ->
%% Treat like a close_write
    State;
%% A file or directory was moved from a watched directory. This event occurs even if the file is simply moved from and to the same directory.
do_process_event({Path, file, move_from, Cookie, Name}, State) ->
%% Treat like a delete
    State;
do_process_event({Path, file, close_write, Cookie, Name}, State) ->
%% run through exiftool
%% Find on couchdb using path and Name
%% update couchdb
    State;

do_process_event({Path, dir, create, Cookie, Name}, State) ->
%% Watch directory
    State;
do_process_event({Path, dir, delete, Cookie, Name}, State) ->
%% Unwatch directory
    State;
%% A file or directory was moved to a watched directory. This event occurs even if the file is simply moved from and to the same directory.
do_process_event({Path, dir, move_to, Cookie, Name}, State) ->
%% Watch Directory
    State;
do_process_event({Path, dir, move_from, Cookie, Name}, State) ->
%% UnWatch Directory
    State;
do_process_event({Path, dir, modify, Cookie, Name}, State) ->
%% nothing?
    State.
