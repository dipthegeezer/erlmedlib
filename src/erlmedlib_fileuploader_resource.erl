%% @author Bryan Fink <bryan@basho.com>
%% @author Andy Gross <andy@basho.com>
%% @author Justin Sheehy <justin@basho.com>
%% @copyright 2008-2009 Basho Technologies, Inc.

-module(erlmedlib_fileuploader_resource).
-include_lib("webmachine/include/webmachine.hrl").

-export([init/1,
         allowed_methods/2,
         process_post/2]).

init([]) -> {ok, undefined}.

allowed_methods(ReqData, Context) ->{['POST'], ReqData, Context}.

process_post(ReqData, State) ->
    Body = get_streamed_body(wrq:stream_req_body(ReqData, 3), []),
    {true, wrq:set_resp_body({stream, send_streamed_body(Body,4)},ReqData), State}.

send_streamed_body(Body, Max) ->
    HunkLen=8*Max,
    case Body of
        <<Hunk:HunkLen/bitstring, Rest/binary>> ->
            io:format("SENT ~p~n",[Hunk]),
            {Hunk, fun() -> send_streamed_body(Rest,Max) end};
        _ ->
            io:format("SENT ~p~n",[Body]),
            {Body, done}
    end.

get_streamed_body({Hunk,done},Acc) ->
    io:format("RECEIVED ~p~n",[Hunk]),
    iolist_to_binary(lists:reverse([Hunk|Acc]));
get_streamed_body({Hunk,Next},Acc) ->
    io:format("RECEIVED ~p~n",[Hunk]),
    get_streamed_body(Next(),[Hunk|Acc]).



