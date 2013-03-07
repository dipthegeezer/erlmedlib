%% @author Bryan Fink <bryan@basho.com>
%% @author Andy Gross <andy@basho.com>
%% @author Justin Sheehy <justin@basho.com>
%% @copyright 2008-2009 Basho Technologies, Inc.

-module(erlmedlib_fileuploader_resource).

-export([init/1,
         allowed_methods/2,
         process_post/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.

allowed_methods(ReqData, Context) ->{['POST'], ReqData, Context}.

process_post(ReqData, State) ->
    Boundary = webmachine_multipart:find_boundary(ReqData),
    io:format("Boundary ~p~n",[Boundary]),
    Parts = accumulate_stream_parts(webmachine_multipart:stream_parts(
                wrq:stream_req_body(ReqData, 1024), Boundary
              ),[]),
    io:format("Parts ~p~n", [Parts]),
    NewReqData = wrq:set_resp_header("Content-type", "text/plain", wrq:set_resp_body(json_body([{success, "true"}]), ReqData)),
    {true, NewReqData, State}.

accumulate_stream_parts(done_parts, Acc) ->
    %%io:format("RECEIVED ~p~n",[done_parts]),
    lists:reverse(Acc);
accumulate_stream_parts({Hunk,Next},Acc) ->
    %%io:format("RECEIVED ~p~n",[Hunk]),
    accumulate_stream_parts(Next(),[Hunk|Acc]).

json_body(QS) -> mochijson:encode({struct, QS}).
