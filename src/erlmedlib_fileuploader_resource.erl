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
    %%Body = get_streamed_body(wrq:stream_req_body(ReqData, 100), []),
    Boundary = webmachine_multipart:find_boundary(ReqData),
    io:format("Boundary ~p~n",[ReqData]),
    {Part, Next} = webmachine_multipart:stream_parts(wrq:stream_req_body(ReqData, 100), Boundary),

    io:format("Part ~p~n",[Part]),
    NewReqData = wrq:set_resp_header("Content-type", "text/plain", wrq:set_resp_body(json_body([{success, "true"}]), ReqData)),
    {true, NewReqData, State}.

get_streamed_body({Hunk,done},Acc) ->
    io:format("RECEIVED ~p~n",[Hunk]),
    iolist_to_binary(lists:reverse([Hunk|Acc]));
get_streamed_body({Hunk,Next},Acc) ->
    io:format("RECEIVED ~p~n",[Hunk]),
    get_streamed_body(Next(),[Hunk|Acc]).

json_body(QS) -> mochijson:encode({struct, QS}).
