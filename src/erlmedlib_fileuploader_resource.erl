-module(erlmedlib_fileuploader_resource).

-export([init/1,
         allowed_methods/2,
         process_post/2]).

-include_lib("webmachine/include/webmachine.hrl").

-record(context, {outdir}).

init([OutputDir]) -> {ok, #context{outdir=OutputDir}}.

allowed_methods(ReqData, Context) ->{['POST'], ReqData, Context}.

process_post(ReqData, Context) ->
    Boundary = webmachine_multipart:find_boundary(ReqData),
    io:format("Boundary ~p~n",[Boundary]),
    %% TODO:handle error from here:
    Parts = accumulate_stream_parts(webmachine_multipart:stream_parts(
                wrq:stream_req_body(ReqData, 1024), Boundary
              ),[]),
    io:format("Parts ~p~n", [Parts]),
    case write_to_disk(Parts, Context) of
        ok -> {true, success(ReqData), Context};
        {error, Msg} -> {true, failure(Msg, ReqData), Context};
        {prevent_retry, Msg} -> {true, prevent_retry(Msg, ReqData), Context};
        {reset, Msg} -> {true, reset(Msg, ReqData), Context}
    end.

accumulate_stream_parts(done_parts, Acc) ->
    %%io:format("RECEIVED ~p~n",[done_parts]),
    lists:reverse(Acc);
accumulate_stream_parts({Hunk,Next},Acc) ->
    %%io:format("RECEIVED ~p~n",[Hunk]),
    accumulate_stream_parts(Next(),[Hunk|Acc]).

write_to_disk(Parts, Context) ->
    %% TODO handle error here
    write_to_disk(Parts, Context, qqpartindex(Parts), qqtotalparts(Parts)-1).
write_to_disk(Parts, Context, Last, Last) ->
    case write_to_disk(Parts, Context, Last, Last+1) of
        ok ->%%TODO:get all files
            Root = Context#context.outdir,
            Filename = Root++"/"++"/"++qquuid(Parts)++"/"++qqfilename(Parts),
            case write_combined_parts( Filename, Files ) of
                ok -> ok; %% TODO check file is correct
                {error, Error} -> {error, Error}
            end;
        {error,Error} -> {error,Error}
    end;
write_to_disk(Parts, Context, Index, _TotalIndex) ->
    %% TODO construct part filename here
    Root = Context#context.outdir,
    Filename = Root++"/"++"/"++qquuid(Parts)++"/"++ qqfilename(Parts) ++"_"++Index,
    Bytes = qqfile(Parts),
    case file:write_file(Filename, Bytes) of
        ok -> ok; %%TODO check filesize
        {error, Reason} -> {error, Reason}
    end.


%%write parts to one file
%%returns ok | {error, Reason}
write_combined_parts(Filename, FileParts) when is_list(Filename) ->
    case file:open(Filename, [append]) of
        {ok, Handle} ->
            case write_combined_parts(Handle, FileParts) of
                ok -> ok;
                {error, Reason} ->
                    {error, Reason},
                    file:delete(Filename)
            end;
        {error, Reason} -> {error, Reason}
    end;
write_combined_parts(Handle,[]) ->
    file:close(Handle);
write_combined_parts(Handle,[H|T]) ->
    case file:read_file(H) of
        {ok, Binary} ->
            case file:write(Handle, Binary) of
                ok -> write_combined_parts(Handle,[T]);
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} -> {error, Reason}
    end.

%%params from body
%%TODO: String->Int
qqfilename(Parts) ->
    binary_to_list(get_param("qqfilename", Parts)).

qqtotalparts(Parts) ->
    binary_to_list(get_param("qqtotalparts", Parts)).

qqtotalfilesize(Parts) ->
    binary_to_list(get_param("qqtotalfilesize", Parts)).

qqpartindex(Parts) ->
    binary_to_list(get_param("qqpartindex", Parts)).

qquuid(Parts) ->
    binary_to_list(get_param("qquuid", Parts)).

qqfile(Parts) ->
    get_param("qqfile", Parts).

get_param(Name, Parts) ->
    {Name, _, Val} = proplists:lookup(Name, Parts),
    Val.

%%responses

success(ReqData) ->
    success("true", ReqData).

success(BooleanStr, ReqData) ->
    build_response([{success, BooleanStr}], ReqData).

reset(Msg, ReqData) ->
    build_response([{success, "false"}, {error, Msg}, {reset, "true"}], ReqData).

prevent_retry(Msg, ReqData) ->
    build_response([{success, "false"}, {error, Msg}, {preventRetry, "true"}], ReqData).

failure(Msg, ReqData) ->
    build_response([{error, Msg}], ReqData).

build_response(Status, ReqData)->
    wrq:set_resp_header(
      "Content-type", "text/plain",
      response_body(Status),
      ReqData
    ).

response_body(Status) ->
    wrq:set_resp_body(
      mochijson:encode(
        {struct, [Status]}
      )
    ).
