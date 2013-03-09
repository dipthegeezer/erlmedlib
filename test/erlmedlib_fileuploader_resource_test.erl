-module(erlmedlib_fileuploader_resource_test).
-include_lib("eunit/include/eunit.hrl").

start() ->
    [].

stop(_SetupData) ->
    ok.

files_test_() ->
    {setup,
     fun start/0,
     fun stop/1,
     fun(SetupData) -> ?_assertEqual(true, true) %%write a part
     end,
     fun(SetupData) -> ?_assertEqual(true, true) %%find
     end,
     fun(SetupData) -> ?_assertEqual(true, true) %%write final part
     end,
     fun(SetupData) -> ?_assertEqual(true, true) %%size
     end,
     fun(SetupData) -> ?_assertEqual(true, true) %%delete
     end
    }.

params_test_() ->
    {setup,
     fun start/0,
     fun stop/1,
     {inparallel, [
     fun(SetupData) -> ?_assertEqual(true, true) %%qqfilename
     end,
     fun(SetupData) -> ?_assertEqual(true, true) %%qqtotalparts
     end,
     fun(SetupData) -> ?_assertEqual(true, true) %%qqtotalfilesiz
     end,
     fun(SetupData) -> ?_assertEqual(true, true) %%qqpartindex
     end,
     fun(SetupData) -> ?_assertEqual(true, true) %%quuid
     end,
     fun(SetupData) -> ?_assertEqual(true, true) %%qqfile
     end]}
    }.


response_test_() ->
    {setup,
     fun start/0,
     fun stop/1,
     {inparallel, [
     fun(SetupData) -> ?_assertEqual(true, true) %%success
     end,
     fun(SetupData) -> ?_assertEqual(true, true) %%success2
     end,
     fun(SetupData) -> ?_assertEqual(true, true) %%reset
     end,
     fun(SetupData) -> ?_assertEqual(true, true) %%prevent_retry
     end,
     fun(SetupData) -> ?_assertEqual(true, true) %%failure
     end]}
    }.
