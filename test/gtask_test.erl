-module (gtask_test).

-include_lib("eunit/include/eunit.hrl").

-export([task/1]).

api_test_() ->
  Grp = api_test,
  BadGrp = "api_test",
  TskA = fun () -> 2 + 2 end,
  TskB = fun () -> timer:sleep(100), 3 * 3 end,
  TskC = fun (_) -> ok end,
  TskD = {?MODULE, task, [100]},
  TskE = {?MODULE, foobar, []},
  [?_assertEqual(ok, gtask:new(Grp)),
   ?_assertMatch({error, _}, gtask:new(Grp)),
   ?_assertMatch({error, _}, gtask:new(BadGrp)),
   ?_assertMatch({error, _}, gtask:new(Grp, #{ max_workers => -1 })),
   ?_assertEqual(ok, gtask:add(Grp, TskA)),
   ?_assertEqual(ok, gtask:add(Grp, TskB)),
   ?_assertMatch({error, _}, gtask:add(Grp, TskC)),
   ?_assertMatch({error, _}, gtask:add(BadGrp, TskA)),
   ?_assertMatch({error, _}, gtask:add(Grp, TskA, #{ timeout => -1 })),
   ?_assertEqual(ok, gtask:add(Grp, TskD)),
   ?_assertMatch({error, _}, gtask:add(Grp, TskE)),
   ?_assertMatch({ok, L} when is_list(L) andalso length(L) == 3,
                 gtask:await(Grp)),
   ?_assertMatch({error, _}, gtask:await(BadGrp)),
   ?_assertEqual(ok, gtask:delete(Grp)),
   ?_assertMatch({error, _}, gtask:delete(Grp)),
   ?_assertMatch({error, _}, gtask:delete(BadGrp))].

task(Time) ->
  timer:sleep(Time),
  ok.
