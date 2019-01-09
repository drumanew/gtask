-module (gtask_test).

-include_lib("eunit/include/eunit.hrl").

api_test_() ->
  Grp = api_test,
  BadGrp = "api_test",
  TskA = fun () -> 2 + 2 end,
  TskB = fun () -> timer:sleep(100), 3 * 3 end,
  TskC = fun (_) -> ok end,
  [?_assertEqual(ok, gtask:new(Grp)),
   ?_assertMatch({error, _}, gtask:new(Grp)),
   ?_assertMatch({error, _}, gtask:new(BadGrp)),
   ?_assertEqual(ok, gtask:add(Grp, TskA)),
   ?_assertEqual(ok, gtask:add(Grp, TskB)),
   ?_assertMatch({error, _}, gtask:add(Grp, TskC)),
   ?_assertMatch({error, _}, gtask:add(BadGrp, TskA)),
   ?_assertMatch({ok, L} when is_list(L), gtask:await(Grp)),
   ?_assertMatch({error, _}, gtask:await(BadGrp)),
   ?_assertEqual(ok, gtask:delete(Grp)),
   ?_assertMatch({error, _}, gtask:delete(Grp)),
   ?_assertMatch({error, _}, gtask:delete(BadGrp))].
