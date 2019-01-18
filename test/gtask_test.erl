-module (gtask_test).

-include_lib("eunit/include/eunit.hrl").

-export([task/1]).

api_test_() ->
    Grp1 = api_test,
    Grp2 = "api_test",
    BadGrp = {},
    TskA = fun () -> 2 + 2 end,
    TskB = fun () -> timer:sleep(100), 3 * 3 end,
    TskC = fun (_) -> ok end,
    TskD = {?MODULE, task, [100]},
    TskE = {?MODULE, foobar, []},
    Setup = fun () ->
                {ok, Apps} = application:ensure_all_started(gtask),
                syn:init(),
                Apps
            end,
    Cleanup = fun (Apps) ->
                  [ ok = application:stop(App) || App <- Apps ]
              end,
    Tests =
        [?_assertEqual(ok, gtask:new(Grp1)),
         ?_assertMatch({error, _}, gtask:new(Grp1)),
         ?_assertMatch({error, _}, gtask:new(Grp2, #{ max_workers => -1 })),
         ?_assertMatch({error, _}, gtask:new(Grp2, "not an options")),
         ?_assertEqual(ok, gtask:new(Grp2)),
         ?_assertEqual(ok, gtask:add(Grp1, TskA)),
         ?_assertEqual(ok, gtask:add(Grp1, TskB)),
         ?_assertMatch({error, _}, gtask:add(Grp1, TskC)),
         ?_assertMatch({error, _}, gtask:add(BadGrp, TskA)),
         ?_assertMatch({error, _}, gtask:add(Grp2, TskA, #{ timeout => -1 })),
         ?_assertMatch({error, _}, gtask:add(Grp2, TskA, "not an options")),
         ?_assertEqual(ok, gtask:add(Grp1, TskD)),
         ?_assertMatch({error, _}, gtask:add(Grp2, TskE)),
         ?_assertMatch({ok, L} when is_list(L) andalso length(L) == 3,
                       gtask:await(Grp1)),
         ?_assertMatch({error, _}, gtask:await(Grp2, -1)),
         ?_assertEqual({ok, []}, gtask:await(Grp2)),
         ?_assertMatch({error, _}, gtask:await(BadGrp)),
         ?_assertEqual(ok, gtask:delete(Grp1)),
         ?_assertMatch({error, _}, gtask:delete(Grp1)),
         ?_assertEqual(ok, gtask:delete(Grp2)),
         ?_assertMatch({error, _}, gtask:delete(Grp2)),
         ?_assertMatch({error, _}, gtask:delete(BadGrp))],
    {setup, Setup, Cleanup, Tests}.

task(Time) ->
    timer:sleep(Time),
    ok.
