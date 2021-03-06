-module (gtask_SUITE).

-export ([all/0,
          init_per_testcase/2,
          end_per_testcase/2]).

-export ([ mod_exists/1
         , simple/1
         , flow_control/1
         , task_timeout/1
         , task_expiration/1
         ]).

all() ->
    [ mod_exists
    , simple
    , flow_control
    , task_timeout
    , task_expiration
    ].

init_per_testcase(_Test, Config) ->
    {ok, Apps} = application:ensure_all_started(gtask),
    syn:init(),
    [{apps, Apps} | Config].

end_per_testcase(_Test, Config) ->
    Apps = proplists:get_value(apps, Config, []),
    [ ok = application:stop(App) || App <- Apps ].

mod_exists(_) ->
    {module, gtask} = code:load_file(gtask).

simple(_) ->
    Chars = lists:seq($a, $z),

    %% create groups
    ok = gtask:new(a),
    ok = gtask:new(b),
    ok = gtask:new(c),

    %% ensure groups are empty
    {ok, []} = gtask:await(a),
    {ok, []} = gtask:await(b),
    {ok, []} = gtask:await(c),

    %% add tasks
    [ ok = gtask:add(a, task(Char)) || Char <- Chars ],
    [ ok = gtask:add(b, task(Char)) || Char <- Chars ],
    [ ok = gtask:add(c, task(Char)) || Char <- Chars ],

    %% get result
    {ok, V1} = gtask:await(a),
    {ok, V2} = gtask:await(b),
    {ok, V3} = gtask:await(c),

    %% check result
    Chars = lists:map(fun ({done, V}) -> V end, lists:sort(V1)),
    Chars = lists:map(fun ({done, V}) -> V end, lists:sort(V2)),
    Chars = lists:map(fun ({done, V}) -> V end, lists:sort(V3)),

    %% ensure groups are empty
    {ok, []} = gtask:await(a),
    {ok, []} = gtask:await(b),
    {ok, []} = gtask:await(c),

    %% add tasks again
    [ ok = gtask:add(a, task(Char)) || Char <- Chars ],
    [ ok = gtask:add(b, task(Char)) || Char <- Chars ],
    [ ok = gtask:add(c, task(Char)) || Char <- Chars ],

    %% get result
    {ok, V4} = gtask:await(a),
    {ok, V5} = gtask:await(b),
    {ok, V6} = gtask:await(c),

    %% check result
    Chars = lists:map(fun ({done, V}) -> V end, lists:sort(V4)),
    Chars = lists:map(fun ({done, V}) -> V end, lists:sort(V5)),
    Chars = lists:map(fun ({done, V}) -> V end, lists:sort(V6)),

    %% ensure groups are empty
    {ok, []} = gtask:await(a),
    {ok, []} = gtask:await(b),
    {ok, []} = gtask:await(c),

    %% delete groups
    ok = gtask:delete(a),
    ok = gtask:delete(b),
    ok = gtask:delete(c),

    %% ensure adding is imposible
    [ {error, _} = gtask:add(a, task(Char)) || Char <- Chars ],
    [ {error, _} = gtask:add(b, task(Char)) || Char <- Chars ],
    [ {error, _} = gtask:add(c, task(Char)) || Char <- Chars ],

    %% ensure ensure awaiting is imposible
    {error, _} = gtask:await(a),
    {error, _} = gtask:await(b),
    {error, _} = gtask:await(c),

    %% ensure groups already removed
    {error, _} = gtask:delete(a),
    {error, _} = gtask:delete(b),
    {error, _} = gtask:delete(c),

    %% create groups again
    ok = gtask:new(a),
    ok = gtask:new(b),
    ok = gtask:new(c),

    %% ensure groups are empty
    {ok, []} = gtask:await(a),
    {ok, []} = gtask:await(b),
    {ok, []} = gtask:await(c),

    %% add tasks
    [ ok = gtask:add(a, task(Char)) || Char <- Chars ],
    [ ok = gtask:add(b, task(Char)) || Char <- Chars ],
    [ ok = gtask:add(c, task(Char)) || Char <- Chars ],

    %% get result
    {ok, V7} = gtask:await(a),
    {ok, V8} = gtask:await(b),
    {ok, V9} = gtask:await(c),

    %% check result
    Chars = lists:map(fun ({done, V}) -> V end, lists:sort(V7)),
    Chars = lists:map(fun ({done, V}) -> V end, lists:sort(V8)),
    Chars = lists:map(fun ({done, V}) -> V end, lists:sort(V9)),

    %% ensure groups are empty
    {ok, []} = gtask:await(a),
    {ok, []} = gtask:await(b),
    {ok, []} = gtask:await(c),

    %% delete groups
    ok = gtask:delete(a),
    ok = gtask:delete(b),
    ok = gtask:delete(c),

    ok.

flow_control(_) ->
    Group = flow_control_test,

    %% create group
    ok = gtask:new(Group, #{ max_workers => 10 }),

    %% add 50 tasks with 2sec delays
    [ ok = gtask:add(Group, fun () -> timer:sleep(2000) end)
      || _ <- lists:seq(1, 50) ],

    %% check queue size
    40 = maps:get(q_len, sys:get_state({via, syn, Group})),
    timer:sleep(2100),

    30 = maps:get(q_len, sys:get_state({via, syn, Group})),
    timer:sleep(2100),

    20 = maps:get(q_len, sys:get_state({via, syn, Group})),
    timer:sleep(2100),

    10 = maps:get(q_len, sys:get_state({via, syn, Group})),
    timer:sleep(2100),

    {ok, R} = gtask:await(Group),

    R = lists:duplicate(50, {done, ok}),

    %% add 20 tasks with 2sec delays
    [ ok = gtask:add(Group, fun () -> timer:sleep(2000) end)
      || _ <- lists:seq(1, 20) ],

    {error, timeout} = gtask:await(Group, 2),

    ok = gtask:delete(Group),

    ok.

task_timeout(_) ->
    Group = task_timeout_test,
    ok = gtask:new(Group),
    ok = gtask:add(Group, fun () -> timer:sleep(2000) end, #{ timeout => 1 }),
    {ok, [timeout]} = gtask:await(Group),
    ok.

task_expiration(_) ->
    Group = task_expiration_test,
    ok = gtask:new(Group, #{ max_workers => 5 }),
    [ ok = gtask:add(Group, fun () -> timer:sleep(5000) end)
      || _ <- lists:seq(1, 5) ],
    [ ok = gtask:add(Group, task("must expire"), #{ expire => 4 })
      || _ <- lists:seq(1, 5) ],
    {ok, R0} = gtask:await(Group),
    R1 = lists:sort(lists:flatten(lists:duplicate(5, {done, ok}),
                                  lists:duplicate(5, expired))),
    R1 = lists:sort(R0),
    ok.

%% private

task(Return) ->
    fun () ->
        T = rand:uniform(1000),
        timer:sleep(T),
        Return
    end.
