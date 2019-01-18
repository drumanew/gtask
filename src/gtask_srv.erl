-module(gtask_srv).
-behaviour(gen_server).

-export([start_link/2]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2]).

-record (entry, {ref, pid}).

-include_lib("stdlib/include/ms_transform.hrl").

%% API

start_link(Name, Opts) ->
    gen_server:start_link({via, syn, Name}, ?MODULE, Opts#{ name => Name }, []).

%% gen_server callbacks

init(Opts) ->
    process_flag(trap_exit, true),
    T = ets:new(undefined, [{keypos, #entry.ref}, protected, set]),
    State0 = #{ tab => T,
                reply_to => [],
                count => 0,
                values => [],
                queue => queue:new(),
                q_len => 0 },
    State = maps:merge(State0, Opts),
    {ok, State}.

handle_call({add, Task, Opts}, _From, State0) ->
    {Reply, State} = do_add(Task, Opts, State0),
    {reply, Reply, State};
handle_call(await, From, State0) ->
    {ok, State} = do_await(From, State0),
    {noreply, State};
handle_call(_Req, _From, State) ->
    {reply, ignore, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', From, Reason}, State0) ->
    {ok, State} = handle_exit(From, Reason, State0),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%% private

do_add(Task, Opts, State = #{ count := Count,
                              max_workers := Max,
                              queue := Q,
                              q_len := L }) when Count >= Max ->
    {ok, State#{ queue => queue:in({Task, Opts}, Q), q_len => L + 1 }};
do_add(Task, Opts, State) ->
    Now = erlang:monotonic_time(second),
    maybe_expired(Task, Opts, Now, State).

do_await(From, State0 = #{ reply_to := ReplyTo }) ->
    State = State0#{ reply_to => [From | ReplyTo] },
    maybe_reply(State).

handle_exit(_From, {report, Report}, State) ->
    %% maybe check From in Report?
    handle_report(Report, State);
handle_exit(From, Reason, State = #{ tab := T }) ->
    MatchSpec = ets:fun2ms(fun (E = #entry{ pid = P }) when P == From -> E end),
    Entries = ets:select(T, MatchSpec),
    handle_unexpected(Entries, Reason, State).

handle_report(_Report, State = #{ count := C }) when C == 0 ->
    %% BUG! report not expected!
    error_logger:error_msg("report not expected: ~p~n", [_Report]),
    {ok, State};
handle_report({Ref, Pid, Result}, State0 = #{ tab := T,
                                              values := V,
                                              count := C }) ->
    Entries = ets:lookup(T, Ref),
    case Entries of
        [#entry{ ref = Ref, pid = Pid }] ->
            ets:delete(T, Ref),
            State1 = State0#{ values => [Result | V], count => C - 1 },
            {ok, State2} = maybe_reply(State1),
            maybe_pop_queue(State2);
        [] ->
            %% BUG! no entries for report
            error_logger:error_msg("no entries for report: ~p~n",
                                   [{Ref, Pid, Result}]),
            {ok, State0}
    end.

handle_unexpected([], _Reason, State) ->
    {ok, State};
handle_unexpected([#entry{}], _Reason, _State) ->
    %% BUG! dont even want to handle it
    error_logger:error_msg("dont even want to handle it~n", []),
    exit(kill).

maybe_expired(_Task, #{ expire := Time }, Now, State0 = #{ values := V })
    when Now > Time ->
    error_logger:info_msg("group: ~p: task ~p expired~n",
                          [maps:get(name, State0), _Task]),
    State = State0#{ values => [ expired | V ] },
    maybe_reply(State);
maybe_expired(Task, Opts, _, State = #{ tab := T, count := Count }) ->
    Ref = erlang:make_ref(),
    Pid = spawn_task(Ref, Task, Opts),
    Entry = #entry{ ref = Ref, pid = Pid },
    ets:insert(T, Entry),
    {ok, State#{ count => Count + 1 }}.

maybe_reply(State = #{ reply_to := ReplyTo,
                       values := Values0,
                       count := 0,
                       q_len := 0 }) when ReplyTo /= [] ->
    Values = lists:reverse(Values0),
    [ gen_server:reply(From, {ok, Values}) || From <- ReplyTo ],
    {ok, State#{ reply_to := [], values := [] }};
maybe_reply(State) ->
    {ok, State}.

maybe_pop_queue(State = #{ q_len := 0 }) ->
    {ok, State};
maybe_pop_queue(State0 = #{ queue := Q0, q_len := QLen0 }) ->
    {{value, {Task, Opts}}, Q1} = queue:out(Q0),
    QLen1 = QLen0 - 1,
    do_add(Task, Opts, State0#{ queue => Q1, q_len => QLen1 }).

spawn_task(Ref, Task, #{ timeout := Timeout }) ->
    erlang:spawn_link(
        fun () ->
            Pid = self(),
            %% Spawn killer with no link, no need to monitor him
            erlang:spawn_link(
                fun () ->
                    timer:sleep(Timeout),
                    exit({report, {Ref, Pid, timeout}})
                end),
            try execute(Task) of
                Result ->
                    exit({report, {Ref, self(), {done, Result}}})
            catch
                exit:{report, _} = Report ->
                    exit(Report);
                _Class:Error ->
                    exit({report, {Ref, self(), {fail, Error}}})
            end
        end).

execute({M, F, A}) ->
    erlang:apply(M, F, A);
execute(Fun) when is_function(Fun, 0) ->
    Fun().
