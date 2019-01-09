-module(gtask_srv).
-behaviour(gen_server).

-export([start_link/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2]).

-record (entry, {ref, pid}).

-define (TASK_TIMEOUT, 15000).

%% API

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

%% gen_server callbacks

init(_Args) ->
    process_flag(trap_exit, true),
    T = ets:new(undefined, [{keypos, #entry.ref}, protected, set]),
    {ok, #{ tab => T, reply_to => [], count => 0, values => [] }}.

handle_call({add, Task}, _From, State0) ->
    {Reply, State} = do_add(Task, State0),
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

do_add(Task, State = #{ tab := T, count := Count }) ->
    Ref = erlang:make_ref(),
    Pid = spawn_task(Ref, Task),
    Entry = #entry{ ref = Ref, pid = Pid },
    ets:insert(T, Entry),
    {ok, State#{ count => Count + 1 }}.

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
handle_report({Ref, Pid, Result}, State0 = #{ tab := T, values := V, count := C }) ->
    Entries = ets:lookup(T, Ref),
    case Entries of
        [#entry{ ref = Ref, pid = Pid }] ->
            ets:delete(T, Ref),
            State = State0#{ values => [Result | V], count => C - 1 },
            maybe_reply(State);
        [] ->
            %% BUG! no entries for report
            error_logger:error_msg("no entries for report: ~p~n", [{Ref, Pid, Result}]),
            {ok, State0}
    end.

handle_unexpected([], _Reason, State) ->
    {ok, State};
handle_unexpected([#entry{}], _Reason, _State) ->
    %% BUG! dont even want to handle it
    error_logger:error_msg("dont even want to handle it~n", []),
    exit(kill).

maybe_reply(State = #{ reply_to := ReplyTo, values := V, count := 0 }) when ReplyTo /= [] ->
    [ gen_server:reply(From, {ok, V}) || From <- ReplyTo ],
    {ok, State#{ reply_to := [], values := [] }};
maybe_reply(State) ->
    {ok, State}.

spawn_task(Ref, Task) ->
    erlang:spawn_link(
        fun () ->
            Pid = self(),
            %% Spawn killer with no link, no need to monitor him
            erlang:spawn_link(
                fun () ->
                    timer:sleep(?TASK_TIMEOUT),
                    exit({report, {Ref, Pid, timeout}})
                end),
            try Task() of
                Result ->
                    exit({report, {Ref, self(), {done, Result}}})
            catch
                exit:{report, _} = Report ->
                    exit(Report);
                Class:Error ->
                    exit({report, {Ref, self(), {fail, {Class, Error}}}})
            end
        end).
