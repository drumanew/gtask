-module(gtask).

%% API exports
-export([new/1,
         new/2,
         add/2,
         add/3,
         await/1,
         await/2,
         delete/1]).

-type group()       :: any().
-type create_opts() :: #{ max_workers := pos_integer() }.
-type task()        :: fun(() -> any()) | {module(), fun(), list(any())}.
-type task_opts()   :: #{ timeout := timeout() }.
-type error()       :: {error, Reason :: term()}.
-type report()      :: {done, any()} | timeout | {fail, Reason :: term()}.
-type reports()     :: list(report()).

-spec new(group()) -> ok | error().
-spec new(group(), create_opts()) -> ok | error().
-spec add(group(), task()) -> ok | error().
-spec add(group(), task(), task_opts()) -> ok | error().
-spec await(group()) -> {ok, reports()} | error().
-spec delete(group()) -> ok | error().

-define(is_timeout(T), ((T == infinity) orelse (is_integer(T) andalso T >= 0))).
-define(is_group(_), true).

-define(DEFAULT_AWAIT_TIMEOUT, 30000).
-define(DEFAULT_MAX_WORKERS,  100).
-define(DEFAULT_TASK_TIMEOUT, 15000).

-define(catchsome(Expr),
        case catch Expr of
            ok -> ok;
            {ok, _} = __Rep -> __Rep;
            {'EXIT', noproc} -> {error, not_started};
            {'EXIT', {timeout, _}} -> {error, timeout};
            {'EXIT', {noproc, _}} -> {error, not_started}
        end).

%%====================================================================
%% API functions
%%====================================================================

new(Group) ->
    new(Group, default_opts(create)).

new(Group, Opts0) when ?is_group(Group) andalso is_map(Opts0) ->
    Opts = maps:merge(default_opts(create), Opts0),
    case check_opts(create, Opts) of
        true ->
            case gtask_srv:start_link(Group, Opts) of
                {ok, _Pid} -> ok;
                {error, _} = Error -> Error
            end;
        _ ->
            {error, badarg}
    end;
new(_, _) ->
    {error, badarg}.

add(Group, Task) ->
    add(Group, Task, default_opts(task)).

add(Group, Task, Opts0) when ?is_group(Group) andalso is_map(Opts0) ->
    case check_task(Task) of
        true ->
            Opts = maps:merge(default_opts(task), Opts0),
            case check_opts(task, Opts) of
                true ->
                    ?catchsome(call(Group, {add, Task, Opts}));
                false ->
                    {error, badarg}
            end;
        _ ->
            {error, badarg}
    end;
add(_, _, _) ->
    {error, badarg}.

await(Group) ->
    await(Group, ?DEFAULT_AWAIT_TIMEOUT).

await(Group, Timeout) when ?is_group(Group) andalso ?is_timeout(Timeout) ->
    ?catchsome(call(Group, await, Timeout));
await(_, _) ->
    {error, badarg}.

delete(Group) when ?is_group(Group) ->
    ?catchsome(gen_server:stop({via, syn, Group})).

%%====================================================================
%% Internal functions
%%====================================================================

call(Group, Req) ->
    call(Group, Req, 5000).

call(Group, Req, Timeout) ->
    gen_server:call({via, syn, Group}, Req, Timeout).

default_opts(create) ->
    #{ max_workers => ?DEFAULT_MAX_WORKERS };
default_opts(task) ->
    #{ timeout => ?DEFAULT_TASK_TIMEOUT }.

check_opts(create, #{ max_workers := MaxWorkers }) ->
    is_integer(MaxWorkers) andalso MaxWorkers > 0;
check_opts(task, #{ timeout := Timeout }) ->
    ?is_timeout(Timeout).

check_task({M, F, A}) ->
    is_atom(M) andalso
    is_list(A) andalso
    erlang:function_exported(M, F, length(A));
check_task(Task) ->
    is_function(Task, 0).
