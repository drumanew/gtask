-module(gtask).

%% API exports
-export([new/1,
         new/2,
         add/2,
         await/1,
         delete/1]).

-type group()  :: atom().
-type opts()   :: #{ max_workers := pos_integer() }.
-type task()   :: fun(() -> any()) | {module(), fun(), list(any())}.
-type error()  :: {error, Reason :: term()}.
-type result() :: [any()].

-spec new(group()) -> ok | error().
-spec new(group(), opts()) -> ok | error().
-spec add(group(), task()) -> ok | error().
-spec await(group()) -> {ok, result()} | error().
-spec delete(group()) -> ok | error().

-define(noproc(Expr), case catch Expr of
                          ok -> ok;
                          {ok, _} = __Rep -> __Rep;
                          {'EXIT', noproc} -> {error, not_started};
                          {'EXIT', {noproc, _}} -> {error, not_started}
                      end).

%%====================================================================
%% API functions
%%====================================================================

new(Group) ->
  new(Group, default_opts()).

new(Group, Opts0) when is_atom(Group) andalso is_map(Opts0) ->
    Opts = maps:merge(default_opts(), Opts0),
    case check_opts(Opts) of
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

add(Group, Task = {M, F, A}) when is_atom(Group) ->
    case check_mfa(M, F, A) of
        true ->
            ?noproc(gen_server:call(Group, {add, Task}));
        false ->
            {error, badarg}
    end;
add(Group, Task) when is_atom(Group) andalso is_function(Task, 0) ->
    ?noproc(gen_server:call(Group, {add, Task}));
add(_, _) ->
    {error, badarg}.

await(Group) when is_atom(Group) ->
    ?noproc(gen_server:call(Group, await, 30000));
await(_) ->
    {error, badarg}.

delete(Group) when is_atom(Group) ->
    ?noproc(gen_server:stop(Group));
delete(_) ->
    {error, badarg}.

%%====================================================================
%% Internal functions
%%====================================================================

default_opts() ->
    #{ max_workers => 500 }.

check_opts(#{ max_workers := MaxWorkers }) when is_integer(MaxWorkers) andalso
                                                MaxWorkers > 0 ->
    true;
check_opts(_) ->
    false.

check_mfa(M, F, A) ->
    is_atom(M) andalso
    is_list(A) andalso
    erlang:function_exported(M, F, length(A)).
