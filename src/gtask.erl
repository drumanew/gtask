-module(gtask).

%% API exports
-export([new/1,
         add/2,
         await/1,
         delete/1]).

-type group()  :: atom().
-type task()   :: fun(() -> any()) | {module(), fun(), list(any())}.
-type error()  :: {error, Reason :: term()}.
-type result() :: [any()].

-spec new(group()) -> ok | error().
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

new(Group) when is_atom(Group) ->
    case gtask_srv:start_link(Group) of
        {ok, _Pid} -> ok;
        {error, _} = Error -> Error
    end;
new(_) ->
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

check_mfa(M, F, A) ->
    is_atom(M) andalso
    is_list(A) andalso
    erlang:function_exported(M, F, length(A)).
