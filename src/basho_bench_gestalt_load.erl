-module(basho_bench_gestalt_load).
-export([
         new/1,
         run/4
]).

-include("basho_bench.hrl").
-include_lib("kernel/include/file.hrl").
-define(MB, (1024*1024)).

new({_,1,_}) ->
    D = basho_bench_config:get(gestalt_save_dir, "/tmp"),
    Fns = get_all_load_files(D, "gestalt_save"),
    File = pick_one(Fns),
    {ok, Info} = file:read_file_info(File),
    {Time, {ok, Name}} = timer:tc(fun() -> ets:file2tab(File) end),
    ?INFO("Loaded file ~p (~w MB) in ~w microseconds~n",
          [File, Info#file_info.size div ?MB, Time]),
    Pid = spawn(fun() -> ets_owner() end),
    ets:give_away(Name, Pid, []),
    {ok, #{ tab => Name }};
new(_Id) ->
    Name = basho_bench_config:get(gestalt_tab_name),
    ok = wait_for({fun() -> table_ready(Name) end, table_ready}, 10),
    {ok, #{ tab => Name }}.

run(get, KeyGen, _ValGen, #{ tab := Name } = S) ->
    Key = KeyGen(),
    _ = ets:lookup(Name, Key),
    {ok, S}.

ets_owner() ->
    receive
        stop -> ok;
        Other ->
            ?DEBUG("Got ~p", [Other]),
            ets_owner()
    end.

pick_one(L) when is_list(L) ->
    lists:nth(length(L), L).

get_all_load_files(Dir, Prefix) ->
    filelib:wildcard(filename:join(Dir, Prefix ++ "*")).

wait_for(_Fun, 0) -> error({error, ets_timeout});
wait_for({Fun, Label}, R) ->
    case Fun() of
        true -> ok;
        false ->
            ?DEBUG("Waiting for ~p: ~p", [Label, R]),
            timer:sleep(100 * R),
            wait_for({Fun, Label}, R-1)
    end.

table_ready(N) ->
    case ets:info(N, name) of
        undefined -> false;
        N -> true
    end.
