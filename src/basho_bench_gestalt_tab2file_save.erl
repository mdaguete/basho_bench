-module(basho_bench_gestalt_tab2file_save).
-export([
         new/1,
         run/4
]).

-include("basho_bench.hrl").
-define(OPTIONS, [public, named_table, {read_concurrency, true}, {write_concurrency, true}]).

new({_,1,_}) ->
    Name = basho_bench_config:get(gestalt_tab_name),
    Cutoff = basho_bench_config:get(gestalt_key_cutoff),
    Pid = spawn(fun() -> spawn_ets(Name) end),
    ?INFO("Spawned ETS ~p", [Pid]),
    {ok, #{ tab => Name, cur => 0, cutoff => Cutoff }};
new(_Id) ->
    Name = basho_bench_config:get(gestalt_tab_name),
    Cutoff = basho_bench_config:get(gestalt_key_cutoff),
    ok = wait_for({fun() -> table_ready(Name) end, table_ready}, 10),
    {ok, #{ cutoff => Cutoff, cur => 0, tab => Name }}.

run(put, KeyGen, ValGen, #{ cutoff := Cutoff, cur := Cur } = S) ->
    NewState = case Cur == Cutoff of
        true ->
            do_save(S);
        false ->
            do_put(KeyGen, ValGen, S)
    end,
    {ok, NewState}.

do_save(S = #{ cutoff := Cutoff, tab := N }) ->
    wait_for({fun() -> ets_full(N, Cutoff+1) end, ets_full}, 10),
    D = basho_bench_config:get(gestalt_save_dir, "/tmp"),
    Fn = basho_bench_config:get(gestalt_file, gen_file_name("gestalt_save")),
    Path = filename:join(D, Fn),
    ok = filelib:ensure_dir(Path),
    {Time, ok} = timer:tc(fun() -> ets:tab2file(N, Path) end),
    ?INFO("Took ~w microseconds to store ~p~n", [Time, Path]),
    S#{ cur => Cutoff+1}.

do_put(KeyGen, ValGen, S = #{ tab := Name }) ->
    Key = KeyGen(),
    true = ets:insert(Name, {Key, ValGen()}),
    S#{ cur => Key }.

gen_file_name(Prefix) ->
    {{Y,M,D},{H,Mn,S}} = calendar:now_to_datetime(erlang:timestamp()),
    Prefix ++ "." ++ io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0wZ", [Y,M,D,H,Mn,S]).

spawn_ets(Name) ->
    Tid = ets:new(Name, ?OPTIONS),
    ?DEBUG("Table ~p created", [Tid]),
    receive
        stop -> ok
    end.

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

ets_full(N, Size) ->
    case ets:info(N, size) of
        Size -> true;
        _ -> false
    end.
