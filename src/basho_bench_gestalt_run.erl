-module(basho_bench_gestalt_run).
-export([
         new/1,
         run/4
]).

-include("basho_bench.hrl").
-define(OPTIONS, [public, named_table, {read_concurrency, true}, {write_concurrency, true}]).

new({_,1,_}) ->
    D = basho_bench_config:get(gestalt_save_dir, "/tmp/gtest"),
    Name = basho_bench_config:get(gestalt_tab_name),
    Pid = spawn(fun() -> spawn_ets(Name) end),
    ?INFO("Spawned ETS ~p", [Pid]),
    gestalt:start(),
    I = basho_bench_config:get(gestalt_key_interval, 5000),
    {ok, GPid} = gestalt_sup:start_new_manager(Pid, Name, D, I),
    basho_bench_config:set(gestalt_manager_pid, GPid),
    {ok, #{ tab => Name, gpid => GPid }};
new(_Id) ->
    Name = basho_bench_config:get(gestalt_tab_name),
    ok = wait_for({fun() -> table_ready(Name) end, table_ready}, 10),
    GPid = basho_bench_config:get(gestalt_manager_pid),
    {ok, #{ tab => Name, gpid => GPid }}.

run(put, KeyGen, ValGen, #{ tab := Name, gpid := GPid } = S) ->
    K = KeyGen(),
    true = ets:insert(Name, {K, ValGen()}),
    gestalt_mgr:queue_store(GPid, K),
    {ok, S};

run(get, KeyGen, _ValGen, #{ tab := Name, gpid := GPid } = S) ->
    K = KeyGen(),
    case ets:lookup(Name, K) of
        [] -> gestalt_mgr:load(GPid, K);
        [_] -> ok
    end,
    {ok, S}.

spawn_ets(Name) ->
    Tid = ets:new(Name, ?OPTIONS),
    ?DEBUG("Table ~p created", [Tid]),
    loop().

loop() ->
    receive
        stop -> ok;
        Other ->
            ?DEBUG("Got ~p", [Other]),
            loop()
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
