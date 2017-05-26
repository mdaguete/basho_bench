-module(basho_bench_gestalt_base).
-export([
         new/1,
         run/4
]).

-include("basho_bench.hrl").
-define(OPTIONS, [public, named_table, {read_concurrency, true}, {write_concurrency, true}]).

new({_,1,_}) ->
    Name = basho_bench_config:get(gestalt_tab_name),
    Pid = spawn(fun() -> spawn_ets(Name) end),
    ?INFO("Spawned ETS ~p", [Pid]),
    {ok, #{ tab => Name }};
new(_Id) ->
    Name = basho_bench_config:get(gestalt_tab_name),
    ok = wait_for({fun() -> table_ready(Name) end, table_ready}, 10),
    {ok, #{ tab => Name }}.

run(put, KeyGen, ValGen, #{ tab := Name } = S) ->
    K = KeyGen(),
    true = ets:insert(Name, {K, ValGen()}),
    {ok, S};

run(get, KeyGen, _ValGen, #{ tab := Name } = S) ->
    K = KeyGen(),
    case ets:lookup(Name, K) of
        [] -> not_found;
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
