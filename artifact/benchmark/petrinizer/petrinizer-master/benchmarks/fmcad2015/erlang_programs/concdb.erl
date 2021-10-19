-module(concdb).

-export([main/0]).

-include_lib("soter.hrl").
-include_lib("grammars.hrl").

%-uncoverable("client_writes >= 2").

main() ->
    DB = spawn(fun()->dataBase([])end),
    spawnmany(fun()->client(DB) end).

spawnmany(F) ->
    spawn(F),
    case ?any_bool() of
        false -> ok;
        true -> spawnmany(F)
    end.

dataBase(L) ->
    receive
        {allocate,Key,P} ->
            case lookup(Key,L) of
                fail ->
                    P!free,
                    receive
                        {value,V,P} ->
                            dataBase([{Key,V}|L])
                    end;
                {succ,V} ->
                    P!allocated,
                    dataBase(L)
            end;
        {lookup,Key,P} ->
            P!lookup(Key,L),
            dataBase(L)
    end.

lookup(K,L) ->
    case L of
        []        -> fail;
        [{K,V}|_] -> {succ,V};
        [_|Xs]    -> lookup(K,Xs)
    end.

client(DB) ->
    case read() of
        {ok,i} ->
            K = readKey(),
            DB!{allocate,K,self()},
            receive
                free ->
                    V = readVal(),
                    ?label(client_writes),
                    DB!{value,V,self()},
                    client(DB);
                allocated ->
                    ?label(client_denied),
                    client(DB)
            end;
        {ok,l} ->
            K = readKey(),
            DB!{lookup,K,self()},
            receive
                fail -> ?label(client_fail),client_not_found(DB, K);
                {succ,V} -> ?label(client_reads), client_found(DB, K, V)
            end,
            client(DB)
    end.

client_found(DB,_,_) -> client(DB).
client_not_found(DB,_) -> client(DB).

read() -> ?SoterOneOf([{ok,i}, {ok,l}]).
% SoterOneOf is defined in grammars.hrl (automatically imported)

readVal() -> ?any_peano().

readKey() -> ?any_peano().

