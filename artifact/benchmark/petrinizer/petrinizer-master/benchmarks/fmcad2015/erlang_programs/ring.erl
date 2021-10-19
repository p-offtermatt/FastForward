-module(ring).

-export([main/0]).

-include_lib("soter.hrl").
-include_lib("grammars.hrl").

%-uncoverable("slave_mb >= 2").

main()->
    P = init_ring(fun(A,B)->slave(A,B) end,[zero,{s,zero},{s,{s,zero}}]),
    probe_ring(P).

probe_ring(P) ->
    P ! {peek, zero, self()},
    receive
        {ans, _} -> hurray
    end,
    probe_ring(P).

init_ring(Fun,List) ->
    spawn(fun()-> bootstrap_ring(Fun,List) end).

bootstrap_ring(Fun, Xs) ->
    bootstrap_ring(Fun, self(), Xs).
bootstrap_ring(Fun, Prev, [X]) ->
    Fun(Prev, X);
bootstrap_ring(Fun, Prev, [X | Xs]) ->
    Nxt = spawn(fun()-> Fun(Prev, X) end),
    bootstrap_ring(Fun, Nxt, Xs).

slave(Nxt, Me) ->
    ?label_mail('slave_mb', Nxt),
    receive
        {forward, X} -> Nxt ! {forward, [Me|X]};
        {peek, X, From} ->
            Nxt ! {forward, [Me|X]},
            receive
                {forward, Y} -> From ! {ans, Y}
            end
    end,
    slave(Nxt, Me).
