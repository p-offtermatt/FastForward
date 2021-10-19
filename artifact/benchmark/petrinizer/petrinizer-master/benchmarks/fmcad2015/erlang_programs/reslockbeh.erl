-module(reslockbeh).

-export([main/0]).

-include_lib("soter.hrl").
-include_lib("grammars.hrl").

%-soter_config(peano).
%-uncoverable("critical >= 2").

res_start(Res) -> spawn(fun()->res_free(Res) end).

res_free(Res) ->
    receive
        {lock, P} ->
            P ! {acquired, self()},
            res_locked(Res, P)
    end.

res_locked(Res, P) ->
    receive
        {req, P, Cmd} ->
            {NewRes, R} = Res(P, Cmd),
            case R of
                ok ->
                    res_locked(NewRes, P);
                {reply, A} ->
                    P ! {ans, self(), A},
                    res_locked(NewRes, P)
            end;
        {unlock, P} ->
            res_free(Res)
    end.

res_lock(Q) ->
    Q ! {lock, self()},
    receive
        {acquired, Q} -> ok
    end.

res_unlock(Q) ->
    Q ! {unlock, self()}, ok.

res_request(Q, Cmd) ->
    Q ! {req, self(), Cmd},
    receive
        {ans, Q, X} -> X
    end.

res_do(Q, Cmd) ->
    Q ! {req, self(), Cmd}, ok.

inst_start() -> res_start(inst({s, {s, {s, {s, zero}}}})).
inst(_X) ->
    fun(_P, _Cmd)->
        {
            inst({s, {s, {s, {s, zero}}}}),
%            ?SoterOneOf([
%                ok,
                { reply, any_ans() }
%            ])
        }
    end.

any_ans() ->
%    ?SoterOneOf([
%        ans1,
        {ans2,{s, {s, {s, {s, zero}}}}}.
%    ]).

any_client(C) ->
    res_lock(C),
    ?label(critical),
    any_interaction(C),
    res_unlock(C),
    any_client(C).

any_interaction(C) ->
    soter:choice(
        fun() -> ok end,
        fun() -> res_do(C, cmd),
                 any_interaction(C)
        end.
        fun() -> res_request(C, req),
                 any_interaction(C)
        end
    ).

main() ->
    C = inst_start(),
    many_clients(C, {s, {s, {s, {s, zero}}}}).

% ?any_peano is defined in grammars.hrl (automatically included)
% and generates all the terms of the form X ::= zero | {s, X}

many_clients(_, zero) -> ok;
many_clients(C, {s, N}) ->
    spawn(fun()->any_client(C) end),
    many_clients(C, N).


