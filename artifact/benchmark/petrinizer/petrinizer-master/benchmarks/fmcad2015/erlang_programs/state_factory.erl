-module(state_factory).

-export([main/0]).

-include_lib("soter.hrl").
-include_lib("grammars.hrl").

%-soter_config(peano).

-uncoverable("state_mail > 1").
-uncoverable("after_receive > 0, state_mail > 0").

state(N, NewState) ->
    ?label_mail("state_mail"),
    receive
        {P,In}->
            ?label("after_receive"),
            M = NewState(N,In),
            P ! M,
            state(M, NewState)
    end.

factory(N, NewState) ->
    P=spawn(fun() -> state(N,NewState) end),
    fun(In)->
            P!{self(),In},
            receive
                Out-> Out
            end
    end.

main() ->
    FunWithState = factory(2000, fun(_X,_Y) -> _X + _Y end),
    call_loop(100, FunWithState).

call_loop(1,Fun) -> Fun(100);
call_loop(N,Fun) -> Fun(100),
                  call_loop(N-1,Fun).


