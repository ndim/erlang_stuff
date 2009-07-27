%% Show how fast sending a message to and receiving an answer back
%% from a process is:
%% NOP test
%%   Start: {1248,709798,73040}
%%   End:   {1248,709798,155169}
%%   Delta: 82129 us
%%   us per iteration: 0.082129
%% Ask dummy registry for a number
%%   Start: {1248,709798,155771}
%%   End:   {1248,709800,227486}
%%   Delta: 2071715 us
%%   us per iteration: 2.071715

-module(message_performance).


%% Testing CLI and Erlang API
-export([test/0, test/1]).

%% Internal
-export([dummy_register/1]).


-define(DEFAULT_ITERATIONS, 1000000).


dummy_register(Count) ->
    receive
        {Sender, request} ->
            Sender ! {reply, Count},
            dummy_register(Count+1)
    end.


run_test_int(_, _, 0) ->
    ok;
run_test_int(RunFun, State, Iterations) ->
    NewState = RunFun(State),
    run_test_int(RunFun, NewState, Iterations-1).


to_us(Now) ->
    {Now_Ms, Now_s, Now_us} = Now,
    (Now_Ms*1000000+Now_s)*1000000+Now_us.


run_test(Name, Iterations, StartFun, RunFun) ->
    State = StartFun(),
    StartTime = now(),
    run_test_int(RunFun, State, Iterations),
    EndTime = now(),
    Delta_us = to_us(EndTime) - to_us(StartTime),
    io:format("~s~n"
	      "  Start: ~w~n"
	      "  End:   ~w~n"
	      "  Delta: ~w us~n"
	      "  us per iteration: ~w~n",
	      [Name, StartTime, EndTime, Delta_us, Delta_us/Iterations]).


test(Iterations) when is_integer(Iterations) ->
    run_test("NOP test",
	     Iterations,
	     fun() -> dummy_state end,
	     fun(State) -> State end),
    run_test("Ask dummy registry for a number",
	     Iterations,
	     fun() -> spawn(?MODULE, dummy_register, [0]) end,
	     fun(Reg) ->
		     Reg ! {self(), request},
                     receive
                         {reply, _Port} -> ok
                     end,
                     Reg
             end);
test([Atom]) when is_atom(Atom) ->
    test([atom_to_list(Atom)]);
test([String]) when is_list(String) ->
    test(list_to_integer(String));
test([]) ->
    test().


test() ->
    test(?DEFAULT_ITERATIONS).
