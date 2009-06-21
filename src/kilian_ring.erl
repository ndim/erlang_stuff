%% Run this program like this:
%%   $ erl -noshell -run kilian_ring start -s init stop
%%   $ erl -noshell -run kilian_ring start 13 -s init stop

-module(kilian_ring).


-export([start/0, start/1]).
-export([loop/2]).


loop(none, Number) -> % receiver process
    receive
        {message, M} ->
            io:format("Process ~w/~w: Received message ~w~n",
                      [Number, self(), M]),
            loop(none, Number);
        stop ->
            io:format("Process ~w/~w: stopping.~n",
                      [Number, self()])
    end;
loop(Next, Number) -> % forwarder process
    receive
        {message, M} = Msg ->
            io:format("Process ~w/~w: forwarding message to Pid ~w: ~w~n",
                      [Number, self(), Next, M]),
            Next ! Msg,
            loop(Next, Number);
        stop = Msg ->
            io:format("Process ~w/~w: stopping (forwarding to Pid ~w)~n",
                      [Number, self(), Next]),
            Next ! Msg
    end.


spawn_stuff(Number, Next, Acc) when Number > 0 ->
    Pid = spawn(?MODULE, loop, [Next,Number]),
    spawn_stuff(Number-1, Pid, [{Number,Pid}|Acc]);
spawn_stuff(0, Next, Acc) ->
    {Next, lists:reverse(Acc)}.


spawn_stuff(Number) ->
    spawn_stuff(Number, none, []).


start() ->
    start([]).


start([]) ->
    start(["5"]);
start([NumStr]) ->
    %% Set up chain of processes
    {First, Processes} = spawn_stuff(list_to_integer(NumStr)),
    io:format("First: ~p~n", [First]),
    io:format("Procs: ~p~n", [Processes]),

    %% Send a few messages through the chain
    Messages = ['moo', ['foo', 'bar'], "blah", {42,23}, 'that\'s it!'],
    [ First ! {message, M} || M <- Messages ],
    receive % BUG: Race condition.
        after 2000 -> ok
    end,

    %% Send a stop message to chain
    First ! stop,
    receive % BUG: Race condition.
        after 2000 -> ok
    end.
