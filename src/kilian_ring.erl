%% Run this program like this:
%%   $ erl -noshell -run kilian_ring start -s init stop
%%   $ erl -noshell -run kilian_ring start 13 -s init stop

-module(kilian_ring).


-export([start/0, start/1]).
-export([loop/2]).


loop(none, Number) -> % receiver process
    receive
        {message, Sender, M} ->
            io:format("Process ~w/~w: Received message ~w~n",
                      [Number, self(), M]),
            Sender ! {ack, M},
            loop(none, Number);
        {stop, Sender} ->
            io:format("Process ~w/~w: stopping.~n",
                      [Number, self()]),
            Sender ! {ack, stop}
    end;
loop(Next, Number) -> % forwarder process
    receive
        {message, _Sender, M} = Msg ->
            io:format("Process ~w/~w: forwarding message to Pid ~w: ~w~n",
                      [Number, self(), Next, M]),
            Next ! Msg,
            loop(Next, Number);
        {stop, _Sender} = Msg ->
            io:format("Process ~w/~w: stopping (forwarding to Pid ~w)~n",
                      [Number, self(), Next]),
            Next ! Msg
    end.


start() ->
    start([]).


receive_acks(_Timeout, []) ->
    ok;
receive_acks(Timeout, [Head|Tail]) ->
    receive
        {ack, Head} -> receive_acks(Timeout, Tail)
        after Timeout  -> timeout
    end.


receive_acks(Messages) ->
    receive_acks(1000, Messages).


spawn_chain(Number, Next) when Number > 0 ->
    Pid = spawn(?MODULE, loop, [Next,Number]),
    spawn_chain(Number-1, Pid);
spawn_chain(0, Next) ->
    Next.


spawn_chain(Number) ->
    spawn_chain(Number, none).


start([]) ->
    start(["5"]);
start([NumStr]) ->
    %% Set up chain of processes
    First = spawn_chain(list_to_integer(NumStr)),
    io:format("First: ~p~n", [First]),

    %% Send a few messages through the chain
    Messages = ['moo', ['foo', 'bar'], "blah", {42,23}, 'that\'s it!'],
    [ First ! {message, self(), M} || M <- Messages ],
    case receive_acks(Messages) of
        ok -> io:format("All messages have crossed the process chain.~n", []);
        timeout -> io:format("Timeout waiting for all messages to cross process chain.~n", [])
    end,

    %% Send a stop message to chain
    First ! {stop, self()},
    ok = receive_acks([stop]),
    io:format("The process chain has been torn down.~n", []).
