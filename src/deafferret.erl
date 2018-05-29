%% deafferret.erl - mutate a sequence of bases in a single place
%% Written with and for deafferret on freenode/#erlang.

-module(deafferret).

-export([start/0]).
-export([mutate/2, mutate/3]).

-define(DEFAULT_ALPHABET, "ACGT").


%% BUG: If the OrigSequence contains elements not in Alphabet -> wrong result.

%% FIXME: We are using ++ in two places, which mostly is not a good idea.

%% NOTE: We return the results in a very strange kind of "ordering".

%% NOTE: A simple test case of "AAAA" lets you miss permutated characters.


%% Author's comments:
%%
%% I like the result list construction in mutate() now: Create a list
%% of lists of strings and then flattening that to a list of strings
%% at the end.
%%
%% I do not like the Prefix++[Base] expression yet, at all. I have no
%% idea what do do about it (yet), though.
%%
%% I have some doubts about the Prefix++[X|Suffix] expression as
%% well. I have no idea what do do about it (yet), though.


%% Verbose version. Comment out the io:format stuff for a mute version.
mutate(1, [], _Alphabet, _Prefix, Acc) ->
    lists:append(Acc);
mutate(1, [Base|Suffix]=_OrigSequence, Alphabet, Prefix, Acc) ->
    io:format("mutate(~w,~p,~p,~p,~p)~n", [1,_OrigSequence,Alphabet,Prefix,Acc]),
    NewMutations = [Prefix++[X|Suffix] || X<-Alphabet, X=/=Base],
    io:format("  new: ~p~n", [NewMutations]),
    mutate(1, Suffix, Alphabet, Prefix++[Base], [NewMutations|Acc]);

mutate(2, [], _Alphabet, _Prefix, Acc) ->
    lists:append(Acc);
mutate(2, [Base|Suffix]=_OrigSequence, Alphabet, Prefix, Acc) ->
    io:format("mutate(~w,~p,~p,~p,~p)~n", [2,_OrigSequence,Alphabet,Prefix,Acc]),
    RM = mutate(1, Suffix, Alphabet, [], []),
    io:format("  rec: ~p~n", [RM]),
    NewMutations =
	lists:append([[Prefix++[X|S] || X<-Alphabet, X=/=Base] || S <- RM]),
    io:format("  new: ~p~n", [NewMutations]),
    mutate(2, Suffix, Alphabet, Prefix++[Base], [NewMutations|Acc]).


mutate(N, OrigSequence, Alphabet) ->
    mutate(N, OrigSequence, Alphabet, [], []).


mutate(N, OrigSequence) ->
    mutate(N, OrigSequence, ?DEFAULT_ALPHABET).


%% Run example.
example(N, OrigSequence) ->
    Mutations = mutate(N, OrigSequence),
    io:format("Mutations of orig sequence ~p in ~w places for default alphabet ~p:~n"
	      "  ~p~n~n",
	      [OrigSequence, N, ?DEFAULT_ALPHABET,
	       Mutations]).


%% Run a few examples.
start() ->
    [ example(N, X) || N <- [1], X <- ["CATTAG", "AAAA", "TCG", "AC", "T"] ].
