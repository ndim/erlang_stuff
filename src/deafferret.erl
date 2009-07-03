-module(deafferret).

-export([start/0]).
-export([mutate/1, mutate/2]).


%% BUG: If the OrigSequence contains elements not in Dict -> wrong result.
%% FIXME: We are using ++ in two places, which mostly is not a good idea.
%% NOTE: We return the results in a very strange kind of "ordering".
%% NOTE: A simple test case of "AAAA" lets you miss permutated characters.


%% Verbose version. Comment out the io:format stuff for a mute version.
mutate([], _Dict, _Prefix, Acc) ->
    lists:append(Acc);
mutate([Base|Suffix]=_OrigSequence, Dict, Prefix, Acc) ->
    io:format("mutate(~p,~p,~p,~p)~n", [_OrigSequence,Dict,Prefix,Acc]),
    NewMutations = [Prefix++[X|Suffix] || X<-Dict, X=/=Base],
    io:format("  new: ~p~n", [NewMutations]),
    mutate(Suffix, Dict, Prefix++[Base], [NewMutations|Acc]).


mutate(OrigSequence, Dict) ->
    mutate(OrigSequence, Dict, "", []).

mutate(OrigSequence) ->
    mutate(OrigSequence, "ACGT").


%% Run example.
example(OrigSequence) ->
    Dict = "ACGT",
    Mutations = mutate(OrigSequence, Dict),
    io:format("Mutations of orig sequence ~p for dictionary ~p:~n"
	      "  ~p~n",
	      [OrigSequence, Dict,
	       Mutations]).

start() ->
    [ example(X) || X <- ["CATTAG", "AAAA"] ].
