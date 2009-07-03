-module(deafferret).

-export([start/0]).
-export([mutate/1, mutate/2]).
-export([m/1, m/2]).


%% Mute version
m([], _Dict, _Prefix, Acc) ->
    lists:reverse(Acc);
m([Cur|Suffix]=G, Dict, Prefix, Acc) ->
    m(Suffix, Dict, [Cur|Prefix],
      lists:foldl(fun(El,Ac) -> [El|Ac] end, Acc,
		  [Prefix++[X|Suffix] || X<-Dict, X=/=Cur])).

m(Given, Dict) ->
    m(Given, Dict, "", []).

m(Given) ->
    m(Given, "ACGT").


%% Verbose version
mutate([], _Dict, _Prefix, Acc) ->
    lists:reverse(Acc);
mutate([Cur|Suffix]=G, Dict, Prefix, Acc) ->
    io:format("mutate(~p,~p,~p,~p)~n", [G,Dict,Prefix,Acc]),
    NewMutations = [Prefix++[X|Suffix] || X<-Dict, X=/=Cur],
    io:format("  new: ~p~n", [NewMutations]),
    mutate(Suffix, Dict, [Cur|Prefix],
	   lists:foldl(fun(El,Ac) -> [El|Ac] end, Acc, NewMutations)).

mutate(Given, Dict) ->
    mutate(Given, Dict, "", []).

mutate(Given) ->
    mutate(Given, "ACGT").


%% Run example.
start() ->
    Dict = "ACGT",
    Given = "GCAT",
    Mutations = mutate(Given, Dict),
    io:format("Mutations of ~p for base ~p:~n  ~p~n", [Given, Dict, Mutations]).
