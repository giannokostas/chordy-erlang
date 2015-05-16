-module(tester).
-export([run/1]).

run(Node) ->
	{MegaSecs, _, _} = erlang:now(),
	spawn(fun() -> init(Node, MegaSecs) end).

init(Node, MegaSecs) ->
	random:seed(MegaSecs, MegaSecs, MegaSecs),
	routine(Node, []).

routine(Node, Keys) ->
	%io:format("Node: ~w, Keys: ~w~n", [Node, Keys]),
	receive
		{add, N} ->
			Nkeys = add(Node, N, Keys),
			routine(Node, Nkeys);
		lookup ->
			lookup(Node, Keys, erlang:now()),
			routine(Node, Keys);
		stop ->
			ok;
		X ->
			io:format("Caught some strange message: ~w~n", [X]),
			routine(Node, Keys)
	end.

add(_, 0, Keys) ->
	Keys;
add(Node, N, Keys) ->
	Qref = make_ref(),
	Key = random:uniform(10000),
	Node ! {add, Key, random:uniform(1000), Qref, self()},
	receive
		{Qref, _} ->
			ok
	end,
	add(Node, N - 1, [Key | Keys]).

lookup(_, [], T) ->
	io:format("Time to lookup all the keys: ~w~n", [timer:now_diff(erlang:now(), T)]);
lookup(Node, Keys, T) ->
	[Key | Tail] = Keys,
	Qref = make_ref(),
	Node ! {lookup, Key, Qref, self()},
	receive
		{Qref, _} ->
			ok
	end,
	lookup(Node, Tail, T).
