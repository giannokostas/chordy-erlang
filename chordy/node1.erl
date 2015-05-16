-module(node1).

-export([start/1,start/2]).

-define(Stabilize,500).
-define(Timeout,1000000).

start(Id) ->
	%io:format("The first node of the circle~n" ),
	start(Id,nil).

start(Id,Peer) ->
	timer:start(),
	%io:format("Insert new node to the circle!~n" ),
	spawn(fun() ->init(Id,Peer) end).

init(Id,Peer) -> 
	Predecessor = nil,
	{ok,Successor} = connect(Id,Peer),
	%io:format("successor of ~w is: ~w~n", [Id,Successor]),
	schedule_stabilize(),
	node(Id,Predecessor,Successor).


connect(Id, nil) ->
	%io:format("myself is a successor!~n"),
	{ok, {Id,self()}};
connect(_, Peer) ->
	Qref = make_ref(),
	Peer ! {key, Qref, self()},
	receive
		{Qref, Skey} ->
			{ok,{Skey,Peer}}
	after 
		?Timeout ->
		io:format("Time out: no response~n")
	end.

node(Id, Predecessor, Successor) ->
	receive
		{key, Qref, Peer} ->
			Peer ! {Qref, Id},
			%io:format("New node  ~w asks our key: ~w~n", [Peer,Id]),
			node(Id, Predecessor, Successor);
		{notify, New} ->
			Pred = notify(New, Id, Predecessor),
			%io:format("Predecessor of ~w is: ~w~n", [Id,Pred]),
			node(Id, Pred, Successor);
		{request, Peer} ->
			request(Peer, Predecessor),
			node(Id, Predecessor, Successor);
		{status, Pred} ->
			Succ = stabilize(Pred, Id, Successor),
			%io:format("Successor of ~w is: ~w~n", [Id,Succ]),
			node(Id, Predecessor, Succ);
		stabilize ->
			stabilize(Successor),
			node(Id, Predecessor, Successor);
		status ->
			io:format("Node ~w - Successor : ~w - Predecessor ~w~n", [Id,Successor,Predecessor]),
			node(Id, Predecessor, Successor);
		probe ->
			create_probe(Id, Successor),
			node(Id, Predecessor, Successor);
		{probe, Id, Nodes, T} ->
			remove_probe(T, Nodes),
			node(Id, Predecessor, Successor);
		{probe, Ref, Nodes, T} ->
			forward_probe(Ref, T, Nodes, Id, Successor),
			node(Id, Predecessor, Successor)
	end.
request(Peer, Predecessor) ->
	case Predecessor of
		nil ->
			Peer ! {status, nil};
		{Pkey, Ppid} ->
			Peer ! {status, {Pkey, Ppid}}
	end.


schedule_stabilize() ->
	timer:send_interval(?Stabilize, self(), stabilize).



stabilize({_,Spid}) ->
	Spid ! {request,self()}.

stabilize(Pred, Id, Successor) ->
	
	{Skey, Spid} = Successor,
	case Pred of
		nil ->
			Spid ! {notify, {Id,self()}},
			Successor;
		{Id, _} ->
			Successor;
		{Skey, _} ->
			Spid ! {notify, {Id,self()}},
			Successor;
		{Xkey, Xpid} ->
			case key:between(Xkey, Id, Skey) of
				true ->
					stabilize(Pred,Id,{Xkey,Xpid});
				false ->
					Spid ! {notify, {Id,self()}},
					Successor
			end
	end.


notify({Nkey, Npid}, Id, Predecessor) ->
	case Predecessor of
		nil ->
			Npid ! {status,{Nkey,Npid}},
			{Nkey,Npid};
		{Pkey, Ppid} ->
			case key:between(Nkey, Pkey, Id) of
				true ->
					Npid ! {status,{Nkey,Npid}},
					{Nkey,Npid};
				false ->
					Npid ! {status,{Pkey, Ppid}},
					{Pkey, Ppid}
			end
	end.

%send first probe
create_probe(Id, Successor) ->
	{_,Pid} = Successor,
	Pid ! {probe, Id,[Id],erlang:now()}.
%print Time
remove_probe(T, Nodes) ->
	Diff = timer:now_diff(erlang:now(), T),
	L = lists:flatlength(Nodes),
	io:format("Node:~w Removing probe after ~wmicros.~n Nodes visited: ~w~n~n", [self(), Diff, L]).

%send prob to successor
forward_probe(Ref, T, Nodes, Id, Successor)->
	{_,Pid} = Successor,
	Pid ! {probe,Ref,[Id|Nodes],T}.

