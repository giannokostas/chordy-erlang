-module(test).

-compile(export_all).

run() ->
	Node1 = node1:start(1),
	Node2 = node1:start(9,Node1),
	Node3 = node1:start(3,Node1),
	Node4 = node1:start(7,Node1),
	Node5 = node1:start(11,Node1),
	Node6 = node1:start(6,Node1),
	timer:sleep(5000),
	Node1 ! status,
	Node2 ! status,
	Node3 ! status,
	Node4 ! status,
	Node5 ! status,
	Node6 ! status.
	%Node3 ! probe.