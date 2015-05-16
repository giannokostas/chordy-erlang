-module(test2).

-compile(export_all).

run() ->
	Node1 = node2:start(1),
	Node2 = node2:start(9,Node1),
	Node3 = node2:start(3,Node1),
	Node4 = node2:start(7,Node1),
	Node5 = node2:start(11,Node1),
	Node6 = node2:start(6,Node1),
	timer:sleep(5000),
	Node1 ! status,
	Node2 ! status,
	Node3 ! status,
	Node4 ! status,
	Node5 ! status,
	Node6 ! status.
	%Node3 ! probe.
