-module(key).
-compile(export_all).

generate()->
	random:uniform(1000).

between(Key,From,To)->
	if (From<To) ->
		   if 
			(Key>From) and (Key<To) ->
				true;
			true ->
				if (Key==To)->
				   true;
			   	true ->
				   false
				end
			end;
	true ->
		   if 
			((Key>From) and (Key>=To)) or ((Key<From) and (Key=<To)) ->
				true;
			true ->
				if (Key==To)->
				   true;
			   	true ->
				   false
				end
			end
	end.