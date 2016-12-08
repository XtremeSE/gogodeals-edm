-module(handler_mqtt).
-export([start_link/0]).

start_link() -> 
	{ok, Client} = emqttc:start_link([{host, "176.10.136.208"},{client_id, <<"bob">>}, {keepalive, 0}, {proto_ver, 31}]),	
	emqttc:subscribe(Client, [	%% Client/Customer
		{<<"deal/gogodeals/deal/info">>, 1},
		{<<"deal/gogodeals/deal/new">>, 1},
		{<<"deal/gogodeals/deal/edit">>, 1},
		{<<"deal/gogodeals/deal/delete">>, 1},
		{<<"deal/gogodeals/client/info">>, 1},
		{<<"deal/gogodeals/client/new">>, 1},
		{<<"deal/gogodeals/client/edit">>, 1},
		{<<"deal/gogodeals/client/delete">>, 1},
					
		%% User
		{<<"deal/gogodeals/deal/fetch">>, 1},
		{<<"deals/gogodeals/deal/grocode">>, 1},
		{<<"deal/gogodeals/user/check">>, 1},
		{<<"deal/gogodeals/deal/save">>, 1},
		{<<"deal/gogodeals/deal/remove">>, 1},
		{<<"deal/gogodeals/deal/verify">>, 1},
		{<<"deal/gogodeals/user/info">>, 1},
		{<<"deal/gogodeals/user/new">>, 1},
		{<<"deal/gogodeals/user/facebook">>,1},
		{<<"deal/gogodeals/user/update">>,1},
		{<<"deal/gogodeals/user/filter">>, 1}]),
	loop(Client),
	register(client, Client).


loop(Client) ->
	receive
		%% Receive messages from subscribed topics
      {publish, Topic, Payload} ->
			handler_db:start_link(jtm:get_action(Topic), {Client, Topic, Payload}),
			loop(Client)               
	end.

