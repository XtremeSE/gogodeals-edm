-module(edm).

%% API exports
-export([start/0]).

%%====================================================================
%% API functions
%%====================================================================

start() -> 
	spawn(fun () -> init(),
	io:format("Client is up: ~p~n", [whereis(client)]) end).

%%====================================================================
%% Internal functions
%%====================================================================

init() ->
	{ok, Client} = emqttc:start_link([{host, "localhost"},{client_id, <<"simpleClient">>}, {proto_ver, 3}]),
	emqttc:subscribe(Client, [{<<"topic">>, 0}, 
					{<<"web/deal/new">>, 1},
					{<<"web/deal/edit">>, 1},
					{<<"web/deal/delete">>, 1},
					{<<"web/user/register">>, 1},
					{<<"web/user/edit">>, 1},
					{<<"web/user/delete">>, 1},
					{<<"app/deal/save">>, 1},
					{<<"app/deal/delete">>, 1},
					{<<"app/deal/verify">>, 1},
					{<<"app/user/new">>, 1},
					{<<"app/user/filter">>, 1}]),
        broker_loop(Client),
	%io:format("Broker connection is up: ~p~n", [Broker]),
	register(client, Client).

json(Message) -> 
	Payload = lists:append("{'target_id': 1,'data': {", Message,"}}"),
	jsx:encode(Payload).

un_jason(Payload) ->
	Message = jsx:decode(Payload, [return_maps]).

broker_loop(Client) ->
	receive
 	
	%% Receive messages from subscribed topics
        {publish, Topic, Payload} ->
                io:format("Message from ~s: ~p~n", [Topic, Payload]),
		case Topic of
			<<"topic">> ->  emqttc:publish(Client, <<"database/info/update">>, Payload, [{qos,1}]);
			<<"web/deal/new">> ->  emqttc:publish(Client, <<"database/info/web">>, Payload, [{qos,1}]);
			<<"web/deal/edit">> ->  emqttc:publish(Client, <<"database/info/update">>, Payload, [{qos,1}]);
                                        <<"web/deal/delete">> ->  emqttc:publish(Client, <<"database/info/update">>, Payload, [{qos,1}]);
                                        <<"web/user/register">> ->  emqttc:publish(Client, <<"database/info/web">>, Payload, [{qos,1}]);
                                        <<"web/user/edit">> ->  emqttc:publish(Client, <<"database/info/web">>, Payload, [{qos,1}]);
                                        <<"web/user/delete">> ->  emqttc:publish(Client, <<"database/info/web">>, Payload, [{qos,1}]);
                                        <<"app/deal/save">> ->  emqttc:publish(Client, <<"database/info/app">>, Payload, [{qos,1}]);
                                        <<"app/deal/delete">> ->  emqttc:publish(Client, <<"database/info/app">>, Payload, [{qos,1}]);
                                        <<"app/deal/verify">> ->  emqttc:publish(Client, <<"database/info/app">>, Payload, [{qos,1}]);
                                        <<"app/user/new">> ->  emqttc:publish(Client, <<"database/info/app">>, Payload, [{qos,1}]);
                                        <<"app/user/filter">> ->  emqttc:publish(Client, <<"database/info/app">>, Payload, [{qos,1}]) end,
                broker_loop(Client);

        %% Publish messages with a topic and Qos to the broker
        {send, Topic, Message, Qos} ->
                io:format("Publish: ~p~n", [Message]),
		Payload = json(Message),
                emqttc:publish(Client, Topic, Payload, [{qos, Qos}]),
                broker_loop(Client);

        %% Subscribe to a topic on the broker
        % {subscribe, Topic} ->
        %        emqttc:subscribe(Client, Topic, qos0),
        %        broker_loop(Client);

        %% Stop the loop as a part of stopping the client
        stop ->
                exit(broker, normal)
	end.

%database_loop(Database) ->
%	receive
%		{"web/deal/new", Payload} -> 
%			Message = un_jason(Payload),
%			mysql:query(Database, "INSERT INTO deals (
%				client_id, name, price, picture, description, 
%				duration, count) VALUES (?,?,?,?,?,?)", [Message]),
%			NewMessage = mysql:query("SELECT * FROM deals WHERE location = ?", [Message]),
%			broker ! {publish, <<"database/deal/location">>, NewMessage, 1},
%			database_loop(Database);
%
%		{"web/deal/edit", Payload} ->
%			Message = un_jason(Payload), 
%			mysql:query(Database, "UPDATE deals SET name = ?, price = ?, 
%				picture = ?, description = ?, duration = ?, count = ? 
%				WHERE client_id = ?", 
%				[Message]),
%			NewMessage = mysql:query("SELECT * FROM deals WHERE location = ?", [Message]),
%			broker ! {publish, <<"database/deal/location">>, NewMessage, 1},
%			database_loop(Database);
%
%		{"web/deal/delete", Payload} -> 
%			Message = un_jason(Payload),
%			mysql:query(Database, "DELETE FROM deals WHERE client_id = ?", 
%				[Message]),
%			NewMessage = mysql:query("SELECT * FROM deals WHERE location = ?", [Message]),
%			broker ! {publish, <<"database/deal/location">>, NewMessage, 1},
%			database_loop(Database);
%
%		{"web/user/new", Payload} -> 
%			Message = un_jason(Payload),
%			mysql:query(Database, "INSERT INTO clients (name, location, 
%				email, password) VALUES (?,?,?,?)", [Message]),
%			database_loop(Database);
%
%		{"web/user/edit", Payload} -> 
%			Message = un_jason(Payload),
%			mysql:query(Database, "UPDATE clients SET  ", [Message]),
%			database_loop(Database);
%
%		{"web/user/delete", Payload} -> 
%			Message = un_jason(Payload),
%			mysql:query(Database, "DELETE FROM clients WHERE id = ?", [Message]),
%			database_loop(Database);
%
%		{"app/deal/save", Payload} -> 
%			Message = un_jason(Payload),
%			mysql:query(Database, "UPDATE users SET deals = ?  WHERE id = ? ", 
%				[Message]),
%			database_loop(Database);
%
%		{"app/deal/delete", Payload} -> 
%			Message = un_jason(Payload),
%			mysql:query(Database, "UPDATE users SET deals = ?  WHERE id = ? ", 
%				[Message]),
%			database_loop(Database);
%
%		{"app/deal/verify", Payload} -> 
%			verified,
%			database_loop(Database);
%
%		{"app/user/new", Payload} -> 
%			Message = un_jason(Payload),
%			mysql:query(Database, "INSERT INTO user (name, email, info, 
%				deals, filters) VALUES (?,?,?,?,?)", [Message]),
%			database_loop(Database);
%
%		{"app/user/filter", Payload} -> 
%			Message = un_jason(Payload),
%			mysql:query(Database, "UPDATE users SET filters = ?  WHERE id = ? ", 
%				[Message]),
%			database_loop(Database);
%
%		{insert, Topic, Payload} ->
%			io:format("Hello Database ~p~n", [Payload]),
%			mysql:query(Database, "INSERT INTO test (id, name) VALUES (?,?)", [0, Payload]),
%			Message = mysql:query(Database, "SELECT * FROM table WHERE name = ?", [Payload]),
%			client ! {send, <<"database/info/update">>, Message, 1},
%			database_loop(Database);
%		
%		{user, Id} -> 
%			UserInfo = mysql:query(Database, "SELECT * FROM users WHERE id = ?", [Id]),
%			client ! {publish, <<"database/info/user">>, UserInfo, 1};
%		
%		{client, Id} -> 
%			ClientInfo = mysql:query(Database, "SELECT * FROM clients WHERE id = ?", [Id]),
%			client ! {publish, <<"database/info/client">>, ClientInfo, 1};
%		
%		stop -> 
%			%broker ! stop,
%			exit(database, normal)
%	end.
