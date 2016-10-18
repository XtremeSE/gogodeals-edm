-module(edm).

%% API exports
-export([start/0]).

%%====================================================================
%% API functions
%%====================================================================

start() -> spawn(fun () -> init() end).

%%====================================================================
%% Internal functions
%%====================================================================

init() -> 
	{ok, Client} = emqttc:start_link([{host, "176.10.136.208"}, {client_id, <<"simpleClient">>}, {proto_ver, 3}]),
	emqttc:subscribe(Client, [{<<"web/deal/new">>, 1},
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

	{ok, Database} = mysql:start_link([{host, "localhost"}, {user, "user"}, {password, "password"}, {database, "test"}]),
        
	Broker = spawn(fun () -> broker_loop(Client) end),
	Database = spawn(fun () -> database_loop(Database) end),
        
	register(broker,Broker),
	register(database, Database).

	

broker_loop(Client) ->
	receive
 	
	%% Receive messages from subscribed topics
        {{publish, Topic, Payload}} ->
                database ! {Topic, Payload},
		io:format("Message from ~s: ~p~n", [Topic, Payload]),
                broker_loop(Client);

        %% Publish messages with a topic and Qos to the broker
        {publish, Topic, Message, Qos} ->
                Payload = list_to_binary([Message]),
                emqttc:publish(Client, Topic, Payload, [{qos, Qos}]),
                broker_loop(Client);

        %% Subscribe to a topic on the broker
        {subscribe, Topic} ->
                emqttc:subscribe(Client, Topic, qos0),
                broker_loop(Client);

        %% Stop the loop as a part of stopping the client
        stop ->
                ok
	
	end.

database_loop(Database) ->
	receive
		{"web/deal/new", Payload} -> 
			mysql:query(Database, "INSERT INTO deals (
				client_id, name, price, picture, description, 
				duration, count) VALUES (?,?,?,?,?,?)", [Message]),
			broker ! {publish, <<"database/deal/location">>, NewMessage},
			database_loop(Database);

		{"web/deal/edit", Payload} -> 
			mysql:query(Database, "UPDATE deals SET name = ?, price = ?, 
				picture = ?, description = ?, duration = ?, count = ? 
				WHERE client_id = ?", 
				[Message]),
			broker ! {publish, <<"database/deal/location">>, NewMessage},
			database_loop(Database);

		{"web/deal/delete", Payload} -> 
			mysql:query(Database, "DELETE FROM deals WHERE client_id = ?", 
				[Message]),
			broker ! {publish, <<"database/deal/location">>, NewMessage},
			database_loop(Database);

		{"web/user/new", Payload} -> 
			mysql:query(Database, "INSERT INTO clients (name, location, 
				email, password) VALUES (?,?,?,?)", [Message]),
			database_loop(Database);

		{"web/user/edit", Payload} -> 
			mysql:query(Database, "UPDATE clients SET  ", [Message]),
			database_loop(Database);

		{"web/user/delete", Payload} -> 
			mysql:query(Database, "DELETE FROM clients WHERE id = ?", [Message]),
			database_loop(Database);

		{"app/deal/save", Payload} -> 
			mysql:query(Database, "UPDATE users SET deals = ?  WHERE id = ? ", 
				[Message]),
			database_loop(Database);

		{"app/deal/delete", Payload} -> 
			mysql:query(Database, "UPDATE users SET deals = ?  WHERE id = ? ", 
				[Message]),
			database_loop(Database);

		{"app/deal/verify", Payload} -> 
			verified,
			database_loop(Database);

		{"app/user/new", Payload} -> 
			mysql:query(Database, "INSERT INTO user (name, email, info, 
				deals, filters) VALUES (?,?,?,?,?)", [Message]),
			database_loop(Database);

		{"app/user/filter", Payload} -> 
			mysql:query(Database, "UPDATE users SET filters = ?  WHERE id = ? ", 
				[Message]),
			database_loop(Database);

		{user, Id} -> 
			UserInfo = mysql:query(Database, "SELECT * FROM users WHERE id = ?", [Id]),
			broker ! {publish, <<"database/info/user">>, UserInfo};
		
		{client, Id} -> 
			ClientInfo = mysql:query(Database, "SELECT * FROM clients WHERE id = ?", [Id]),
			broker ! {publish, <<"database/info/client">>, ClientInfo};
		
		stop -> exit(database, normal)
	end.
