-module(edm).

%% API exports
-export([start/0]).

%%====================================================================
%% API functions
%%====================================================================

%start() -> spawn(fun () -> init() end).

%%====================================================================
%% Internal functions
%%====================================================================

start() ->
	{ok, Client} = emqttc:start_link([{host, "176.10.136.208"},{client_id, <<"simpleClient">>}, {proto_ver, 3}]),
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
        Broker = spawn(fun () -> broker_loop(Client) end),
	register(broker,Broker),

	{ok, Data} = mysql:start_link([{host, "localhost"}, {user, "root"}, {password, "password"}, {database, "gogodeals"}]),
        Database = spawn(fun () -> database_loop(Data) end),
        register(database, Database).

json(Message) -> 
	Payload = lists:append("{'target_id': 1,'data': {", Message,"}}"),
	jsx:encode(Payload).

un_jason(Payload) ->
	Message = jsx:decode(Payload, [return_maps]).
	

broker_loop(Client) ->
	receive
 	
	%% Receive messages from subscribed topics
        {{publish, Topic, Payload}} ->
                database ! {insert, Topic, Payload},
		io:format("Message from ~s: ~p~n", [Topic, Payload]),
                broker_loop(Client);

        %% Publish messages with a topic and Qos to the broker
        {publish, Topic, Message, Qos} ->
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

database_loop(Database) ->
	receive
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

		{insert, Topic, Payload} ->
			mysql:query(Database, "INSERT INTO test (id, name) VALUES (?,?)", [Payload, "test"]),
			Message = mysql:query(Database, "SELECT * FROM table WHERE id = ?, name = ?", [Payload]),
			broker ! {publish, <<"database/info/update">>, Message, 1},
			database_loop(Database);
		
		{user, Id} -> 
			UserInfo = mysql:query(Database, "SELECT * FROM users WHERE id = ?", [Id]),
			broker ! {publish, <<"database/info/user">>, UserInfo, 1};
		
		{client, Id} -> 
			ClientInfo = mysql:query(Database, "SELECT * FROM clients WHERE id = ?", [Id]),
			broker ! {publish, <<"database/info/client">>, ClientInfo, 1};
		
		stop -> 
			%broker ! stop,
			exit(database, normal)
	end.
