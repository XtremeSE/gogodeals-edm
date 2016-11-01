-module(dbc).

%% API exports
-export([start/0, handle_call/4, test/0]).

%%====================================================================
%% API functions
%%====================================================================

%% Connect to a mysql database.
%% Initialize an internal serverloop.
start() ->
        {ok, Pid} = mysql:start_link([{host, "localhost"}, {user, "root"},
                              {password, "password"}, {database, "gogodeals"}]),
	Db = spawn(fun () -> loop(Pid) end),
	register(database, Db).

handle_call(Action, From, Topic, Payload) -> 
	Message = jtm:get_data(Payload),
	database ! {Action, self(), Topic, Message},
	receive
	        M -> From ! M
	end.
	
test() ->
        start(),
        Topic = <<"web/deal/new">>,
        Message = <<"{\"id\": \"1\", \"payload_encryption\": false, \"data\": {\"client_id\": 2, \"name\": \"Coffee\", \"price\": 15, \"picture\": null, \"description\": \"This is a mean thing\", \"duration\": 123456, \"count\": 15},}">>,
        handle_call(insert, self(), Topic, Message),
        receive
                M -> M
        end.
	

%%====================================================================
%% Internal functions
%%====================================================================

%% Listens for calls.
loop(Database) ->
	receive
		%% Insert the content of a Message into the expected table in the database
		{insert, From, Topic, Message} -> 
			case Topic of
		                <<"web/deal/new">> -> 
		                        Stmt = "INSERT INTO deals (" ++ jtm:get_key(Message) ++ ") VALUES (?,?,?,?,?,?,?)",
		                        Values = jtm:get_values(Message),
		                        io:format("Statement: ~s~n", [Stmt]),
		                        io:format("Values: ~p~n", [Values]),
		                        W = mysql:query(Database, Stmt, Values);
		                <<"web/user/new">> -> 
		                        W = mysql:query(Database, "INSERT INTO clients (" ++ jtm:get_key(Message) ++ ") VALUES (?,?,?,?,?)", [jtm:get_values(Message)]);
		                <<"app/user/new">> -> 
		                        W = mysql:query(Database, "INSERT INTO user (" ++ jtm:get_key(Message) ++ ") VALUES (?,?,?)", [jtm:get_values(Message)])
	                end,
	                From ! W;
	        
	        %% Select info from the database corresponding to the Topic and publish it.      
		{select, _From, Topic, Message} -> 
			case Topic of
		                <<"app/user/info">> -> 
		                        {ok, ColumnNames, Rows} = 
		                                mysql:query(Database, "Select * From users Where id = ?", [jtm:get_values(Message)]),
                                        edm:publish(Database, <<"database/user/info">>, to_map(ColumnNames, Rows), 1);
		                
		                <<"web/user/info">> -> 
		                        {ok, ColumnNames, Rows} = 
		                                mysql:query(Database, "Select * From clients Where id = ?", [jtm:get_values(Message)]),
		                        edm:publish(Database, <<"database/client/info">>, to_map(ColumnNames, Rows), 1);
		                
		                <<"web/deal/info">> -> 
		                        {ok, ColumnNames, Rows} = 
		                                mysql:query(Database, "Select * From deals Where client_id = ?", [jtm:get_values(Message)]),
		                        edm:publish(Database, <<"database/deal/info">>, to_map(ColumnNames, Rows), 1);
		                        
		                <<"app/deal/info">> -> 
		                        {ok, ColumnNames, Rows} = 
		                                mysql:query(Database, "Select * From deals Where location = ?", [jtm:get_values(Message)]),
                			edm:publish(Database, <<"database/deal/info">>, to_map(ColumnNames, Rows))
	                end;
			
		%% Update the content of a Message into the expected table in the database
		{update, From, Topic, Message} -> 
			case Topic of
		                <<"web/deal/edit">> -> 
		                        mysql:query(Database, "UPDATE deals SET name = ?, price = ?, picture = ?, 
		                        description = ?, duration = ?, count = ? WHERE client_id = ?, id = ?", [jtm:stupid_sort(["name","price","picture","description","duration","count","client_id","id"],Message)]);
		
		                <<"web/user/edit">> -> 
		                        mysql:query(Database, "UPDATE clients SET name = ?, location = ?, email = ?, 
		                        password = ? WHERE id = ?", [jtm:stupid_sort(["name","location","email","password"], Message)]);
		
		                <<"app/deal/save">> -> 
		                        mysql:query(Database, "UPDATE users SET deals = ? WHERE id = ?", [jtm:stupid_sort(["deals","id"], Message)]);
		
		                <<"app/deal/delete">> -> 
		                        mysql:query(Database, "UPDATE users SET deals = ? WHERE id = ?", [jtm:stupid_sort(["deals","id"], Message)]);
				
		                <<"app/user/filter">> -> 
		                        mysql:query(Database, "UPDATE users SET filters = ? WHERE id = ?", [jtm:stupid_sort(["filters","id"],Message)]);

		                <<"app/deal/verify">> -> todo
	                end,
			From ! {ok, updated};
		
		%% Delete data in the database according to the content of the Message
		{delete, From, Topic, Message} -> 
			case Topic of
		                <<"web/deal/delete">> -> 
		                        mysql:query(Database, "DELETE FROM clients WHERE id = ?", [jtm:get_values(Message)]);
		
		                <<"web/user/delete">> -> 
		                        mysql:query(Database, "DELETE FROM deals WHERE client_id = ?", [jtm:get_values(Message)])
	                end,
			From ! {ok, deleted}
	end.


%% Convert a list of ColumnNames and a list of Rows into a map
to_map(ColumnNames, Rows) -> maps:from_list([ {C, R} || C <- ColumnNames, R <- Rows]).
