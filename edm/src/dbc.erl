%%%-------------------------------------------------------------------
%% @doc module for connecting to a MySQL db through the mysql/otp api
%% @end
%%%-------------------------------------------------------------------

-module(dbc).

%% API exports
-export([start/0, handle_call/4, test/0]).

%%====================================================================
%% API functions
%%====================================================================

start() -> init().


handle_call(Action, From, Topic, Payload) -> 
	database ! {Action, self(), Topic, Payload}.
	
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

%% Connect to a mysql database.
%% Initialize an internal serverloop.
init() ->
        {ok, Pid} = mysql:start_link([{host, "localhost"}, {user, "root"},
                              {password, "password"}, {database, "gogodeals"}]),
	Db = spawn(fun () -> loop(Pid) end),
	register(database, Db),
	{ok, Pid}.


%% Listens for calls.
loop(Database) ->
	receive
		%% Insert the content of a Message into the expected table in the database
		{insert, From, Topic, Message} -> 
			Data = jtm:get_data(Message),
			case Topic of
		                <<"web/deal/new">> -> 
		                        Stmt = "INSERT INTO deals (" ++ jtm:get_key(Data) ++ ") VALUES (?,?,?,?,?,?,?)",
		                        Values = jtm:get_values(Data),
		                        mysql:query(Database, Stmt, Values);
		                        
		                <<"web/user/new">> -> 
		                        mysql:query(Database, "INSERT INTO clients (" ++ jtm:get_key(Data) ++ ") VALUES (?,?,?,?)", jtm:get_values(Data));
		                        
		                <<"app/user/new">> -> 
		                        mysql:query(Database, "INSERT INTO users (" ++ jtm:get_key(Data) ++ ") VALUES (?,?,?,?,?)", jtm:get_values(Data))
		                        
	                end,
	                loop(Database);
	        
	        %% Select info from the database corresponding to the Topic and publish it.      
		{select, From, Topic, Message} -> 
			Data = jtm:get_data(Message),
			case Topic of
		                <<"app/user/info">> -> 
		                        {ok, ColumnNames, Rows} = 
		                                mysql:query(Database, <<"Select * From users Where id =?">>, jtm:get_values(Data)),
                                        io:format("Selected: ~s~n", [ColumnNames]),
                                        io:format("Selected: ~p~n", Rows),
                                        edm:publish(From, <<"database/user/info">>, {jtm:get_id(Message), false, to_map(ColumnNames, Rows)}, 1);
		                
		                <<"web/user/info">> -> 
		                        {ok, ColumnNames, Rows} = 
		                                mysql:query(Database, "Select * From clients Where id = ?", [jtm:get_values(Data)]),
		                        edm:publish(Database, <<"database/client/info">>, {jtm:get_id(Message), false, to_map(ColumnNames, Rows)}, 1);
		                
		                <<"web/deal/info">> -> 
		                        {ok, ColumnNames, Rows} = 
		                                mysql:query(Database, "Select * From deals Where client_id = ?", [jtm:get_values(Data)]),
		                        edm:publish(Database, <<"database/deal/info">>, {jtm:get_id(Message), false, to_map(ColumnNames, Rows)}, 1);
		                        
		                <<"app/deal/info">> -> 
		                        {ok, ColumnNames, Rows} = 
		                                mysql:query(Database, "Select * From deals Where location = ?", [jtm:get_values(Data)]),
                			edm:publish(Database, <<"database/deal/info">>, {jtm:get_id(Message), false, to_map(ColumnNames, Rows)}, 1)
	                end,
	                loop(Database);
			
		%% Update the content of a Message into the expected table in the database
		{update, From, Topic, Message} -> 
		        Data = jtm:get_data(Message),
			case Topic of
		                <<"web/deal/edit">> -> 
		                        mysql:query(Database, "UPDATE deals SET name = ?, price = ?, picture = ?, 
		                        description = ?, duration = ?, count = ? WHERE id = ?", jtm:stupid_sort(["name","price","picture","description","duration","count"],Data) ++ jtm:get_id(Message));
		
		                <<"web/user/edit">> -> 
		                        mysql:query(Database, "UPDATE clients SET name = ?, location = ?, email = ?, 
		                        password = ? WHERE id = ?", jtm:stupid_sort(["name","location","email","password"], Message));
		
		                <<"app/deal/save">> -> 
		                        mysql:query(Database, "UPDATE users SET deals = ? WHERE id = ?", jtm:stupid_sort(["deals","id"], Message));
		
		                <<"app/deal/delete">> -> 
		                        mysql:query(Database, "UPDATE users SET deals = ? WHERE id = ?", jtm:stupid_sort(["deals","id"], Message));
				
		                <<"app/user/filter">> -> 
		                        mysql:query(Database, "UPDATE users SET filters = ? WHERE id = ?", jtm:stupid_sort(["filters","id"],Message));

		                <<"app/deal/verify">> -> todo
	                end,
			From ! {ok, updated},
			loop(Database);
		
		%% Delete data in the database according to the content of the Message
		{delete, From, Topic, Message} -> 
			Data = jtm:get_data(Message),
			case Topic of
		                <<"web/user/delete">> -> 
		                        mysql:query(Database, "DELETE FROM clients WHERE id = ?", jtm:get_values(Data));
		
		                <<"web/deal/delete">> -> 
		                        mysql:query(Database, "DELETE FROM deals WHERE client_id = ?", jtm:get_values(Data))
	                end,
			From ! {ok, deleted},
			loop(Database)
	end.


%% Convert a list of ColumnNames and a list of Rows into a map
to_map(ColumnNames, [Rows]) ->         
        maps:from_list(lists:zip(ColumnNames, Rows)).
