%%%-------------------------------------------------------------------
%% @doc module for connecting to a MySQL db through the mysql/otp api
%% @end
%%%-------------------------------------------------------------------

-module(dbc).

%% API exports
-export([start/0, handle_call/4]).

%%====================================================================
%% API functions
%%====================================================================

%% Start a connection to a MySQL database
start() -> init().

%% Send a message to the database consisting of:
%%      Action       :: What to do
%%      From         :: Who is the caller
%%      Topic        :: What is the topic
%%      Payload      :: What is the message
handle_call(Action, From, Topic, Payload) -> 
	database ! {Action, From, Topic, Payload}.
		
		
%%====================================================================
%% Internal functions
%%====================================================================

%% Connect to a mysql database.
%% Initialize an internal serverloop.
init() ->
        {ok, Pid} = mysql:start_link([{host, "localhost"}, 
                                        {user, "root"},
                                        {password, "Mammamu77"}, 
                                        {database, "gogodeals"}]),
	Db = spawn(fun () -> loop(Pid) end),
	register(database, Db),
	{ok, Pid}.


%% Listens for calls.
loop(Database) ->
	receive
		%% Insert the content of a Message into the expected 
		%% table in the database
		{insert, _From, Topic, Message} -> 
			Data = jtm:get_data(Message),
			case Topic of
		                <<"deal/gogodeals/deal/new">> -> 
		                        Stmt = "INSERT INTO deals (" 
		                                ++ jtm:get_key(Data) 
		                                ++ ") VALUES (?,?,?,?,?,?,?,?,?,?)", %% name, description, picture, longitude, latitude, duration, count, filters, client_id
		                        Values = jtm:get_values(Data),
		                        mysql:query(Database, Stmt, Values);
		                        
		                <<"deal/gogodeals/client/new">> -> 
		                        mysql:query(Database, 
		                                "INSERT INTO clients (" 
		                                ++ jtm:get_key(Data) 
		                                ++ ") VALUES (?,?,?,?,?)", %% name, email, password, longitude, latitude
		                                jtm:get_values(Data));
		                        
		                <<"deal/gogodeals/user/new">> -> 
		                        mysql:query(Database, 
		                        "INSERT INTO users (" 
		                        ++ jtm:get_key(Data) 
		                        ++ ") VALUES (?,?,?)", %% name, email, password
		                        jtm:get_values(Data))
		                        
	                end,
	                loop(Database);
	        
	        %% Select info from the database corresponding to the 
	        %% Topic and publish it.      
		{select, From, Topic, Message} -> 
			Data = jtm:get_data(Message),
		        [Id] = jtm:get_id(Message),
			case Topic of
		                <<"deal/gogodeals/user/info">> -> 
		                        {ok, ColumnNames, Rows} = 
		                                mysql:query(Database, <<"Select * From users Where email =? and password = ?">>, jtm:get_values(Data)),
					edm:publish(From, <<"deal/gogodeals/database/info">>, {Id, to_map(ColumnNames, Rows)}, 1);
		                
		                <<"deal/gogodeals/client/info">> -> 
		                        {ok, ColumnNames, Rows} = 
			                	mysql:query(Database, "Select * From clients Where email =? and password = ?", jtm:get_values(Data)),
					edm:publish(From, <<"deal/gogodeals/database/info">>, {Id, to_map(ColumnNames, Rows)}, 1);
		                
		                <<"deal/gogodeals/deal/info">> -> %% From Website
					{ok, ColumnNames, Rows} = mysql:query(Database, "Select * From deals Where client_id = ?", [Id]),
					edm:publish(From, <<"deal/gogodeals/database/info">>, {Id, to_deal_map(1, ColumnNames, Rows)}, 1);

		                <<"deal/gogodeals/deal/fetch">> -> %% From Application 		                                
					LongMin = [ V - 0.2 || {<<"longitude">>, V} <- Data],
					LongMax = [ V + 0.2 || {<<"longitude">>, V} <- Data],
					LatMin = [V - 0.2 || {<<"latitude">>, V} <- Data],
					LatMax = [V + 0.2 || {<<"latitude">>, V} <- Data],
					Filters = [binary_to_list(V) || {<<"filters">>,V} <- Data],
					Deals = [ binary_to_list(V) || {<<"deals">>,V} <- Data],
					{ok, ColumnNames, Rows} = mysql:query(Database, "Select * From deals Where longitude between ? and ? and 
									latitude between ? and ? and filters in (?) and id not in (?)", LongMin ++ LongMax ++ LatMin ++ LatMax ++ Filters ++ Deals),
                			edm:publish(From, <<"deal/gogodeals/database/deals">>, {Id, to_deal_map(1, ColumnNames, Rows)}, 1)
	                end,
	                loop(Database);
			
		%% Update the content of a Message into the expected table in the database
		{update, From, Topic, Message} -> 
		        Data = jtm:get_data(Message),
			case Topic of
		                <<"deal/gogodeals/deal/edit">> -> 
		                        mysql:query(Database, "UPDATE deals SET name = ?, price = ?, picture = ?, 
		                        	description = ?, duration = ?, count = ? WHERE id = ?", 
						jtm:stupid_sort([<<"name">>, <<"price">>, <<"picture">>, <<"description">>, 
							<<"duration">>, <<"count">>],Data) ++ jtm:get_id(Message));
		
		                <<"deal/gogodeals/client/edit">> -> 
		                        mysql:query(Database, "UPDATE clients SET name = ?, longitude = ?, latitude = ?, email = ?, 
			                        password = ? WHERE id = ?", 
						jtm:stupid_sort(["name","longitude", "latitude","email","password"], Data));
		
		                <<"deal/gogodeals/deal/save">> -> 
		                        mysql:query(Database, "UPDATE users SET deals = ? WHERE id = ?", jtm:stupid_sort([<<"deals">>,<<"id">>], Data)),
					io:format("Step: ~p~n", ["0"]),
					mysql:query(Database, "UPDATE deals SET count = count - 1 WHERE id = ?", jtm:get_id(Message)),
					io:format("Step: ~p~n", ["1"]),
					{ok, ColumnNames, Rows} = mysql:query(Database, "Select count From deals Where id = ?", jtm:get_id(Message)),
					io:format("Step: ~p~n", Rows),
                			[Id] = jtm:get_id(Message), 
					edm:publish(From, <<"deal/gogodeals/database/info">>, {Id, to_map(ColumnNames, Rows)}, 1);
		
		                <<"deal/gogodeals/deal/remove">> -> %% From Application
		                        mysql:query(Database, "UPDATE users SET deals = ? WHERE id = ?", jtm:stupid_sort(["deals","id"], Data)),
					mysql:query(Database, "UPDATE deals SET count = count + 1 WHERE id = ?", jtm:get_id(Message));

		                <<"deal/gogodeals/user/filter">> -> 
		                        mysql:query(Database, "UPDATE users SET filters = ? WHERE id = ?", jtm:stupid_sort(["filters","id"],Data));

		                <<"deal/gogodeals/deal/verify">> -> todo
	                end,
			%%From ! {ok, updated},
			loop(Database);
		
		%% Delete data in the database according to the content of the Message
		{delete, From, Topic, Message} -> 
			case Topic of
		                <<"deal/gogodeals/client/delete">> -> 
		                        mysql:query(Database, "DELETE FROM clients WHERE id = ?", jtm:get_id(Message));
		
		                <<"deal/gogodeals/deal/delete">> -> 
		                        mysql:query(Database, "DELETE FROM deals WHERE id = ?", jtm:get_id(Message))
	                end,
			loop(Database)
	end.


%% Convert a list of ColumnNames and a list of Rows into a map
to_deal_map(_Counter, _ColumnNames, []) -> #{};
to_deal_map(Counter, ColumnNames, [R|[]]) -> [{Counter, to_map(ColumnNames, [R])}];
to_deal_map(Counter, ColumnNames, [R|Rs]) -> maps:from_list([{Counter, to_map(ColumnNames, [R])}] ++ to_deal_map(Counter+1, ColumnNames, Rs)).

%to_map(_ColumnNames, []) -> #{};
to_map(ColumnNames, [Rows]) -> 
	io:format("Step: ~p~n", ["3"]),	
	maps:from_list(lists:zip(ColumnNames, Rows)).


