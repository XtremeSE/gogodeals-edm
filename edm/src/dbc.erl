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
        {ok, Pid} = mysql:start_link([{host, "129.16.155.11"}, 
                                        {user, "root"},
                                        {password, "password"}, 
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
		                                ++ ") VALUES (?,?,?,?,?,?,?,?)", %% name, description, picture, location, duration, count, filters, client_id
		                        Values = jtm:get_values(Data),
		                        mysql:query(Database, Stmt, Values);
		                        
		                <<"deal/gogodeals/client/new">> -> 
		                        mysql:query(Database, 
		                                "INSERT INTO clients (" 
		                                ++ jtm:get_key(Data) 
		                                ++ ") VALUES (?,?,?,?)", %% name, email, password, location
		                                jtm:get_values(Data));
		                        
		                <<"deal/gogodeal/user/new">> -> 
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
			case Topic of
		                <<"deal/gogodeals/user/info">> -> 
		                        {ok, ColumnNames, Rows} = 
		                                mysql:query(Database, 
		                                        <<"Select * From users Where id =?">>, 
		                                        jtm:get_id(Message)),
                                                edm:publish(From, <<"deal/gogodeals/database/info">>, 
                                                        {jtm:get_id(Message), false, 
                                                        to_map(ColumnNames, Rows)}, 1);
		                
		                <<"deal/gogodeals/client/info">> -> 
		                        {ok, ColumnNames, Rows} = 
		                                mysql:query(Database, 
		                                        "Select * From clients Where id = ?", 
		                                        jtm:get_id(Message)),
		                        edm:publish(Database, <<"deal/gogodeals/database/info">>, 
		                                        {jtm:get_id(Message), false, to_map(ColumnNames, Rows)}, 1);
		                
		                <<"deal/gogodeals/deal/info">> -> %% From Website
		                        {ok, ColumnNames, Rows} = 
		                                mysql:query(Database, 
		                                        "Select * From deals Where client_id = ?", jtm:get_values(Data)),
		                        edm:publish(Database, <<"deal/gogodeals/database/info">>, 
		                                        {jtm:get_id(Message), false, to_map(ColumnNames, Rows)}, 1);
		                        
		                <<"deal/gogodeals/deal/fetch">> -> %% From Application
		                        {ok, ColumnNames, Rows} = 
		                                mysql:query(Database, 
		                                        "Select * From deals Where location = ?, filters in (?), deals not in (?)", jtm:get_values(Data)),
                			        edm:publish(Database, <<"deal/gogodeals/database/info">>, {jtm:get_id(Message), false, to_map(ColumnNames, Rows)}, 1)
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
		                        mysql:query(Database, "UPDATE clients SET name = ?, location = ?, email = ?, 
			                        password = ? WHERE id = ?", 
						jtm:stupid_sort(["name","location","email","password"], Data));
		
		                <<"deal/gogodeals/deal/save">> -> 
		                        mysql:query(Database, "UPDATE users SET deals = ? WHERE id = ?", jtm:stupid_sort(["deals","id"], Data)),
					mysql:query(Database, "UPDATE deals SET count = count - 1 WHERE id = ?", jtm:get_id(Message)),
					{ok, ColumnNames, Rows} = mysql:query(Database, "Select count From deals Where id = ?", jtm:get_id(Message)),
                			edm:publish(Database, <<"deal/gogodeals/database/info">>, {jtm:get_id(Message), false, to_map(ColumnNames, Rows)}, 1);
		
		                <<"deal/gogodeals/deal/remove">> -> %% From Application
		                        mysql:query(Database, "UPDATE users SET deals = ? WHERE id = ?", jtm:stupid_sort(["deals","id"], Data)),
					mysql:query(Database, "UPDATE deals SET count = count + 1 WHERE id = ?", jtm:get_id(Message));

		                <<"deal/gogodeals/user/filter">> -> 
		                        mysql:query(Database, "UPDATE users SET filters = ? WHERE id = ?", jtm:stupid_sort(["filters","id"],Data));

		                <<"deal/gogodeals/deal/verify">> -> todo
	                end,
			From ! {ok, updated},
			loop(Database);
		
		%% Delete data in the database according to the content of the Message
		{delete, From, Topic, Message} -> 
			case Topic of
		                <<"deal/gogodeals/client/delete">> -> 
		                        mysql:query(Database, "DELETE FROM clients WHERE id = ?", jtm:get_id(Message));
		
		                <<"deal/gogodeals/deal/delete">> -> 
		                        mysql:query(Database, "DELETE FROM deals WHERE id = ?", jtm:get_id(Message))
	                end,
			From ! {ok, deleted},
			loop(Database)
	end.


%% Convert a list of ColumnNames and a list of Rows into a map
to_map(ColumnNames, [Rows]) ->         
        maps:from_list(lists:zip(ColumnNames, Rows)).
