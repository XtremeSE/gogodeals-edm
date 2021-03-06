%%%-------------------------------------------------------------------
%% @doc module for connecting to a MySQL db through the mysql/otp api
%% @end
%%%-------------------------------------------------------------------

-module(dbc).

%% API exports
-export([start/1, handle_call/4, terminate/1]).

-export([init/1]).

%%====================================================================
%% API functions
%%====================================================================

%% Start a connection to a MySQL database with the mysql-otp library
%% Takes a list of arguments with the form
%% [{host, "1.1.1.1"},{user, "test"},{password, "test"},{database, "test"}]
%% For complete list of parameters see the mysql-otp API
start(Args) -> init(Args).

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
init(Args) ->
   {ok, Pid} = mysql:start_link(Args),	
	
	Db = spawn(fun () -> loop(Pid) end),
	register(database, Db),	
	{ok, Pid}.


%% Listens for calls.
loop(Database) ->
	receive
		%% Insert the content of a Message into the expected 
		%% table in the database
		{insert, From, Topic, Message} -> 
			Data = jtm:get_data(Message),
			[Id] = jtm:get_id(Message),		                
			case Topic of
				%% Insert new deal into the database
				%% In line with PRATA RFC 15 Add Deal
				<<"deal/gogodeals/deal/new">> -> 
		      	Stmt = "INSERT INTO deals (" ++ jtm:get_key(Data) ++ ") VALUES (?,?,?,?,?,?,?,?,?,?)",
		      	Values = jtm:get_values(Data),
		         mysql:query(Database, Stmt, Values);
		                        
		      %% Insert new client into the database
		      %% Publish "true" to the callers id.
		      <<"deal/gogodeals/client/new">> -> 
		      	mysql:query(Database, "INSERT INTO clients (" ++ jtm:get_key(Data) ++ ") VALUES (?,?,?,?,?)", jtm:get_values(Data)),
					edm:publish(From, <<"deal/gogodeals/database/new">>, {Id, true}, 1);
					
				%% Insert new user into the database
				<<"deal/gogodeals/user/new">> -> 
		      	mysql:query(Database, "INSERT INTO users (" ++ jtm:get_key(Data) ++ ") VALUES (?,?,?)", jtm:get_values(Data));
		                        
		      %% Insert new user (with facebook credentials) into the database
				<<"deal/gogodeals/user/facebook">> ->
					
					%% Only inserts new user if the user doesnt already exist in the db.
					%% Publish the user id to the callers id.
					case mysql:query(Database, <<"Select * From users Where email = ? and name = ?">>, jtm:get_values(Data)) of
						
						{ok, ColumnNames, []} ->
							mysql:query(Database, "insert into users (" ++ jtm:get_key(Data) ++ ", password) values (?,?,\"facebook\")", jtm:get_values(Data)),
							{ok, ColumnNames, Rows} = 
		               	mysql:query(Database, <<"Select * From users Where email = ? and name = ?">>, jtm:get_values(Data)),
							edm:publish(From, <<"deal/gogodeals/facebook/database">>, {Id, to_map(ColumnNames, Rows)}, 1);

						{ok, ColumnNames, Rows} -> 
							edm:publish(From, <<"deal/gogodeals/database/facebook">>, {Id, to_map(ColumnNames, Rows)}, 1)
						
					end
					

	      end,
	      loop(Database);
	        
	   %% Select info from the database corresponding to the 
	   %% Topic and publish it.      
		{select, From, Topic, Message} -> 
			[Id] = jtm:get_id(Message),
			case Topic of
			
				%% Publish information of the user with a email and password corresponding to the information in the message
		      <<"deal/gogodeals/user/info">> ->
		      	Data = jtm:get_data(Message), 
		         {ok, ColumnNames, Rows} = 
		         	mysql:query(Database, <<"Select * From users Where email = ? and password = ?">>, jtm:get_values(Data)),
					edm:publish(From, <<"deal/gogodeals/database/users">>, {Id, to_map(ColumnNames, Rows)}, 1);
				
				%% Publish information of the deal when the deal is grabbed	
				<<"deal/gogodeals/deal/grabbed">> ->
		         {ok, ColumnNames, Rows} =
		         	mysql:query(Database, <<"Select * From deals Where id in (select deal_id from userdeals where user_id = ?)">>, [Id]), 
					edm:publish(From, <<"deal/gogodeals/database/grabbed">>, {Id, to_deal_map(ColumnNames, Rows)}, 1);
				
				%% Publish information of the user with an email corresponding to the one in the message
				<<"deal/gogodeals/user/check">> ->
					Data = jtm:get_data(Message),
					{ok, ColumnNames, Rows} = 
		         	mysql:query(Database, <<"Select * From users Where email = ?">>, jtm:get_values(Data)),
					edm:publish(From, <<"deal/gogodeals/database/check">>, {Id, to_map(ColumnNames, Rows)}, 1);

				%% Publish information of the filters marked by a user
				%% Gogodeals messaging manual:
				<<"deal/gogodeals/user/filter">> ->
					{ok, ColumnNames, Rows} = 
		            mysql:query(Database, <<"Select filters From users Where id = ?">>, [Id]),
					edm:publish(From, <<"deal/gogodeals/database/filters">>, {Id, to_map(ColumnNames, Rows)}, 1);
					
				%% Publish information of the client with a email and password corresponding to the information in the message
				%% Gogodeals messaging manual:
				<<"deal/gogodeals/client/info">> ->
					Data = jtm:get_data(Message), 
		      	{ok, ColumnNames, Rows} = 
			      	mysql:query(Database, "Select * From clients Where email = ? and password = ?", jtm:get_values(Data)),
					edm:publish(From, <<"deal/gogodeals/database/clients">>, {Id, to_map(ColumnNames, Rows)}, 1);
		                
		      %% Publish information of the deals created by a client
		      %% Gogodeals messaging manual:
		      <<"deal/gogodeals/deal/info">> ->
					{ok, ColumnNames, Rows} = mysql:query(Database, "Select * From deals Where client_id = ?", [Id]),
					edm:publish(From, <<"deal/gogodeals/database/clients">>, {Id, to_deal_map(ColumnNames, Rows)}, 1);

				%% Publish information of the deals within a +- 0.2 range (longitude and latitude degrees) from a location and with the right filters.
				%% PRATA RFC 16 Get Deals
		      <<"deal/gogodeals/deal/fetch">> ->
		      	M = jsx:decode(Message, [return_maps]),
					{ok, D} = maps:find(<<"data">>, M), 		                                
					LongMin = [ V - 0.2 || {<<"longitude">>, V} <- maps:to_list(D)],
					LongMax = [ V + 0.2 || {<<"longitude">>, V} <- maps:to_list(D)],
					LatMin = [V - 0.2 || {<<"latitude">>, V} <- maps:to_list(D)],
					LatMax = [V + 0.2 || {<<"latitude">>, V} <- maps:to_list(D)],
					{ok, F} = maps:find(<<"filters">>, D),
					Filters = lists:concat([["\"" ++ binary_to_list(Z) ++ "\"" || {_,Z} <- [maps:find(Y, X) || Y <- maps:keys(X)]] || X <- F]),
					{ok, ColumnNames, Rows} = 
						mysql:query(Database, "Select * From deals Where longitude between ? and ? and latitude between ? and ? and filters in(" ++ jtm:put_comma(Filters) ++ ")", 
							LongMin ++ LongMax ++ LatMin ++ LatMax),
                	edm:publish(From, <<"deal/gogodeals/database/deals">>, {Id, to_deal_map(ColumnNames, Rows)}, 1);

				%% Publish a list of deals corresponding to a list of items from the Gro system
				%% Related to PRATA RFC 29 Add list/Delete list/Fetch lists
				<<"deal/gogodeals/deal/grocode">> ->
					M = jsx:decode(Message, [return_maps]),
					{ok, D} = maps:find(<<"data">>, M),
					Data = lists:concat([["\"" ++ binary_to_list(Z) ++ "\"" || {_,Z} <- [maps:find(Y, X) || Y <- maps:keys(X)]] || X <- D]),
					{ok, ColumnNames, Rows} = mysql:query(Database, "Select * From deals Where name in (" ++ jtm:put_comma(Data) ++ ")"),
					edm:publish(From, <<"deal/gogodeals/database/grocode">>, {Id, to_deal_map(ColumnNames, Rows)}, 1)
	   	
	   	end,
	   	loop(Database);
			
		%% Update the content of a Message into the expected table in the database
		{update, From, Topic, Message} ->
			case Topic of
		      
		      %% Change information of a deal
		      <<"deal/gogodeals/deal/edit">> -> 
					Data = jtm:get_data(Message),		                        
					mysql:query(Database, "UPDATE deals SET name = ?, price = ?, picture = ?, description = ?, duration = ?, count = ? WHERE id = ?", 
						jtm:stupid_sort([<<"name">>, <<"price">>, <<"picture">>, <<"description">>, <<"duration">>, <<"count">>],Data) ++ jtm:get_id(Message));
							
				%% Change information about a client
				<<"deal/gogodeals/client/edit">> -> 
					Data = jtm:get_data(Message),
		         mysql:query(Database, "UPDATE clients SET name = ?, longitude = ?, latitude = ?, email = ?, password = ? WHERE id = ?", 
						jtm:stupid_sort([<<"name">>,<<"longitude">>, <<"latitude">>,<<"email">>,<<"password">>], Data) ++ jtm:get_id(Message));
		
				%% Grab a deal and publish the deal count and the verification code to the caller
		   	<<"deal/gogodeals/deal/save">> -> 
					Data = jtm:get_data(Message),
               [Id] = jtm:get_id(Message), 
		         mysql:query(Database, "insert into userdeals(deal_id, user_id) values (?,?)", [Id] ++ jtm:get_values(Data)),
					
					mysql:query(Database, "UPDATE deals SET count = count - 1 WHERE id = ?", [Id]),
					
					mysql:query(Database, "insert into verify(deal_id, user_id) values (?,?)", [Id] ++ jtm:get_values(Data)),

					{ok, ColumnNames, Rows} = 
						mysql:query(Database, "select deals.count, verify.id from deals, verify where deals.id = ? and verify.deal_id = deals.id", [Id]),

					[User_Id] = jtm:get_data_values(Message),
					edm:publish(From, <<"deal/gogodeals/database/info">>, {User_Id, to_map(ColumnNames, Rows)}, 1);
					
				%% Ungrab a deal from the the user
				<<"deal/gogodeals/deal/remove">> ->
					Data = jtm:get_data(Message),
		                        mysql:query(Database, "delete from userdeals where deal_id = ? and user_id = ?", 
						jtm:get_id(Message) ++ jtm:get_values(Data)),
					
					mysql:query(Database, "UPDATE deals SET count = count + 1 WHERE id = ?", 
						jtm:get_id(Message)),

					mysql:query(Database, "delete from verify where deal_id = ? and user_id = ?", 
						jtm:get_id(Message) ++ jtm:get_values(Data));

				%% Update the users stored filters and publish the new set of filters to the caller
				<<"deal/gogodeals/user/update">> -> 
					Data = jtm:get_data(Message),
					[Id] = jtm:get_id(Message),		                        
					mysql:query(Database, "UPDATE users SET filters = ? WHERE id = ?", jtm:get_values(Data) ++ [Id]),
					{ok, ColumnNames, Rows} = 
		      		mysql:query(Database, <<"Select filters From users Where id = ?">>, [Id]),
					edm:publish(From, <<"deal/gogodeals/database/update">>, {Id, to_map(ColumnNames, Rows)}, 1);
				
				%% Delete the users connection with the deal
				<<"deal/gogodeals/deal/verify">> -> 
					[Id] = jtm:get_id(Message),
					mysql:query(Database, "delete from verify where id = ?", [Id]),
					mysql:query(Database, "delete from userdeals where id = ?", [Id])					

	      end,
			loop(Database);
		
		%% Delete data in the database according to the content of the Message
		{delete, _From, Topic, Message} -> 
			case Topic of
			
				%% Delete a client from the database
		   	<<"deal/gogodeals/client/delete">> -> 
		      	mysql:query(Database, "DELETE FROM clients WHERE id = ?", jtm:get_id(Message));
		
				%% Delete a deal from the database
		   	<<"deal/gogodeals/deal/delete">> -> 
		   		mysql:query(Database, "DELETE FROM deals WHERE id = ?", jtm:get_id(Message))
	      end,
			loop(Database);
		
		_ -> loop(Database)
	end.


%% Convert a list of ColumnNames and a list of Rows into a map of maps of deals
to_deal_map(_ColumnNames, []) -> #{};
to_deal_map(ColumnNames, [R|[]]) -> [to_map(ColumnNames, [R])];
to_deal_map(ColumnNames, [R|Rs]) -> [to_map(ColumnNames, [R])] ++ to_deal_map(ColumnNames, Rs).

%% Convert a list of ColumnNames and a Row into a map of deals
to_map(_ColumnNames, []) -> #{};
to_map(ColumnNames, [Rows]) -> 
	io:format("Step: ~p~n", ["3"]),	
	maps:from_list(lists:zip(ColumnNames, Rows)).
	
	
terminate(Reason) ->
	exit({terminate, Reason}, database).


