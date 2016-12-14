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

%% Start a connection to a MySQL database
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
				<<"deal/gogodeals/deal/new">> -> 
		      	Stmt = "INSERT INTO deals (" ++ jtm:get_key(Data) ++ ") VALUES (?,?,?,?,?,?,?,?,?,?)",
		      	Values = jtm:get_values(Data),
		         mysql:query(Database, Stmt, Values);
		                        
		      <<"deal/gogodeals/client/new">> -> 
		      	mysql:query(Database, "INSERT INTO clients (" ++ jtm:get_key(Data) ++ ") VALUES (?,?,?,?,?)", jtm:get_values(Data)),
					edm:publish(From, <<"deal/gogodeals/database/new">>, {Id, true}, 1);
					
				<<"deal/gogodeals/user/new">> -> 
		      	mysql:query(Database, "INSERT INTO users (" ++ jtm:get_key(Data) ++ ") VALUES (?,?,?)", jtm:get_values(Data));
		                        
				<<"deal/gogodeals/user/facebook">> ->
					
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
		      <<"deal/gogodeals/user/info">> ->
		      	Data = jtm:get_data(Message), 
		         {ok, ColumnNames, Rows} = 
		         	mysql:query(Database, <<"Select * From users Where email = ? and password = ?">>, jtm:get_values(Data)),
					edm:publish(From, <<"deal/gogodeals/database/users">>, {Id, to_map(ColumnNames, Rows)}, 1);
					
				<<"deal/gogodeals/deal/grabbed">> ->
		         case mysql:query(Database, <<"Select * From deals Where id in (select deal_id from userdeals where user_id = ?)">>, [Id]) of
		         
		         	{ok, ColumnNames, []} -> 
							edm:publish(From, <<"deal/gogodeals/database/grabbed">>, {Id, #{ yo => dude}}, 1);
						
						{ok, ColumnNames, Rows} -> 
							edm:publish(From, <<"deal/gogodeals/database/grabbed">>, {Id, to_map(ColumnNames, Rows)}, 1)
					
					end;
				
				<<"deal/gogodeals/user/check">> ->
					Data = jtm:get_data(Message),
					{ok, ColumnNames, Rows} = 
		         	mysql:query(Database, <<"Select * From users Where email = ?">>, jtm:get_values(Data)),
					edm:publish(From, <<"deal/gogodeals/database/check">>, {Id, to_map(ColumnNames, Rows)}, 1);

				<<"deal/gogodeals/user/filter">> ->
					{ok, ColumnNames, Rows} = 
		            mysql:query(Database, <<"Select filters From users Where id = ?">>, [Id]),
					edm:publish(From, <<"deal/gogodeals/database/filters">>, {Id, to_map(ColumnNames, Rows)}, 1);
					
				<<"deal/gogodeals/client/info">> ->
					Data = jtm:get_data(Message), 
		      	{ok, ColumnNames, Rows} = 
			      	mysql:query(Database, "Select * From clients Where email = ? and password = ?", jtm:get_values(Data)),
					edm:publish(From, <<"deal/gogodeals/database/clients">>, {Id, to_map(ColumnNames, Rows)}, 1);
		                
		      <<"deal/gogodeals/deal/info">> -> %% From Website
					{ok, ColumnNames, Rows} = mysql:query(Database, "Select * From deals Where client_id = ?", [Id]),
					edm:publish(From, <<"deal/gogodeals/database/clients">>, {Id, to_deal_map(ColumnNames, Rows)}, 1);

		      <<"deal/gogodeals/deal/fetch">> -> %% From Application
		      	Data = jtm:get_data(Message), 		                                
					LongMin = [ V - 0.2 || {<<"longitude">>, V} <- Data],
					LongMax = [ V + 0.2 || {<<"longitude">>, V} <- Data],
					LatMin = [V - 0.2 || {<<"latitude">>, V} <- Data],
					LatMax = [V + 0.2 || {<<"latitude">>, V} <- Data],
					Filters = [binary_to_list(V) || {<<"filters">>, V} <- Data],
					{ok, ColumnNames, Rows} = 
						mysql:query(Database, "Select * From deals Where longitude between ? and ? and latitude between ? and ? and filters in(?)", 
							LongMin ++ LongMax ++ LatMin ++ LatMax ++ [Filters]),
                	edm:publish(From, <<"deal/gogodeals/database/deals">>, {Id, to_deal_map(ColumnNames, Rows)}, 1);

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
		      <<"deal/gogodeals/deal/edit">> -> 
					Data = jtm:get_data(Message),		                        
					mysql:query(Database, "UPDATE deals SET name = ?, price = ?, picture = ?, description = ?, duration = ?, count = ? WHERE id = ?", 
						jtm:stupid_sort([<<"name">>, <<"price">>, <<"picture">>, <<"description">>, <<"duration">>, <<"count">>],Data) ++ jtm:get_id(Message));
							
				<<"deal/gogodeals/client/edit">> -> 
					Data = jtm:get_data(Message),
		         mysql:query(Database, "UPDATE clients SET name = ?, longitude = ?, latitude = ?, email = ?, password = ? WHERE id = ?", 
						jtm:stupid_sort([<<"name">>,<<"longitude">>, <<"latitude">>,<<"email">>,<<"password">>], Data) ++ jtm:get_id(Message));
		
		   <<"deal/gogodeals/deal/save">> -> 
					Data = jtm:get_data(Message),
               [Id] = jtm:get_id(Message), 
		         mysql:query(Database, "insert into userdeals(deal_id, user_id) values (?,?)", [Id] ++ jtm:get_values(Data)),
					
					mysql:query(Database, "UPDATE deals SET count = count - 1 WHERE id = ?", [Id]),
					
					mysql:query(Database, "insert into verify(deal_id, user_id) values (?,?)", [Id] ++ jtm:get_values(Data)),

					{ok, ColumnNames, Rows} = 
						mysql:query(Database, "select deals.count, verify.id from deals, verify where deals.id = ? and verify.deal_id = deals.id", [Id]),

					edm:publish(From, <<"deal/gogodeals/database/info">>, {Id, to_map(ColumnNames, Rows)}, 1);
					
			<<"deal/gogodeals/deal/remove">> -> %% From Application
					Data = jtm:get_data(Message),
		                        mysql:query(Database, "delete from userdeals where deal_id = ? and user_id = ?", 
						jtm:get_id(Message) ++ jtm:get_values(Data)),
					
					mysql:query(Database, "UPDATE deals SET count = count + 1 WHERE id = ?", 
						jtm:get_id(Message)),

					mysql:query(Database, "delete from verify where deal_id = ? and user_id = ?", 
						jtm:get_id(Message) ++ jtm:get_values(Data));

			<<"deal/gogodeals/user/update">> -> 
				Data = jtm:get_data(Message),
				[Id] = jtm:get_id(Message),		                        
				mysql:query(Database, "UPDATE users SET filters = ? WHERE id = ?", jtm:get_values(Data) ++ [Id]),
				{ok, ColumnNames, Rows} = 
		      	mysql:query(Database, <<"Select filters From users Where id = ?">>, [Id]),
				edm:publish(From, <<"deal/gogodeals/database/update">>, {Id, to_map(ColumnNames, Rows)}, 1);
				
			<<"deal/gogodeals/deal/verify">> -> 
					[Id] = jtm:get_id(Message),
					{ok, ColumnNames, Rows} = mysql:query(Database, 
						"Select user_id from verify where id = ?", [Id]),
					mysql:query(Database, "delete from verify where id = ?", [Id]),
					mysql:query(Database, "delete from userdeals where id = ?", [Id])					

	      end,
			%%From ! {ok, updated},
			loop(Database);
		
		%% Delete data in the database according to the content of the Message
		{delete, _From, Topic, Message} -> 
			case Topic of
		                <<"deal/gogodeals/client/delete">> -> 
		                        mysql:query(Database, "DELETE FROM clients WHERE id = ?", jtm:get_id(Message));
		
		                <<"deal/gogodeals/deal/delete">> -> 
		                        mysql:query(Database, "DELETE FROM deals WHERE id = ?", jtm:get_id(Message))
	                end,
			loop(Database);
		
		_ -> loop(Database)
	end.


%% Convert a list of ColumnNames and a list of Rows into a map
to_deal_map(_ColumnNames, []) -> #{};
to_deal_map(ColumnNames, [R|[]]) -> [to_map(ColumnNames, [R])];
to_deal_map(ColumnNames, [R|Rs]) -> [to_map(ColumnNames, [R])] ++ to_deal_map(ColumnNames, Rs).

%to_map(_ColumnNames, []) -> #{};
to_map(ColumnNames, [Rows]) -> 
	io:format("Step: ~p~n", ["3"]),	
	maps:from_list(lists:zip(ColumnNames, Rows)).
	
	
terminate(Reason) ->
	exit({terminate, Reason}, database).


