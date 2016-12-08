-module(handler_db).
-export([start_link/2]).

start_link(Action, Args) ->
	process_flag(trap_exit, false),	
	{ok, Pid} = mysql:start_link([{host, "129.16.155.11"},{user, "root"},{password, "password"},{database, "gogodeals"}]),	
		
	handle_call(Action, Pid, Args),	
	{ok, Pid}.

handle_call(insert, Database, {Topic, From, Message}) -> 
		Data = jtm:get_data(Message),
		[Id] = jtm:get_id(Message),		                
		case Topic of
			<<"deal/gogodeals/deal/new">> -> 
		   	Stmt = "INSERT INTO deals ("++ jtm:get_key(Data) ++ ") VALUES (?,?,?,?,?,?,?,?,?,?)", 
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
						mysql:query(Database, "insert into users (" ++ jtm:get_key(Data) ++ 
							", password) values (?,?,\"facebook\")", jtm:get_values(Data)),
					
					{ok, ColumnNames, Rows} = 
		         	mysql:query(Database, <<"Select * From users Where email = ? and name = ?">>, jtm:get_values(Data)),
						edm:publish(From, <<"deal/gogodeals/facebook/database">>, {Id, to_map(ColumnNames, Rows)}, 1);

					{ok, ColumnNames, Rows} -> 
						edm:publish(From, <<"deal/gogodeals/database/facebook">>, {Id, to_map(ColumnNames, Rows)}, 1)		
				end 
		end;

handle_call(select, Database, {Topic, From, Message}) -> 
		Data = jtm:get_data(Message),
		[Id] = jtm:get_id(Message),
		case Topic of
	    	<<"deal/gogodeals/user/info">> -> 
	    		{ok, ColumnNames, Rows} = 
	            mysql:query(Database, <<"Select * From users Where email = ? and password = ?">>, jtm:get_values(Data)),
				edm:publish(From, <<"deal/gogodeals/database/users">>, {Id, to_map(ColumnNames, Rows)}, 1);
				
			<<"deal/gogodeals/user/check">> ->
				{ok, ColumnNames, Rows} = 
	         	mysql:query(Database, <<"Select * From users Where email = ?">>, jtm:get_values(Data)),
				edm:publish(From, <<"deal/gogodeals/database/check">>, {Id, to_map(ColumnNames, Rows)}, 1);

			<<"deal/gogodeals/user/filters">> ->
				{ok, ColumnNames, Rows} = 
	         	mysql:query(Database, <<"Select * From users Where id = ?">>, [Id]),
				edm:publish(From, <<"deal/gogodeals/database/filters">>, {Id, to_map(ColumnNames, Rows)}, 1);

		                
	      <<"deal/gogodeals/client/info">> -> 
	      	{ok, ColumnNames, Rows} = 
		      	mysql:query(Database, "Select * From clients Where email = ? and password = ?", jtm:get_values(Data)),
				edm:publish(From, <<"deal/gogodeals/database/clients">>, {Id, to_map(ColumnNames, Rows)}, 1);
	                
	      <<"deal/gogodeals/deal/info">> -> %% From Website
				{ok, ColumnNames, Rows} = mysql:query(Database, "Select * From deals Where client_id = ?", [Id]),
				edm:publish(From, <<"deal/gogodeals/database/clients">>, {Id, to_deal_map(ColumnNames, Rows)}, 1);

	      <<"deal/gogodeals/deal/fetch">> -> %% From Application 		                                
				LongMin = [ V - 0.2 || {<<"longitude">>, V} <- Data],
				LongMax = [ V + 0.2 || {<<"longitude">>, V} <- Data],
				LatMin = [V - 0.2 || {<<"latitude">>, V} <- Data],
				LatMax = [V + 0.2 || {<<"latitude">>, V} <- Data],
				Filters = [binary_to_list(V) || {<<"filters">>, V} <- Data],
				{ok, ColumnNames, Rows} = mysql:query(Database, "Select * From deals Where longitude between ? and ? and 
					latitude between ? and ? and filters in(?)", 
					LongMin ++ LongMax ++ LatMin ++ LatMax ++ [Filters]),
              edm:publish(From, <<"deal/gogodeals/database/deals">>, {Id, to_deal_map(ColumnNames, Rows)}, 1);

			<<"deals/gogodeals/deal/grocode">> ->
				{ok, ColumnNames, Rows} = mysql:query(Database, "Select * From deals Where name in (?)", jtm:get_values(Data)),
				edm:publish(From, <<"deal/gogodeals/database/grocode">>, {Id, to_deal_map(ColumnNames, Rows)}, 1)
			
			end;

handle_call(update, Database, {Topic, From, Message}) -> 
	Data = jtm:get_data(Message),
	[Id] = jtm:get_id(Message), 
	case Topic of
		<<"deal/gogodeals/deal/edit">> -> 
			Data = jtm:get_data(Message),		                        
			mysql:query(Database, "UPDATE deals SET name = ?, price = ?, picture = ?, 
		   	description = ?, duration = ?, count = ? WHERE id = ?", 
				jtm:stupid_sort([<<"name">>, <<"price">>, <<"picture">>, <<"description">>, 
				<<"duration">>, <<"count">>],Data) ++ [Id]);
		
		<<"deal/gogodeals/client/edit">> -> 
		   mysql:query(Database, "UPDATE clients SET name = ?, longitude = ?, latitude = ?, email = ?, 
				password = ? WHERE id = ?", 
				jtm:stupid_sort([<<"name">>,<<"longitude">>, <<"latitude">>,<<"email">>,<<"password">>], Data) ++ [Id]);
		
		<<"deal/gogodeals/deal/save">> -> 
		   mysql:query(Database, "insert into userdeals(deal_id, user_id) values (?,?)", 
				[Id] ++ jtm:get_values(Data)),
			mysql:query(Database, "UPDATE deals SET count = count - 1 WHERE id = ?", [Id]),					
			mysql:query(Database, "insert into verify(deal_id, user_id) values (?,?)", 
				[Id] ++ jtm:get_values(Data)),

			{ok, ColumnNames, Rows} = 
				mysql:query(Database, "select deals.count, verify.id from deals, verify where deals.id = ? and verify.deal_id = deals.id", [Id]),
				edm:publish(From, <<"deal/gogodeals/database/info">>, {Id, to_map(ColumnNames, Rows)}, 1);
		
		<<"deal/gogodeals/deal/remove">> -> %% From Application
		   mysql:query(Database, "delete from userdeals where deal_id = ? and user_id = ?", [Id] ++ Data),		
			mysql:query(Database, "UPDATE deals SET count = count + 1 WHERE id = ?", [Id]),
			mysql:query(Database, "delete from verify where deal_id = ? and user_id = ?", [Id] ++ Data);
		
		<<"deal/gogodeals/user/update">> ->
			mysql:query(Database, "UPDATE users SET filters = ? WHERE id = ?", jtm:stupid_sort(["filters","id"],Data)),
			{ok, ColumnNames, Rows} = 
		   	mysql:query(Database, <<"Select filters From users Where id = ?">>, [Id]),
			edm:publish(From, <<"deal/gogodeals/database/filters">>, {Id, to_map(ColumnNames, Rows)}, 1);
			
		<<"deal/gogodeals/deal/verify">> -> 
			mysql:query(Database, "delete from verify where id = ?", [Id]),
			mysql:query(Database, "delete from userdeals where id = ?", [Id])
	end;

handle_call(delete, Database, {Topic, _From, Message}) -> 
	case Topic of
		<<"deal/gogodeals/client/delete">> -> 
			mysql:query(Database, "DELETE FROM clients WHERE id = ?", jtm:get_id(Message));
		
		<<"deal/gogodeals/deal/delete">> -> 
			mysql:query(Database, "DELETE FROM deals WHERE id = ?", jtm:get_id(Message))
	end.
	
	
	
%% Convert a list of ColumnNames and a list of Rows into a map
to_deal_map(_ColumnNames, []) -> #{};
to_deal_map(ColumnNames, [R|[]]) -> [to_map(ColumnNames, [R])];
to_deal_map(ColumnNames, [R|Rs]) -> [to_map(ColumnNames, [R])] ++ to_deal_map(ColumnNames, Rs).

%to_map(_ColumnNames, []) -> #{};
to_map(ColumnNames, [Rows]) -> 
	io:format("Step: ~p~n", ["3"]),	
	maps:from_list(lists:zip(ColumnNames, Rows)).
