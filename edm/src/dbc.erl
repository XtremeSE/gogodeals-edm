-module(dbc).

%% API exports
-export([start/0, handle_call/4]).

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
		                        mysql:query(Database, "INSERT INTO deals (client_id, name, price, 
		                        picture, description, duration, count) VALUES (?,?,?,?,?,?)", [Message]);
		                <<"web/user/new">> -> 
		                        mysql:query(Database, "INSERT INTO clients (name, location, email, 
		                        password, niche) VALUES (?,?,?,?,?)", [Message]);
		                <<"app/user/new">> -> 
		                        mysql:query(Database, "INSERT INTO user (name, email, info) VALUES (?,?,?)", [Message])
	                end,
	                From ! {ok, inserted};
	                
		{select, From, Topic, Message} -> 
			case Topic of
		                <<"app/user/info">> -> 
		                        S = mysql:query(Database, "Select * From users Where id = ?", [Message]);
		                
		                <<"web/user/info">> -> 
		                        S = mysql:query(Database, "Select * From deals Where id = ?", [Message]);
		                
		                <<"web/deal/info">> -> 
		                        S = mysql:query(Database, "Select * From deals Where client_id = ?", [Message]);
		                
		                <<"app/deal/info">> -> 
		                        S = mysql:query(Database, "Select * From deals Where location = ?", [Message])
	                end,
			From ! S;
			
		{update, From, Topic, Message} -> 
			case Topic of
		                <<"web/deal/edit">> -> 
		                        mysql:query(Database, "UPDATE deals SET name = ?, price = ?, picture = ?, 
		                        description = ?, duration = ?, count = ? WHERE client_id = ?, id = ?", [Message]);
		
		                <<"web/user/edit">> -> 
		                        mysql:query(Database, "UPDATE clients SET name = ?, location = ?, email = ?, 
		                        password = ? WHERE id = ?", [Message]);
		
		                <<"app/deal/save">> -> 
		                        mysql:query(Database, "UPDATE users SET deals = ? WHERE id = ?", [Message]);
		
		                <<"app/deal/delete">> -> 
		                        mysql:query(Database, "UPDATE users SET deals = ? WHERE id = ?", [Message]);
				
		                <<"app/user/filter">> -> 
		                        mysql:query(Database, "UPDATE users SET filters = ? WHERE id = ?", [Message]);

		                <<"app/deal/verify">> -> todo
	                end,
			From ! {ok, updated};
		
		{delete, From, Topic, Message} -> 
			case Topic of
		                <<"web/deal/delete">> -> 
		                        mysql:query(Database, "DELETE FROM clients WHERE id = ?", [Message]);
		
		                <<"web/user/delete">> -> 
		                        mysql:query(Database, "DELETE FROM deals WHERE client_id = ?", [Message])
	                end,
			From ! {ok, deleted}
	end.

handle_call(Action, From, Topic, Payload) -> 
	Message = jtm:get_id(Payload) ++ jtm:get_data_values(Payload),
	database ! {Action, From, Topic, Message}.


