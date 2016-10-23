-module(dbc).

%% API exports
-export([start/0, handle_call/4]).

%%====================================================================
%% API functions
%%====================================================================

start() ->
        {ok, Pid} = mysql:start_link([{host, "localhost"}, {user, "root"},
                              {password, "password"}, {database, "gogodeals"}]),
	Db = spawn(fun () -> loop(Pid) end),
	register(database, Db).

%%====================================================================
%% Internal functions
%%====================================================================


loop(Database) ->
	receive
		{insert, From, Topic, Message} -> 
			insert(Topic, Message),
			From ! {ok, inserted};
		{select, From, Message} -> 
			S = select(Database, Message),
			From ! S;
		{update, From, Topic, Message} -> 
			update(Topic, Message),
			From ! {ok, updated};
		{delete, From, Topic, Message} -> 
			delete(Topic, Message),
			From ! {ok, deleted}
	end.

handle_call(Action, From, Topic, Payload) -> 
	Message = jsx:decode(Payload, [return_maps]),
	database ! {Action, From, Topic, Message}.

insert(Topic, Message) -> 
	case Topic of
		<<"web/deal/new">> -> ok;
		<<"web/user/new">> -> ok;
		<<"app/user/new">> -> ok
	end.

select(Database, Message) -> 
	case Message of
		user -> ok;
		client -> ok;
		{deals, Location} -> mysql:query(Database, "Select * From deals Where location = ?", [Location])
	end.

update(Topic, Message) -> todo.

delete(Topic, Message) -> todo.
