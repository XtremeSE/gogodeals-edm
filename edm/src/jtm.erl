-module(jtm).

%% API exports
-export([test/0]).

%%====================================================================
%% API functions
%%====================================================================

get_id(Message) ->
	M = jsx:decode(Message, [return_maps]),
	{ok, Id} = maps:find(<<"id">>, M),
	[Id].

is_encrypted(Message) ->
	M = jsx:decode(Message, [return_maps]),
	{ok, Encrypted} = maps:find(<<"payload_encryption">>, M),
	Encrypted.

%% Return the values of the data collection within the message as a list
get_data_values(Message) -> 
	M = jsx:decode(Message, [return_maps]),
	{ok, Data} = maps:find(<<"data">>, M),
	[ Y || {_,Y} <- [maps:find(X, Data) || X <- maps:keys(Data)]].	

get_action(Topic) ->
        case Topic of
                <<"web/deal/info">> -> select;
                <<"web/deal/new">> -> insert;
		<<"web/deal/edit">> -> update;
		<<"web/deal/delete">> -> delete;
		
		<<"web/user/info">> -> select;
		<<"web/user/new">> -> insert;
		<<"web/user/edit">> -> update;
		<<"web/user/delete">> -> delete;
		
		<<"app/deal/info">> -> select;
		<<"app/deal/save">> -> update;
		<<"app/deal/delete">> -> update;
		<<"app/deal/verify">> -> update;
		
		<<"app/user/info">> -> select;
		<<"app/user/new">> -> insert;
		<<"app/user/filter">> -> update
        end.
        

test() ->
	Message = <<"{\"id\": \"12345678-1111-M123-N123-123456123456\", \"payload_encryption\": false, \"data\": {\"name\": \"Coffee\"},}">>,
	get_id(Message) ++ get_data_values(Message).


%%====================================================================
%% Internal functions
%%====================================================================

