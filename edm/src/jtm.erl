%%%-------------------------------------------------------------------
%% @doc module for json, map and list handling using jsx lib for json
%% @end
%%%-------------------------------------------------------------------

-module(jtm).

%% API exports
-export([get_id/1, is_encrypted/1, get_data_values/1, get_data/1, get_values/1, get_key/1, stupid_sort/2, get_action/1, to_payload/1]).

-export([test/0]).


%%====================================================================
%% API functions
%%====================================================================

%% Returns an id from a RFC Deal Message Transfer message
get_id(Message) ->
	M = jsx:decode(Message, [return_maps]),
	{ok, Id} = maps:find(<<"id">>, M),
	[Id].

%% Return if data from a RFC Deal Message Transfer message is encrypted
%% or not.
is_encrypted(Message) ->
	M = jsx:decode(Message, [return_maps]),
	{ok, Encrypted} = maps:find(<<"payload_encryption">>, M),
	Encrypted.

%% Return the values of the data collection within the message as a list
get_data_values(Message) -> 
	M = jsx:decode(Message, [return_maps]),
	io:format("Decoded json map: ~p~n", [M]),
	{ok, Data} = maps:find(<<"data">>, M),
	T = [ Y || {_,Y} <- [maps:find(X, Data) || X <- maps:keys(Data)]].	

get_data(Message) -> 
        M = jsx:decode(Message, [return_maps]),
	io:format("Decoded json map: ~p~n", [M]),
	{ok, Data} = maps:find(<<"data">>, M),
	maps:to_list(Data).

get_values([]) -> [];
get_values([{_,V}|L]) -> 
        case is_binary(V) of
                true -> [binary_to_list(V)] ++ get_values(L);
                false -> [V] ++ get_values(L)
        end.

get_key([{K,_}|[]]) -> binary:bin_to_list(K);
get_key([{K,_}|Xs]) -> binary:bin_to_list(K) ++ ", " ++ get_key(Xs).


%%stupid_sort(Key, List) -> [ V || {K,V} <- List, Ks <- Key, K == Ks].

stupid_sort([], []) -> [];
stupid_sort([K|[]], [{K,V}|[]]) -> [V];
stupid_sort([K|Ks], [{K,V}|Ls]) -> [V] ++ stupid_sort(Ks, Ls);
stupid_sort([K|Ks], List) -> stupid_sort(Ks ++ [K], List).


%% Return an action for the database corresponding with a message Topic
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

%% Converts a tuple of {Id, Encryption, MapOfArguments} into a payload message adhearing to the RFC Deal Message Transfer
to_payload({Id, Encryption, MapOfArguments}) -> 
        Payload = #{ target_id => Id, payload_encryption => Encryption, data => MapOfArguments},
	jsx:encode(Payload).
        

test() ->
	Message = <<"{\"id\": \"12345678-1111-M123-N123-123456123456\", \"payload_encryption\": false, \"data\": {\"name\": \"Coffee\", \"client_id\": 10, \"duration\": 1234, \"test\": null},}">>,
	get_data(Message).


%%====================================================================
%% Internal functions
%%====================================================================

