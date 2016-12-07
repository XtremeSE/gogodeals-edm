%%%-------------------------------------------------------------------
%% @doc module for json, map and list handling using jsx lib for json
%% @end
%%%-------------------------------------------------------------------

-module(jtm).

%% API exports
-export([get_id/1, get_data_values/1, get_data/1, get_values/1, get_key/1, stupid_sort/2, get_action/1, to_payload/1, get_filters/1]).



%%====================================================================
%% API functions
%%====================================================================

%% Returns an id from a RFC Deal Message Transfer message
get_id(Message) ->
	M = jsx:decode(Message, [return_maps]),
	{ok, Id} = maps:find(<<"id">>, M),
	[Id].


%% Return the values of the data collection within the message as a list
get_data_values(Message) -> 
	M = jsx:decode(Message, [return_maps]),
	io:format("Decoded json map: ~p~n", [M]),
	{ok, Data} = maps:find(<<"data">>, M),
	[ Y || {_,Y} <- [maps:find(X, Data) || X <- maps:keys(Data)]].	


%% Takes a json message and converts it into a list of tuples 
get_data(Message) -> 
        M = jsx:decode(Message, [return_maps]),
	io:format("Decoded json map: ~p~n", [M]),
	{ok, Data} = maps:find(<<"data">>, M),
	maps:to_list(Data).


%% Return a list of values from a key value tuple
get_values([]) -> [];
get_values([{_,V}|L]) -> 
        case is_binary(V) of
                true -> [binary_to_list(V)] ++ get_values(L);
                false -> [V] ++ get_values(L)
        end.


%% Return a list of keys from a key value tuple
get_key([{K,_}|[]]) -> binary:bin_to_list(K);
get_key([{K,_}|Xs]) -> binary:bin_to_list(K) ++ ", " ++ get_key(Xs).


%% Takes a list of Key(s) and a list of {Key, Value} and sort the list 
%% of tuples with the keys of the Key list
stupid_sort([], []) -> [];
stupid_sort([K|[]], [{K,V}|[]]) -> [V];
stupid_sort([K|Ks], [{K,V}|[]]) -> [V];
stupid_sort([K|Ks], [{K,V}|Ls]) -> [V|stupid_sort(Ks, Ls)];
stupid_sort(Keys, [L|Ls]) -> stupid_sort(Keys, Ls ++ [L]).


%% Return an action for the database corresponding with a message Topic
get_action(Topic) ->
        case Topic of
                <<"deal/gogodeals/deal/info">> -> select;
                <<"deal/gogodeals/deal/new">> -> insert;
		<<"deal/gogodeals/deal/edit">> -> update;
		<<"deal/gogodeals/deal/delete">> -> delete;
		
		<<"deal/gogodeals/client/info">> -> select;
		<<"deal/gogodeals/client/new">> -> insert;
		<<"deal/gogodeals/client/edit">> -> update;
		<<"deal/gogodeals/client/delete">> -> delete;
		
		<<"deal/gogodeals/deal/fetch">> -> select;
		<<"deals/gogodeals/deal/grocode">> -> select;
		<<"deal/gogodeals/deal/save">> -> update;
		<<"deal/gogodeals/deal/remove">> -> update;
		<<"deal/gogodeals/deal/verify">> -> update;
		
		<<"deal/gogodeals/user/info">> -> select;
		<<"deal/gogodeals/user/check">> -> select;
		<<"deal/gogodeals/user/update">> -> select;
		<<"deal/gogodeals/user/facebook">> -> insert;
		<<"deal/gogodeals/user/new">> -> insert;
		<<"deal/gogodeals/user/filter">> -> update;
		_ -> unknown
        end.

%% Converts a tuple of {Id, MapOfArguments} into a payload message
to_payload({Id, MapOfArguments}) -> 
        Payload = #{ id => Id, data => MapOfArguments},
	jsx:encode(Payload).

%to_payload({Id, List, Request}) ->
%	Payload = #{ client_id => Id, list => List, request => Request},
%	jsx:encode(Payload).

get_filters([]) -> [];
get_filters(ListOfWords) -> [string:sub_word(ListOfWords, 1)] ++ get_filters(string:sub_string(ListOfWords, string:chr(ListOfWords, ","))).
        

%%====================================================================
%% Internal functions
%%====================================================================
 
