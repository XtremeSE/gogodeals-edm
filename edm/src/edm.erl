%%%-------------------------------------------------------------------
%% @doc module for initializing a simple emqttc connection to a MQTT 
%%      broker and listening for calls.
%% @end
%%%-------------------------------------------------------------------

-module(edm).

%% API
-export([start/0, publish/4]).

%%====================================================================
%% API functions
%%====================================================================

start() -> 
	init().

publish(From, Topic, Message, Qos) ->
        From ! {send, {Topic, Message, Qos}}.

%%====================================================================
%% Internal functions
%%====================================================================

init() ->
	{ok, Client} = emqttc:start_link([{host, "176.10.136.208"},{client_id, <<"simpleClient">>}, {proto_ver, 3}]),
	emqttc:subscribe(Client, [{<<"web/deal/info">>, 1}, 
				        {<<"web/deal/new">>, 1},
					{<<"web/deal/edit">>, 1},
					{<<"web/deal/delete">>, 1},
					{<<"web/user/info">>, 1},
					{<<"web/user/new">>, 1},
					{<<"web/user/edit">>, 1},
					{<<"web/user/delete">>, 1},
					{<<"app/deal/info">>, 1},
					{<<"app/deal/save">>, 1},
					{<<"app/deal/delete">>, 1},
					{<<"app/deal/verify">>, 1},
					{<<"app/user/info">>, 1},
					{<<"app/user/new">>, 1},
					{<<"app/user/filter">>, 1}]),
        broker_loop(Client),
	%io:format("Broker connection is up: ~p~n", [Broker]),
	register(client, Client).


broker_loop(Client) ->
	receive
 	
	        %% Receive messages from subscribed topics
                {publish, Topic, Payload} ->
                        io:format("Message from ~s: ~p~n", [Topic, jtm:get_data_values(Payload)]),
		        dbc:handle_call(jtm:get_action(Topic), Client, Topic, Payload),
                        io:format("Everything is awesome! ~p~n", ["Cool"]),
                        broker_loop(Client);

                %% Publish messages with a topic and Qos to the broker
                {send, {Topic, Message, Qos}} ->
                        io:format("Publish: ~p~n", [Message]),
		        Payload = jtm:to_payload(Message),
                        emqttc:publish(Client, Topic, Payload, [{qos, Qos}]),
                        broker_loop(Client);                        

                %% Stop the loop as a part of stopping the client
                stop ->
                        exit(broker, normal)
                        
	end.


