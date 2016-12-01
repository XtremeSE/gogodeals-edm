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

%% Start a connection to a broker
start() -> init().

%% Publish messages to the broker
publish(From, Topic, Message, Qos) ->
        Payload = jtm:to_payload(Message),
	emqttc:publish(From, Topic, Payload, [{qos, Qos}]),
	io:format("Step: ~p~n", ["4"]).

%%====================================================================
%% Internal functions
%%====================================================================

init() ->
	{ok, Client} = emqttc:start_link([{host, "176.10.136.208"},{client_id, <<"databaseControllerClient">>}, {keepalive, 0}, {proto_ver, 31}]),
	emqttc:subscribe(Client, [	%% Client/Customer
					{<<"deal/gogodeals/deal/info">>, 1},
				        {<<"deal/gogodeals/deal/new">>, 1},
					{<<"deal/gogodeals/deal/edit">>, 1},
					{<<"deal/gogodeals/deal/delete">>, 1},
					{<<"deal/gogodeals/client/info">>, 1},
					{<<"deal/gogodeals/client/new">>, 1},
					{<<"deal/gogodeals/client/edit">>, 1},
					{<<"deal/gogodeals/client/delete">>, 1},
					
					%% User
					{<<"deal/gogodeals/deal/fetch">>, 1},
					{<<"deal/gogodeals/deal/save">>, 1},
					{<<"deal/gogodeals/deal/remove">>, 1},
					{<<"deal/gogodeals/deal/verify">>, 1},
					{<<"deal/gogodeals/user/info">>, 1},
					{<<"deal/gogodeals/user/new">>, 1},
					{<<"deal/gogodeals/user/filter">>, 1}]),
        broker_loop(Client),
	register(client, Client).


broker_loop(Client) ->
	receive
 	
	        %% Receive messages from subscribed topics
                {publish, Topic, Payload} ->
		        dbc:handle_call(jtm:get_action(Topic), Client, Topic, Payload),
                        broker_loop(Client);

                %% Stop the loop as a part of stopping the client
                stop -> exit(broker, normal);

		{error, _} -> start()
                        
	end.


