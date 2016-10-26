-module(edm).

%% API exports
-export([start/0]).

%%====================================================================
%% API functions
%%====================================================================

start() -> 
	spawn(fun () -> init(),
	io:format("Client is up: ~p~n", [whereis(client)]) end).

%%====================================================================
%% Internal functions
%%====================================================================

init() ->
	{ok, Client} = emqttc:start_link([{host, "localhost"},{client_id, <<"simpleClient">>}, {proto_ver, 3}]),
	emqttc:subscribe(Client, [{<<"topic">>, 0}, 
					{<<"web/deal/new">>, 1},
					{<<"web/deal/edit">>, 1},
					{<<"web/deal/delete">>, 1},
					{<<"web/user/new">>, 1},
					{<<"web/user/edit">>, 1},
					{<<"web/user/delete">>, 1},
					{<<"app/deal/save">>, 1},
					{<<"app/deal/delete">>, 1},
					{<<"app/deal/verify">>, 1},
					{<<"app/user/new">>, 1},
					{<<"app/user/filter">>, 1}]),
        broker_loop(Client),
	%io:format("Broker connection is up: ~p~n", [Broker]),
	register(client, Client).

json(Message) -> 
	Payload = lists:append("{'target_id': 1,'data': {", Message,"}}"),
	jsx:encode(Payload).

un_jason(Payload) ->
	Message = jsx:decode(Payload, [return_maps]).

broker_loop(Client) ->
	receive
 	
	%% Receive messages from subscribed topics
        {publish, Topic, Payload} ->
                io:format("Message from ~s: ~p~n", [Topic, Payload]),
		dbc:handle_call(jtm:get_action(Topic), Client, Topic, Payload),
                broker_loop(Client);

        %% Publish messages with a topic and Qos to the broker
        {send, Topic, Message, Qos} ->
                io:format("Publish: ~p~n", [Message]),
		Payload = json(Message),
                emqttc:publish(Client, Topic, Payload, [{qos, Qos}]),
                broker_loop(Client);

        %% Stop the loop as a part of stopping the client
        stop ->
                exit(broker, normal)
	end.

