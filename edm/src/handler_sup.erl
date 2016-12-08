-module(handler_sup).
-export([start_link/0]).

start_link() ->
	process_flag(trap_exit, true),
 	{ok, _Pid} = handler_mqtt:start_link(),
 	receive
 		{'EXIT', _From, normal} ->
     		ok;

		{'EXIT', _From, _Reason} ->
     		%io:format("Process ~p exited for reason ~p~n",[Pid,Reason]),
     		start_link()
 	end.
