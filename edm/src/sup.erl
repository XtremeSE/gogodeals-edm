%%%-------------------------------------------------------------------
%% @doc db top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).  
        
%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
	Database = [{host, "129.16.155.11"},{user, "root"},{password, "password"},{database, "gogodeals"}], 
	%LocalDb = [{host, "localhost"},{user, "root"},{password, "password"},{database, "gogodeals"}],

	Prata = [{host, "54.154.153.243"},{client_id, <<"gogodealsAwesomeClient">>}, {keepalive, 0}, {proto_ver, 31}],
	%Testing = [{host, "176.10.136.208"},{client_id, <<"BadBob">>}, {keepalive, 0}, {proto_ver, 31}],
	
	{ok, { {one_for_one, 30, 60}, [
		{dbc, {dbc, start, [Database]}, permanent, brutal_kill, worker, [dbc]},
      {prata, {edm, start, [Prata]}, permanent, brutal_kill, worker, [edm]}
	]}}.


