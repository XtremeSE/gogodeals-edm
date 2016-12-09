%%%-------------------------------------------------------------------
%% @doc db top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/1]).

%% Supervisor callbacks
-export([init/1]).

%-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).
     % supervisor:start_link(sup, []).  
        
%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
	Database = [{host, "129.16.155.11"},{user, "root"},{password, "password"},{database, "gogodeals"}], 
	LocalDb = [{host, "localhost"},{user, "root"},{password, "password"},{database, "gogodeals"}],

	Prata = [{host, "54.154.153.243"},{client_id, <<"gogodealsAwesomeClient">>}, {keepalive, 0}, {proto_ver, 31}],
	Testing = [{host, "176.10.136.208"},{client_id, <<"bob">>}, {keepalive, 0}, {proto_ver, 31}],
	
	{ok, { {one_for_all, 5, 10}, [
					%{db_sup, {db_sup, start_link, [LocalDb]}, transient, infinity, supervisor, [db_sup]},                                        
					%{com_sup, {com_sup, start_link, [Prata]}, transient, infinity, supervisor, [db_sup]}
					
					{dbc, {dbc, start, [Database]}, permanent, brutal_kill, worker, []},
               {prata, {edm, start, [Prata]}, permanent, brutal_kill, worker, []}
					%{testing, {edm, start, [Testing]}, transient, infinity, worker, []}
		]}}.

%%====================================================================
%% Internal functions
%%====================================================================

start_child(mqtt) ->
	supervisor:start_child(com_sup, []);
	
start_child(db) ->
	supervisor:start_child(db_sup, []).

