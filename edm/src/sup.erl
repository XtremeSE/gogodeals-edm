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

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).
     % supervisor:start_link(sup, []).  
        
%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, { {one_for_one, 5, 60}, [
					%{db_sup, {db_sup, start_link, []}, transient, infinity, supervisor, [db_sup]}                                        
					{dbc, {dbc, start, []}, permanent, infinity, worker, [dbc]},
                                        {edm, {edm, start, []}, permanent, infinity, worker, [edm]}
                                ]}}.

%%====================================================================
%% Internal functions
%%====================================================================

