%%%-------------------------------------------------------------------
%% @doc database supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(db_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link(Args) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Args).
     %% supervisor:start_link(db_sup, []).  
        
%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init(Args) ->
    {ok, { {simple_one_for_one, 10, 60}, [{dbc, {dbc, start, [Args]}, transient, 1000, worker, [dbc]}]}}.

%%====================================================================
%% Internal functions
%%====================================================================

