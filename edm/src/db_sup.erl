%%%-------------------------------------------------------------------
%% @doc db top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(db_sup).

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

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, { {one_for_all, 0, 1}, [
                                        {edm, {edm, start, []}, permanent, brutal_kill, worker, [edm]},
                                        {dbc, {dbc, start, []}, permanent, brutal_kill, worker, [dbc]}
                                ]} }.

%%====================================================================
%% Internal functions
%%====================================================================

