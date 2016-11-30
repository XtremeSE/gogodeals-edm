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
    %%supervisor:start_link({local, ?SERVER}, ?MODULE, []).
      supervisor:start_link(sup, []).  
        
%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, { {one_for_all, 0, 1}, [
                                        {dbc, {dbc, start, []}, permanent, 1000, worker, [dbc]},
                                        {edm, {edm, start, []}, permanent, 1000, worker, [edm]}
                                ]}}.

%%====================================================================
%% Internal functions
%%====================================================================

