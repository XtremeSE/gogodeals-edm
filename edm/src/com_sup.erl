%%%-------------------------------------------------------------------
%% @doc mqtt communication supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(com_sup).

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
      supervisor:start_link(com_sup, []).  
        
%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, { {one_for_one, 0, 1}, [{edm, {edm, start, []}, permanent, brutal_kill, worker, [edm]}]}}.

%%====================================================================
%% Internal functions
%%====================================================================

