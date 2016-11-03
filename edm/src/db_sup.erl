%%%-------------------------------------------------------------------
%% @doc database supervisor.
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
    %%supervisor:start_link({local, ?SERVER}, ?MODULE, []).
      supervisor:start_link(db_sup, []).  
        
%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, { {one_for_one, 0, 1}, [{dbc, {dbc, start, []}, 
        permanent, brutal_kill, worker, [dbc]}]}}.

%%====================================================================
%% Internal functions
%%====================================================================

