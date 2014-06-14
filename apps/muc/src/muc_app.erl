-module(muc_app).

-behaviour(application).
-behaviour(supervisor).

%% Application callbacks
-export([start/2, stop/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, A), {I, {I, start_link, A}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Nodes = case application:get_env(ecomponent, mnesia_nodes) of
        {ok, N} -> N;
        undefined -> throw("NEED mnesia_nodes CONFIGURATION!")
    end,
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Nodes]).

stop(_State) ->
    ok.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Nodes]) ->
	Call = {muc_room, start_link, []},
    {ok, {{one_for_one, 5, 10}, [
    	?CHILD(forseti, supervisor, [Call, Nodes])
    ]}}.
