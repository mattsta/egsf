-module(egsf_sup).

-behaviour(supervisor).

%% External exports
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  Egsf = {egsf_server,
          {egsf_server, start_link, []},
          permanent, 5000, worker, [egsf_server]},
  Processes = [Egsf],

  Strategy = {one_for_one, 10, 10},
  {ok,
   {Strategy, lists:flatten(Processes)}}.
