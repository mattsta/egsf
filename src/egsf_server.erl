-module(egsf_server).
-behaviour(gen_server).

-compile(export_all).

%% gen_server callbacks
-export([init/1,
         handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

%% api callbacks
-export([start_link/0, start_link/1, start_link/2]).

-record(state, {keyspace % paramterized module
               }).

%%%--------------------------------------------------------------------
%%% api callbacks
%%%--------------------------------------------------------------------
start_link() ->
  start_link(ets).

start_link(ets) ->
  gen_server:start_link({global, ?MODULE}, ?MODULE, ets, []);
start_link({redis, _, _} = Config) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

start_link(LocalGlobal, OtherKeyspace) ->
  gen_server:start_link({LocalGlobal, ?MODULE}, ?MODULE, OtherKeyspace, []).

%%%--------------------------------------------------------------------
%%% gen_server callbacks
%%%--------------------------------------------------------------------
init(Config) ->
  Keyspace = case Config of
               ets -> egsf_provider_ets:new();
               {redis, Server, Port} -> egsf_provider_redis:new(Server, Port);
               {keyspace, PrivateKeyspace} -> PrivateKeyspace
             end,
  {ok, #state{keyspace = Keyspace}}.

handle_call({lock, Key, Wait}, From, #state{keyspace = Keyspace} = State) ->
  case Keyspace:advise(Key) of
    {wait, _lockId} -> Keyspace:enqueue(Key, From),
                       {noreply, State};
               free -> {reply, lock(Keyspace, Key, Wait), State}
  end;

handle_call({renew, Key, LockId}, _, #state{keyspace = Keyspace} = State) ->
  {reply, Keyspace:renew(Key, LockId, existing), State};

handle_call({renew, Key, LockId, Wait}, _, #state{keyspace=Keyspace} = State) ->
  {reply, Keyspace:renew(Key, LockId, Wait), State}.

handle_cast({unlock, Key, LockId}, #state{keyspace = Keyspace} = State) ->
  Keyspace:unlock(Key, LockId),
  case Keyspace:dequeue(Key) of
    empty -> ok;
               % What happens if OldFrom doesn't exist anymore?  Did we just
               % stop dequeuing things and cause a lock backup?
               % Ideally, we should cycle through the dead Froms until we find
               % a good one.
    OldFrom -> case Keyspace:advise(Key) of
                 free -> gen_server:reply(OldFrom, lock(Keyspace, Key, 500));
                    _ -> waitmore
               end
  end,
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%--------------------------------------------------------------------
%%% Lock State Management
%%%--------------------------------------------------------------------
lock(Keyspace, Key, Wait) ->
  {locked, Keyspace:lock(Key, Wait)}.
