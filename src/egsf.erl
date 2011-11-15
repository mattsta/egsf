-module(egsf).

-compile(export_all).

%%%--------------------------------------------------------------------
%%% Lock / Renew / Release
%%%--------------------------------------------------------------------
lock(Key) ->
  lock(Key, 500).

lock(Key, Timeout) ->
  gen_server:call({global, egsf_server}, {lock, Key, Timeout}, 60000).

lock(Key, Timeout, LockWait) ->
  gen_server:call({global, egsf_server}, {lock, Key, Timeout}, LockWait).

renew(Key) ->
  gen_server:call({global, egsf_server}, {renew, Key}).

renew(Key, Timeout) ->
  gen_server:call({global, egsf_server}, {renew, Key, Timeout}).

unlock(Key, LockId) ->
  gen_server:cast({global, egsf_server}, {unlock, Key, LockId}).
