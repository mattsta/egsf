-module(egsf_provider_ets, [KeyId, WaitId]).

-compile(export_all).

-record(key, {key,
              lockid,
              created,   % ms timestamp
              duration   % ms duration
             }).

-record(wait, {key,
               clients}).

%%%--------------------------------------------------------------------
%%% instance creation
%%%--------------------------------------------------------------------
new() ->
  KeyId  = ets:new(?MODULE, [private, set, {keypos, 2}]),
  WaitId = ets:new(?MODULE, [private, ordered_set, {keypos, 2}]),
  instance(KeyId, WaitId).

%%%--------------------------------------------------------------------
%%% provider callbacks
%%%--------------------------------------------------------------------
advise(Key) ->
  case ets:lookup(KeyId, Key) of
    [#key{lockid = LockId} = K] ->
      % If the lock is expired, allow a new one in.
      case expired(K) of
         true -> ets:delete(KeyId, Key),
                 free;
        false -> {wait, LockId}
      end;
     [] -> free
  end.

lock(Key, Duration) ->
  K = key(Key, Duration),
  true = ets:insert_new(KeyId, K),
  K#key.lockid.

enqueue(Key, From) ->
  case ets:lookup(WaitId, Key) of
    [W] -> ets:insert(WaitId, enqueue_client(W, From));
      _ -> ets:insert(WaitId, create_queue(Key, From))
  end.

dequeue(Key) ->
  case ets:lookup(WaitId, Key) of
    [W] -> {NewWait, Client} = dequeue_client(W),
           ets:insert(WaitId, NewWait),
           Client;
      _ -> empty
  end.

renew(Key, LockId, NewDuration) ->
  Now = now_to_ms(),
  case ets:lookup(KeyId, Key) of
    [#key{lockid = LockId} = K] ->
      case expired(K) of
         true -> expired;
        false -> case NewDuration of
                   existing -> ets:insert(KeyId, K#key{created = Now});
                          _ -> ets:insert(KeyId, K#key{created = Now,
                                                       duration = NewDuration})
                 end
      end;
    _ -> invalid_lockid
  end.

unlock(Key, LockId) ->
  case ets:lookup(KeyId, Key) of
    [#key{lockid = LockId} = K] ->
      ets:delete(KeyId, Key),
      case expired(K) of
         true -> expired;
        false -> ok
      end;
    _ -> invalid_lockid
  end.

%%%--------------------------------------------------------------------
%%% Helpers
%%%--------------------------------------------------------------------
key(Key, Duration) ->
  MS = now_to_ms(),
  #key{key = Key,
       lockid = make_ref(),
       created = MS,
       duration = Duration}.

expired(#key{created = Created, duration = Duration}) ->
  (Created + Duration) < now_to_ms().

create_queue(Key, From) ->
  W = #wait{key = Key,
            clients = gb_trees:empty()},
  enqueue_client(W, From).

enqueue_client(#wait{clients = Clients} = W, From) ->
  W#wait{clients = gb_trees:insert(now(), From, Clients)}.

dequeue_client(#wait{clients = Clients} = W) ->
  case gb_trees:size(Clients) of
    0 -> {W, empty};
    _ -> {_InsertTS, FoundFrom, NewTree} = gb_trees:take_smallest(Clients),
         {W#wait{clients = NewTree}, FoundFrom}
  end.

now_to_ms() ->
  now_to_ms(now()).

now_to_ms({Mega, Sec, Micro}) ->
  trunc((((Mega * 1000000) + Sec) + (Micro / 1000000)) * 1000).
