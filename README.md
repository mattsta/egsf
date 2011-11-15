egsf: erlang general serializing framework
==========================================

What is it?
-----------
egsf provides advisory locking across an erlang cluster.

Why?
----
egsf can add serializable transaction semantics to a distributed key-value
store.  Example: registering user accounts at a website with a
non-transactional backend.  You are missing atomic read-modify-write operations,
but you can fake it with egsf.  The first lock is granted on a key
(`{register, Username}`). Further attempts to lock the same key will
block until the lock is released (or until the wait-for-lock
timeout is exceeded).  When you obtain the lock, check if the account
exists.  If yes, somebody else made the account.  If not, you can continue
creating the account.

Organization
------------
egsf is split into three parts:

* front end API (`egsf`)
* backend synchronous action layer (`egsf_server`)
* backend cluster-wide datastore (`egsf_provider_ets`, `egsf_provider_redis`)

API
---
Request a lock for 500ms (blocks until lock is available (waits up to 60s)):

    {locked, LockId} = egsf:lock({create_user, "bob@bob.com"}, 500)

Renew a lock if you want to keep it longer:

    egsf:renew(Key, LockId)       % renew LockId for original lock time
    egsf:renew(Key, LockId, 500)  % renew LockId for 500ms

Release a lock:

    egsf:unlock(LockId)

You should always release your locks.  Do not rely on the expiration time.
The expiration time in your lock and renew calls are only to prevent
a lock from not being released if your process dies.

After the lock expiration time is exceeded, another process *may* obtain
a lock even if you have not released your existing lock.  You get into a
messy situation of running with an old lock while a new process comes
along and obtains a new lock because your prior lock expired.  Use renew
generously to re-up your lock expiration time.

Integration
-----------
Integrating egsf into your platform requires you to:

* Decide on backend for egsf
  * Small sites can use the default ets backend
* Start the egsf application
* Lock, Renew, and Release/Unlock as needed.

Large Scale vs. Small Scale Notes
---------------------------------
### Small Scale
The ets backend requires one instance of egsf for your entire cluster.  The
`egsf_server` will be registered globally because all independent lock
requestors must be funneled through the same ets table gated by `egsf_server`.

### Larger Scale
The redis backend allows `egsf_server` to be registered non-globally
since redis stores all state and timeout information external to Erlang.

### Largest Scale
For a large scale global lock server, abstract egsf_server to sharded
instances managed by riak_core.


Backend State Keeping Providers
-------------------------------
You can implement your own state provider.  The protocol/behavior of a
backend provider requires the following functions from a parameterized module:

* advise(Key :: term()) -> {wait, LockId} | free.
  * Determine if Key is already locked.  If not, return free.
* lock(Key :: term(), Wait :: pos_integer()) -> LockId
  * Lock Key for Wait milliseconds and return LockId for future manipulations.
* enqueue(Key, From :: term()) -> any().
  * If a Key is locked, put the requesting client in a queue for the Key.
* dequeue(Key) -> any().
  * Dequeue the first waiting client for Key and send it a gen_server:reply/2.
* renew(Key, LockId, Wait :: pos_integer() | existing) -> ok | expired | invalid_lockid.
  * Attempt to renew LockId for Wait milliseconds.
* unlock(Key, LockId) -> ok | expired | invalid_lockid.
  * If LockId exists, release lock on Key it represents and dequeue next caller.

Building
--------
    rebar compile

Testing
-------
     rebar eunit skip_deps=true suite=egsf_tests

Initial Status
--------------
Right now, only the ets backend is implemented and the front end API `egsf` is
hard-coded to use the global process registry to find `egsf_server`.

Additional Features
-------------------
* Implement `isLocked?` for keys.
* Ponder how to deal with operating in the absense or crash of a lock server.
