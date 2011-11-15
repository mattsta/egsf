-module(egsf_tests).

-include_lib("eunit/include/eunit.hrl").

-define(E(A, B), ?assertEqual(A, B)).
-define(_E(A, B), ?_assertEqual(A, B)).

setup_clean() ->
  ets:new(testing, [named_table, public, set]),
  application:start(egsf).

cleanup(_) ->
  application:stop(egsf),
  ets:delete(testing).

egsf_runthrough_test_() ->
  Key = {create, <<"bob">>},
  {setup,
    fun setup_clean/0,
    fun cleanup/1,
    fun(_) ->
      [
        fun() ->
          {LockedAtom, LockId} = egsf:lock(Key),
          ?assertMatch(locked, LockedAtom),
          egsf:renew(Key, LockId),
          egsf:unlock(Key, LockId)
        end
      ]
    end
  }.

egsf_parallel_test_() ->
  {setup,
    fun setup_clean/0,
    fun cleanup/1,
    fun(_) ->
      ets:insert(testing, {got, 0}),
      [
       {inparallel, 566,
         [
         {timeout, 300,
           fun() ->
             {locked, LockId} = egsf:lock(incr, 500, infinity),
             ?debugFmt("Locked", ""),
             [{got, N}] = ets:lookup(testing, got),
             ets:insert(testing, {got, N + 1}),
             timer:sleep(35),
             [{got, N2}] = ets:lookup(testing, got),
             ?assertEqual(N + 1, N2),
             ?debugFmt("Released with val ~p", [N2]),
             egsf:unlock(incr, LockId)
           end
         }
         || _ <- lists:seq(1, 1655)
         ]
       }
      ]
    end
  }.
