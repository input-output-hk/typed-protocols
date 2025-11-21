module Network.TypedProtocol.ReqResp3.Tests (tests) where

import Control.Monad.IOSim

import Network.TypedProtocol.ReqResp.Proofs

import Network.TypedProtocol.ReqResp3.Client
import Network.TypedProtocol.ReqResp3.Server

import Test.Tasty.QuickCheck
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests = testGroup "Network.TypedProtocol.ReqResp3"
  [ testProperty "connect clientPeer:Max"   prop_connect
  , testProperty "connect clientPeer:FIFO"  prop_connect_FIFO
  , testProperty "connect clientPeer:FIFO'" prop_connect_FIFO'
  , testProperty "connect RoundRobin:Max"   prop_connect_RoundRobin_Max
  , testProperty "connect RoundRobin:FIFO"  prop_connect_RoundRobin_FIFO
  , testProperty "connect RoundRobin:FIFO'" prop_connect_RoundRobin_FIFO'
  ]


-- list of all Fibonacci numbers
fib :: [Int]
{-# INLINE fib #-}
fib = fst <$> induction
  where
    -- list of pairs n-th and (n+1)-th Fibonacci numbers
    induction :: [(Int, Int)]
    induction =
      -- beginning of induction
      (0,1) :
      -- inductive step
      [ (q, p + q)
      | (p, !q) <- induction
      ]


prop_connect
  :: NonNegative Int
  -> NonNegative Int
  -> NonNegative Int
  -> Property
prop_connect (NonNegative m) (NonNegative n) (NonNegative o) =
    let trace = runSimTrace (connect (clientPeer m n o) serverMax)
    in  counterexample (ppTrace trace)
      $ case traceResult True trace of
          Left err       -> counterexample (show err) False
          Right (res, _) -> res === results
  where
    results :: [Result]
    results =
         [ ResInt  a  | a <- take m [0..]]
      ++ [ ResFib  a  | a <- take n fib]
      ++ [ ResStr [a] | a <- take o ['a'..]]


prop_connect_FIFO
  :: NonNegative Int
  -> NonNegative Int
  -> NonNegative Int
  -> Property
prop_connect_FIFO (NonNegative m) (NonNegative n) (NonNegative o) =
    let trace = runSimTrace (connect (clientPeer m n o) serverFIFO)
    in  counterexample (ppTrace trace)
      $ case traceResult True trace of
          Left err       -> counterexample (show err) False
          Right (res, _) -> res === results
  where
    results :: [Result]
    results =
         [ ResInt  a  | a <- take m [0..]]
      ++ [ ResFib  a  | a <- take n fib]
      ++ [ ResStr [a] | a <- take o ['a'..]]


prop_connect_FIFO'
  :: NonNegative Int
  -> NonNegative Int
  -> NonNegative Int
  -> Property
prop_connect_FIFO' (NonNegative m) (NonNegative n) (NonNegative o) =
    let trace = runSimTrace (connect (clientPeer m n o) serverFIFO')
    in  counterexample (ppTrace trace)
      $ case traceResult True trace of
          Left err       -> counterexample (show err) False
          Right (res, _) -> res === results
  where
    results :: [Result]
    results =
         [ ResInt  a  | a <- take m [0..]]
      ++ [ ResFib  a  | a <- take n fib]
      ++ [ ResStr [a] | a <- take o ['a'..]]


prop_connect_RoundRobin_Max
  :: NonNegative Int
  -> Property
prop_connect_RoundRobin_Max (NonNegative n) =
    let trace = runSimTrace (connect (clientPeerRoundRobin n) serverMax)
    in  counterexample (ppTrace trace)
      $ case traceResult True trace of
          Left err       -> counterexample (show err) False
          Right (res, _) -> res === results
  where
    results :: [Result]
    results =
         [ ResInt  a  | a <- take n [0..]]
      ++ [ ResFib  a  | a <- take n fib]
      ++ [ ResStr [a] | a <- take n ['a'..]]


prop_connect_RoundRobin_FIFO
  :: NonNegative Int
  -> Property
prop_connect_RoundRobin_FIFO (NonNegative n) =
    let trace = runSimTrace (connect (clientPeerRoundRobin n) serverFIFO)
    in  counterexample (ppTrace trace)
      $ case traceResult True trace of
          Left err       -> counterexample (show err) False
          Right (res, _) -> res === results
  where
    results :: [Result]
    results =
      [ r
      | (a, b, c) <- take n $ zip3 [0..] fib ['a'..]
      , r <- [ResStr [c], ResFib b, ResInt a]
      ]


prop_connect_RoundRobin_FIFO'
  :: NonNegative Int
  -> Property
prop_connect_RoundRobin_FIFO' (NonNegative n) =
    let trace = runSimTrace (connect (clientPeerRoundRobin n) serverFIFO')
    in  counterexample (ppTrace trace)
      $ case traceResult True trace of
          Left err       -> counterexample (show err) False
          Right (res, _) -> res === results
  where
    results :: [Result]
    results =
      [ r
      | (a, b, c) <- take n $ zip3 [0..] fib ['a'..]
      , r <- [ResStr [c], ResFib b, ResInt a]
      ]
