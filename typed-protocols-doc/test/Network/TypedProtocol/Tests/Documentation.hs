{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE DataKinds #-}

module Network.TypedProtocol.Tests.Documentation
where

import Test.Tasty
import Test.Tasty.QuickCheck

import Data.SerDoc.Class
import Network.TypedProtocol.Documentation
import Network.TypedProtocol.Documentation.TestProtocol
import Network.TypedProtocol.Documentation.TH

tests :: TestTree
tests = testGroup "Documentation"
          [ testProperty "no errors" (testProtocolDescription `seq` True)
          , testProperty "correct agencies" (p_correctAgencies testProtocolDescription)
          ]

testProtocolDescription :: ProtocolDescription (TestCodec ())
testProtocolDescription = $(describeProtocol ''TestProtocol [''()] ''TestCodec [''()])

p_correctAgencies :: ProtocolDescription (TestCodec ()) -> Property
p_correctAgencies d = once $
  (counterexample "IdleState" $ lookup "IdleState" stateAgencyMap === Just ServerAgencyID)
  .&&.
  (counterexample "AwaitingPongState" $ lookup "AwaitingPongState" stateAgencyMap === Just ClientAgencyID)
  where
    stateAgencyMap = [(state, agency) | (state, _, agency) <- protocolStates d]
