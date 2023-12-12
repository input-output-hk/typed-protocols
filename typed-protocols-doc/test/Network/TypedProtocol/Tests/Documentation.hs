{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE DataKinds #-}

module Network.TypedProtocol.Tests.Documentation
where

import Test.Tasty
import Test.Tasty.QuickCheck

import Data.SerDoc.Class
import Network.TypedProtocol.Documentation
import Network.TypedProtocol.Documentation.TestProtocol
import Network.TypedProtocol.Tests.ControlProtocol

{-# ANN module "HLINT: ignore Use camelCase" #-}
{-# ANN module "HLINT: ignore Move brackets to avoid $" #-}

tests :: TestTree
tests = testGroup "Documentation"
          [ testProperty "no errors" (testProtocolDescription `seq` True)
          , testProperty "correct agencies" (p_correctAgencies testProtocolDescription)
          ]

testProtocolDescription :: ProtocolDescription (TestCodec ())
testProtocolDescription = $(describeProtocol ''ControlProtocol [''IO, ''()] ''TestCodec [''()])

p_correctAgencies :: ProtocolDescription (TestCodec ()) -> Property
p_correctAgencies d =
  counterexample (show stateAgencyMap) .
  once $
  (counterexample "EndState" $ lookup "EndState" stateAgencyMap === Just NobodyAgencyID)
  .&&.
  (counterexample "InitialState" $ lookup "InitialState" stateAgencyMap === Just ServerAgencyID)
  .&&.
  (counterexample "IdleState" $ lookup "IdleState" stateAgencyMap === Just ServerAgencyID)
  .&&.
  (counterexample "WaitForConfirmationState" $ lookup "WaitForConfirmationState" stateAgencyMap === Just ClientAgencyID)
  .&&.
  (counterexample "WaitForInfoState" $ lookup "WaitForInfoState" stateAgencyMap === Just ClientAgencyID)
  .&&.
  (counterexample "WaitForPublicKeyState" $ lookup "WaitForPublicKeyState" stateAgencyMap === Just ClientAgencyID)
  where
    stateAgencyMap = [(state, agency) | (state, _, agency) <- protocolStates d]
