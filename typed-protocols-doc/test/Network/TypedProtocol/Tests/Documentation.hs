{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE DataKinds #-}

module Network.TypedProtocol.Tests.Documentation
where

import Test.Tasty
import Test.Tasty.QuickCheck

import Data.SerDoc.Class
import Data.Maybe

import Network.TypedProtocol.Documentation
import Network.TypedProtocol.Documentation.TestProtocol
import Network.TypedProtocol.Tests.ControlProtocol

{-# ANN module "HLINT: ignore Use camelCase" #-}
{-# ANN module "HLINT: ignore Move brackets to avoid $" #-}

tests :: TestTree
tests = testGroup "Documentation"
          [ testProperty "no errors" (testProtocolDescription `seq` True)
          , testProperty "agencies" (p_correctAgencies testProtocolDescription)
          , testProperty "state transitions" (p_correctStateTransitions testProtocolDescription)
          ]

testProtocolDescription :: ProtocolDescription (TestCodec ())
testProtocolDescription = $(describeProtocol ''ControlProtocol [''IO, ''()] ''TestCodec [''()])

p_correctAgencies :: ProtocolDescription (TestCodec ()) -> Property
p_correctAgencies d =
  counterexample (show stateAgencyMap) .
  once $
  counterexample "EndState" (lookup (State "EndState") stateAgencyMap === Just NobodyAgencyID)
  .&&.
  counterexample "InitialState" (lookup (State "InitialState") stateAgencyMap === Just ServerAgencyID)
  .&&.
  counterexample "IdleState" (lookup (State "IdleState") stateAgencyMap === Just ServerAgencyID)
  .&&.
  counterexample "WaitForConfirmationState" (lookup (State "WaitForConfirmationState") stateAgencyMap === Just ClientAgencyID)
  .&&.
  counterexample "WaitForInfoState" (lookup (State "WaitForInfoState") stateAgencyMap === Just ClientAgencyID)
  .&&.
  counterexample "WaitForPublicKeyState" (lookup (State "WaitForPublicKeyState") stateAgencyMap === Just ClientAgencyID)
  where
    stateAgencyMap = [(state, agency) | (state, _, agency) <- protocolStates d]

p_correctStateTransitions :: ProtocolDescription (TestCodec ()) -> Property
p_correctStateTransitions d =
  once $
    checkMessage "VersionMessage" (State "InitialState") (State "IdleState")
    .&&.
    checkMessage "GenStagedKeyMessage" (State "IdleState") (State "WaitForPublicKeyState")
    .&&.
    checkMessage "QueryStagedKeyMessage" (State "IdleState") (State "WaitForPublicKeyState")
    .&&.
    checkMessage "DropStagedKeyMessage" (State "IdleState") (State "WaitForPublicKeyState")
    .&&.
    checkMessage "PublicKeyMessage" (State "WaitForPublicKeyState") (State "IdleState")
    .&&.
    checkMessage "InstallKeyMessage" (State "IdleState") (State "WaitForConfirmationState")
    .&&.
    checkMessage "InstallResultMessage" (State "WaitForConfirmationState") (State "IdleState")
    .&&.
    checkMessage "RequestInfoMessage" (State "IdleState") (State "WaitForInfoState")
    .&&.
    checkMessage "InfoMessage" (State "WaitForInfoState") (State "IdleState")
    .&&.
    checkMessage "AbortMessage" (State "InitialState") (State "EndState")
    .&&.
    checkMessage "EndMessage" (State "IdleState") (State "EndState")
    .&&.
    checkMessage "ProtocolErrorMessage" AnyState (State "EndState")
  where
    checkMessage :: String -> StateRef -> StateRef -> Property
    checkMessage msgName fromState toState =
      counterexample msgName $ do
        msg <- findMessage msgName
        return $
          counterexample "fromState"
            (messageFromState msg === fromState)
          .&&.
          counterexample "toState"
            (messageToState msg === toState)

    findMessage :: String -> Maybe (MessageDescription (TestCodec ()))
    findMessage msgName =
      listToMaybe [ msg | msg <- protocolMessages d, messageName msg == msgName ]
