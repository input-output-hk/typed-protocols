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
  counterexample "EndState" (lookup "EndState" stateAgencyMap === Just NobodyAgencyID)
  .&&.
  counterexample "InitialState" (lookup "InitialState" stateAgencyMap === Just ServerAgencyID)
  .&&.
  counterexample "IdleState" (lookup "IdleState" stateAgencyMap === Just ServerAgencyID)
  .&&.
  counterexample "WaitForConfirmationState" (lookup "WaitForConfirmationState" stateAgencyMap === Just ClientAgencyID)
  .&&.
  counterexample "WaitForInfoState" (lookup "WaitForInfoState" stateAgencyMap === Just ClientAgencyID)
  .&&.
  counterexample "WaitForPublicKeyState" (lookup "WaitForPublicKeyState" stateAgencyMap === Just ClientAgencyID)
  where
    stateAgencyMap = [(state, agency) | (state, _, agency) <- protocolStates d]

p_correctStateTransitions :: ProtocolDescription (TestCodec ()) -> Property
p_correctStateTransitions d =
  once $
    checkMessage "VersionMessage" "InitialState" "IdleState"
    .&&.
    checkMessage "GenStagedKeyMessage" "IdleState" "WaitForPublicKeyState"
    .&&.
    checkMessage "QueryStagedKeyMessage" "IdleState" "WaitForPublicKeyState"
    .&&.
    checkMessage "DropStagedKeyMessage" "IdleState" "WaitForPublicKeyState"
    .&&.
    checkMessage "PublicKeyMessage" "WaitForPublicKeyState" "IdleState"
    .&&.
    checkMessage "InstallKeyMessage" "IdleState" "WaitForConfirmationState"
    .&&.
    checkMessage "InstallResultMessage" "WaitForConfirmationState" "IdleState"
    .&&.
    checkMessage "RequestInfoMessage" "IdleState" "WaitForInfoState"
    .&&.
    checkMessage "InfoMessage" "WaitForInfoState" "IdleState"
    .&&.
    checkMessage "AbortMessage" "InitialState" "EndState"
    .&&.
    checkMessage "EndMessage" "IdleState" "EndState"
    .&&.
    checkMessage "ProtocolErrorMessage" "st" "EndState"
  where
    checkMessage :: String -> String -> String -> Property
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
