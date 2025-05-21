{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

-- | The module contains exclusion lemmas which are proven using ad absurdum:
--
-- * it's impossible for both client and server have agency
-- * it's impossible for either side to be in a terminal state (no agency) and
--   the other side have agency
--
module Network.TypedProtocol.Lemmas where

import Data.Kind (Type)
import Network.TypedProtocol.Core


-- $about
--
-- Typed languages such as Haskell can embed proofs. In total languages this
-- is straightforward: a value inhabiting a type is a proof of the property
-- corresponding to the type.
--
-- In languages like Haskell that have ⊥ as a value of every type, things
-- are slightly more complicated. We have to demonstrate that the value that
-- inhabits the type of interest is not ⊥ which we can do by evaluation.
--
-- This idea crops up frequently in advanced type level programming in Haskell.
-- For example @Refl@ proofs that two types are equal have to have a runtime
-- representation that is evaluated to demonstrate it is not ⊥ before it
-- can be relied upon.
--
-- The proofs here are about the nature of typed protocols in this framework.
-- The 'connect' and 'connectPipelined' proofs rely on a few internal lemmas.

-- | An evidence that both relative agencies are equal to 'NobodyHasAgency'.
--
type ReflNobodyHasAgency :: RelativeAgency -> RelativeAgency -> Type
data ReflNobodyHasAgency ra ra' where
     ReflNobodyHasAgency :: ReflNobodyHasAgency
                                NobodyHasAgency
                                NobodyHasAgency


-- | A proof that if both @Relative pr a@ and @Relative (FlipAgency pr) a@ are
-- equal then nobody has agency.  In particular this lemma excludes the
-- possibility that client and server has agency at the same state.
--
exclusionLemma_ClientAndServerHaveAgency
  :: forall (pr :: PeerRole) (a :: Agency)
            (ra  :: RelativeAgency).
     SingPeerRole pr
  -> ReflRelativeAgency a ra (Relative             pr  a)
  -- ^ evidence that `ra` is equal to `Relative pr a`, e.g. that client has
  -- agency
  -> ReflRelativeAgency a ra (Relative (FlipAgency pr) a)
  -- ^ evidence that `ra` is equal to `Relative (FlipAgency pr) a`, e.g. that
  -- the server has agency
  -> ReflNobodyHasAgency     (Relative             pr  a)
                             (Relative (FlipAgency pr) a)
  -- ^ derived evidence that nobody has agency in that case
exclusionLemma_ClientAndServerHaveAgency
  SingAsClient ReflNobodyAgency ReflNobodyAgency = ReflNobodyHasAgency
exclusionLemma_ClientAndServerHaveAgency
  SingAsServer ReflNobodyAgency ReflNobodyAgency = ReflNobodyHasAgency

exclusionLemma_ClientAndServerHaveAgency
  SingAsClient ReflClientAgency x        = case x of {}
exclusionLemma_ClientAndServerHaveAgency
  SingAsServer ReflClientAgency x        = case x of {}
exclusionLemma_ClientAndServerHaveAgency
  SingAsClient ReflServerAgency x        = case x of {}
exclusionLemma_ClientAndServerHaveAgency
  SingAsServer ReflServerAgency x        = case x of {}


-- | A proof that if one side has terminated, then the other side terminated as
-- well.
--
terminationLemma_1
  :: SingPeerRole pr
  -> ReflRelativeAgency a ra              (Relative             pr  a)
  -> ReflRelativeAgency a NobodyHasAgency (Relative (FlipAgency pr) a)
  -> ReflNobodyHasAgency                  (Relative             pr  a)
                                          (Relative (FlipAgency pr) a)
terminationLemma_1
  SingAsClient ReflNobodyAgency ReflNobodyAgency = ReflNobodyHasAgency
terminationLemma_1
  SingAsServer ReflNobodyAgency ReflNobodyAgency = ReflNobodyHasAgency
terminationLemma_1 SingAsClient ReflClientAgency x = case x of {}
terminationLemma_1 SingAsClient ReflServerAgency x = case x of {}
terminationLemma_1 SingAsServer ReflClientAgency x = case x of {}
terminationLemma_1 SingAsServer ReflServerAgency x = case x of {}


-- | Internal; only need to formulate auxiliary lemmas in the proof of
-- 'terminationLemma_2'.
--
type        FlipRelAgency :: RelativeAgency -> RelativeAgency
type family FlipRelAgency ra where
  FlipRelAgency WeHaveAgency    = TheyHaveAgency
  FlipRelAgency TheyHaveAgency  = WeHaveAgency
  FlipRelAgency NobodyHasAgency = NobodyHasAgency


-- | Similar to 'terminationLemma_1'.
--
-- Note: this could be proven the same way 'terminationLemma_1' is proved, but
-- instead we use two lemmas to reduce the assumptions (arguments) and we apply
-- 'terminationLemma_1'.
--
terminationLemma_2
  :: SingPeerRole pr
  -> ReflRelativeAgency a ra              (Relative (FlipAgency pr) a)
  -> ReflRelativeAgency a NobodyHasAgency (Relative             pr  a)
  -> ReflNobodyHasAgency                  (Relative (FlipAgency pr) a)
                                          (Relative             pr  a)

terminationLemma_2 singPeerRole refl refl' =
    case terminationLemma_1 singPeerRole
                       (lemma_flip  singPeerRole refl)
                       (lemma_flip' singPeerRole refl')
    of x@ReflNobodyHasAgency -> x
    -- note: if we'd swap arguments of the returned @ReflNobodyHasAgency@ type,
    -- we wouldn't need to pattern match on the result.  But in this form the
    -- lemma is a symmetric version of 'terminationLemma_1'.
  where
    lemma_flip
      :: SingPeerRole pr
      -> ReflRelativeAgency a                ra  (Relative (FlipAgency pr) a)
      -> ReflRelativeAgency a (FlipRelAgency ra) (Relative             pr  a)

    lemma_flip'
      :: SingPeerRole pr
      -> ReflRelativeAgency a                ra  (Relative             pr  a)
      -> ReflRelativeAgency a (FlipRelAgency ra) (Relative (FlipAgency pr) a)

    -- both lemmas are identity functions:
    lemma_flip  SingAsClient ReflClientAgency = ReflClientAgency
    lemma_flip  SingAsClient ReflServerAgency = ReflServerAgency
    lemma_flip  SingAsClient ReflNobodyAgency = ReflNobodyAgency
    lemma_flip  SingAsServer ReflClientAgency = ReflClientAgency
    lemma_flip  SingAsServer ReflServerAgency = ReflServerAgency
    lemma_flip  SingAsServer ReflNobodyAgency = ReflNobodyAgency

    lemma_flip' SingAsClient ReflClientAgency = ReflClientAgency
    lemma_flip' SingAsClient ReflServerAgency = ReflServerAgency
    lemma_flip' SingAsClient ReflNobodyAgency = ReflNobodyAgency
    lemma_flip' SingAsServer ReflClientAgency = ReflClientAgency
    lemma_flip' SingAsServer ReflServerAgency = ReflServerAgency
    lemma_flip' SingAsServer ReflNobodyAgency = ReflNobodyAgency
