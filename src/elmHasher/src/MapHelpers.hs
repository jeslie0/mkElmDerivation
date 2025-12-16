{-# LANGUAGE ImportQualifiedPost #-}

module MapHelpers where

import Control.Monad
import Data.HashMap.Strict qualified as M
import Data.Hashable
import Data.Vector qualified as V
import Types

-- | Inserts the Elm package information and hash into the given map.
addHashBundleToMap ::
  -- | Elm Package
  ElmPackage ->
  -- | Hash with archive and docs hash
  HashBundle ->
  -- | Map to update
  M.HashMap Name (M.HashMap Version HashBundle) ->
  M.HashMap Name (M.HashMap Version HashBundle)
addHashBundleToMap (ElmPackage name ver) hashBundle =
  M.insertWith
    (\_ -> M.insert ver hashBundle)
    name
    (M.singleton ver hashBundle)

-- | Filters the keys and values in the map by terms of the second
-- that don't appear.
-- >>> extractNewPackages [(1, [1,2,3])] [(1,[(2,3)])]
-- fromList [(1,[1,3])]
extractNewPackages ::
  (Hashable a, Hashable b) =>
  M.HashMap a (V.Vector b) ->
  M.HashMap a (M.HashMap b c) ->
  M.HashMap a (V.Vector b)
extractNewPackages =
  M.differenceWith
    ( \vers verHashMap ->
        let newVers = do
              v <- vers
              guard $ not (v `M.member` verHashMap)
              return v
         in if null newVers then Nothing else Just newVers
    )

-- | Removes terms from the first map that are also in the second
-- list.
-- >>> removeFailedPkgs [(1, [1,2,3]), (2, [3])]   [(1, [1]), (2, [3])]
-- fromList [(1,[2,3])]
removeFailedPkgs ::
  (Hashable a, Eq b) =>
  -- | Map of failures.
  M.HashMap a (V.Vector b) ->
  -- | Map of things to hash.
  M.HashMap a (V.Vector b) ->
  M.HashMap a (V.Vector b)
removeFailedPkgs =
  M.differenceWith $
    \vers badVers ->
      if vers == badVers
        then Nothing
        else Just $ vers \\ badVers

-- | Combines the two maps with both unions and keys, favouring the
-- keys on the left.
-- >>> joinNewPackages [(1, [(2, 3), (0, 4)])] [(1, [(5, 6), (2, 9)])]
-- fromList [(1,fromList [(0,4),(2,3),(5,6)])]
joinNewPackages ::
  (Hashable a, Hashable b) =>
  -- | First map.
  M.HashMap a (M.HashMap b c) ->
  -- | Second map.
  M.HashMap a (M.HashMap b c) ->
  M.HashMap a (M.HashMap b c)
joinNewPackages =
  M.unionWith M.union

-- | Generates a new map that is the union of the given two, whose
-- values are combined.
-- >>> updateFailures [(1,[2])] [(1, [1]), (2, [3] :: [Int])]
-- fromList [(1,[2,1]),(2,[3])]
updateFailures ::
  (Hashable a, Monoid b) =>
  -- | Map of failures
  M.HashMap a b ->
  -- | Map of new failures to hash
  M.HashMap a b ->
  M.HashMap a b
updateFailures =
  M.unionWith $
    \badVers vers -> badVers <> vers

-- | A Vector based difference function.
-- >>> V.fromList [1,2,3] \\ (V.fromList [2,3])
-- [1]
(\\) :: (Eq a) => V.Vector a -> V.Vector a -> V.Vector a
(\\) = V.foldl' (\as b -> V.filter (b /=) as)

-- | Map a monadic action (which uses both keys and values) over a
-- map, keeping the result.
mapMWithKey :: (Monad m) => (k -> a -> m b) -> M.HashMap k a -> m (M.HashMap k b)
mapMWithKey f kvMap = sequence $ M.mapWithKey f kvMap

-- | Map a monadic action (which uses both keys and values) over a
-- map, for sideffects only.
mapMWithKey_ :: (Monad m) => (k -> a -> m b) -> M.HashMap k a -> m ()
mapMWithKey_ f kvMap = sequence_ $ M.mapWithKey f kvMap
