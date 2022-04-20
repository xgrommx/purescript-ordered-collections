module Bench.Data.Map where

import Prelude

import Data.FoldableWithIndex (foldlWithIndex)
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Performance.Minibench (bench, benchWith)

data These a b = This a | That b | Both a b

mergeMapWithKey :: forall k a b c. Ord k => (k -> c -> These a b -> c) -> c -> M.Map k a -> M.Map k b -> c
mergeMapWithKey f init xs ys = do
  let
    Tuple acc leftovers =
      foldlWithIndex
        ( \k (Tuple a ls) b ->
            case M.pop k ls of
              Just (Tuple l ls') ->
                Tuple (f k a (Both b l)) ls'
              Nothing ->
                Tuple (f k a (This b)) ls
        )
        (Tuple init ys)
        xs
  foldlWithIndex
    (\k a b -> f k a (That b))
    acc
    leftovers

unionWithKey :: forall k c. Ord k => (k -> c -> c -> c) -> M.Map k c -> M.Map k c -> M.Map k c
unionWithKey f = mergeMapWithKey fn M.empty
  where
    fn k m t = M.insert k (go t) m
      where
        go = case _ of
          This x -> x
          That x -> x
          Both x y -> f k x y

unionWith :: forall k v. Ord k => (v -> v -> v) -> M.Map k v -> M.Map k v -> M.Map k v
unionWith f m1 m2 = foldlWithIndex go m1 m2
  where
  go k m v = M.alter (Just <<< maybe v (f v)) k m

benchMap :: Effect Unit
benchMap = do
  log "size"
  log "---------------"
  benchSize

  log ""

  log "fromFoldable"
  log "------------"
  benchFromFoldable

  log "union"
  log "---------------"
  benchUnion

  log ""

  where

  benchUnion = do
    let nats = L.range 0 9999
        natPairs = (flip Tuple) unit <$> nats
        bigMap = M.fromFoldable $ natPairs
    
    log $ "union: big map (" <> show (M.size bigMap) <> ")"
    bench \_ -> M.union bigMap bigMap

    log $ "unionWithKey: big map (" <> show (M.size bigMap) <> ")"
    bench \_ -> unionWithKey (\k v1 v2 -> v2) bigMap bigMap

    log $ "unionWith: big map (" <> show (M.size bigMap) <> ")"
    bench \_ -> unionWith (\v1 v2 -> v2) bigMap bigMap

  benchSize = do
    let nats = L.range 0 999999
        natPairs = (flip Tuple) unit <$> nats
        singletonMap = M.singleton 0 unit
        smallMap = M.fromFoldable $ L.take 100 natPairs
        midMap = M.fromFoldable $ L.take 10000 natPairs
        bigMap = M.fromFoldable $ natPairs

    log "size: singleton map"
    bench \_ -> M.size singletonMap

    log $ "size: small map (" <> show (M.size smallMap) <> ")"
    bench \_ -> M.size smallMap

    log $ "size: midsize map (" <> show (M.size midMap) <> ")"
    benchWith 100 \_ -> M.size midMap

    log $ "size: big map (" <> show (M.size bigMap) <> ")"
    benchWith 10  \_ -> M.size bigMap

  benchFromFoldable = do
    let natStrs = show <$> L.range 0 99999
        natPairs = (flip Tuple) unit <$> natStrs
        shortPairList = L.take 10000 natPairs

    log $ "fromFoldable (" <> show (L.length shortPairList) <> ")"
    benchWith 100 \_ -> M.fromFoldable shortPairList

    log $ "fromFoldable (" <> show (L.length natPairs) <> ")"
    benchWith 10 \_ -> M.fromFoldable natPairs
