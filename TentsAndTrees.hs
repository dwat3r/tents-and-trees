{-# LANGUAGE TemplateHaskell, RecordWildCards #-}
module TentsAndTrees where

import qualified Data.Map as M
import qualified Data.Vector as V
import Data.List
import Data.Function
import Debug.Trace
import Control.Lens

data Field = Grass | Tent | Tree deriving (Eq, Show)

data Puzzle a = Puzzle {
    _fields :: (M.Map (Int,Int) a),
    _horizontal :: (M.Map Int Int),
    _vertical :: (M.Map Int Int)
  } deriving Show

makeLenses ''Puzzle

solve puzzle = 
      step [] tnts
    where
      step ts trs | length (exactTents trs) > 0 = step (ts ++ exactTents trs) $ reducePuzzle trs $ exactTents trs
                  | otherwise = (ts, trs)
      tnts =   treesAndTents puzzle $
                filter (not . null . (neighTrees puzzle)) $ 
                filter (\(x,y) -> (y `M.notMember` (M.filter (==0) $ _vertical puzzle)) &&
                                  (x `M.notMember` (M.filter (==0) $ _horizontal puzzle))) $
                filter (\t -> M.notMember t $ _fields puzzle) $
                (,) <$> [0..size] <*> [0..size]
      size = M.size (_vertical puzzle) - 1


neighTrees Puzzle{..} (x,y) = filter (`M.member` _fields) [(x-1,y),(x,y-1), (x+1,y), (x,y+1)]

treesAndTents p ts = over fields (M.mapWithKey (\tr _ -> (neighTentsOfTree ts tr))) p
  where
    neighTentsOfTree tents (x,y) = filter (`elem` tents)  [(x-1,y),(x,y-1), (x+1,y), (x,y+1)]

maximumm = M.foldlWithKey (\(maxInd, maxVal) i n -> if maxVal < n then (i, n) else (maxInd, maxVal)) (-1,0)

reducePuzzle Puzzle{..} ts = Puzzle (M.map (filter (\t -> t `notElem` ts && validTree t)) _fields) reducedHorizontal reducedVertical
  where
    validTree (x,y)   = _horizontal M.! x > 0 && _vertical M.! y > 0
    reducedHorizontal = M.mapWithKey (\x n -> n - (length $ filter ((==x) . fst) ts)) _horizontal
    reducedVertical   = M.mapWithKey (\y n -> n - (length $ filter ((==y) . snd) ts)) _vertical

exactTents = concat . filter ((==1) . length) . M.elems . _fields
