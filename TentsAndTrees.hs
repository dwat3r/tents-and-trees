{-# LANGUAGE RecordWildCards #-}
module TentsAndTrees where

import qualified Data.Map as M
import qualified Data.Vector as V
import Data.List
import Data.Function
import Debug.Trace

data Field = Grass | Tent | Tree deriving (Eq, Show)

data Puzzle = Puzzle {
    fields :: (M.Map (Int,Int) Field),
    horizontal :: (M.Map Int Int),
    vertical :: (M.Map Int Int)
  } deriving Show

solve puzzle = 
  -- reduceProblem (puzzle, [], []) $
  -- filter (isSolution puzzle) $
  -- traceShowId $
--  genSols $
  treesAndTents puzzle $
  filter (not . null . (neighTrees puzzle)) $ 
  filter (\(x,y) -> (y `M.notMember` (M.filter (==0) $ vertical puzzle)) &&
                    (x `M.notMember` (M.filter (==0) $ horizontal puzzle))) $
  M.keys $
  M.differenceWith (\_ _ -> Nothing)
    (szigetFest ((M.size $ horizontal puzzle) - 1))
    (fields puzzle)
    where
      szigetFest size = M.fromList $ zip ((,) <$> [0..size] <*> [0..size]) $ repeat Tent

-- constraint checking on permutations
-- isSolution p@Puzzle{..} ts =  
--                               (and $ map (\(x, n) -> (n == (length $ filter (==x) $ map (fst . snd) ts))) $ M.toList horizontal)  &&
--                               (and $ map (\(y, n) -> (n == (length $ filter (==y) $ map (snd . snd) ts))) $ M.toList vertical)

-- backtracking

      


      reduceProblem (p, gts, bts) [] = Just gts
      reduceProblem s@(p, gts, bts) ((tr,[]):tts) = Nothing
      reduceProblem s@(p, gts, bts) trs@((tr,(t:ts)):tts)
        | t `elem` bts = Nothing
        | otherwise    = case placeTent s (tr, t) of
          Just rs@(rp, _, _) -> case reduceProblem rs $ removeInvalidTents t $ removeOrphanTents rp tts of
            Just gts -> Just gts
            Nothing -> traceShow rs $ Nothing  -- reduceProblem (p, gts, t:bts) ((tr,ts):tts)
          Nothing -> reduceProblem s ((tr,ts):tts)

removeOrphanTents p  = map (\(tr,ts) -> (tr, filter (not . null . neighTrees p) ts))
removeInvalidTents t = map (\(tr,ts) -> (tr, filter (not . isNeighTent t) ts))

placeTent (p@Puzzle{..}, gts, bts) (tr,te@(x,y))
  | horizontal M.! x > 0 && 
    vertical M.! y > 0 = Just (Puzzle 
              (M.delete tr fields)
              (M.adjust (subtract 1) x horizontal)
              (M.adjust (subtract 1) y vertical),
               te:gts,
               bts)
  | otherwise = Nothing

neighTrees Puzzle{..} (x,y) = filter (`M.member` fields) [(x-1,y),(x,y-1), (x+1,y), (x,y+1)]

isNeighTent (x,y) nt = any (== nt) [
                                    (x-1,y-1),(x-1,y),(x-1,y+1),
                                    (x,y-1), (x,y+1),
                                    (x+1,y-1),(x+1,y),(x+1,y+1)
                                   ] 


-- do a decision tree of this
-- make a partition of rows and columns of available tents, and if they can be placed,
-- then reduce the problem and try another row/column.
-- if we meet a contradiction, backtrack by trying out another field
-- treesAndTents Puzzle{..} ts = sortBy (compare `on` (length . snd)) $ M.toList $ M.mapWithKey (\tr _ -> (neighTentsOfTree ts tr)) fields
treesAndTents Puzzle{..} ts = M.mapWithKey (\tr _ -> (neighTentsOfTree ts tr)) fields
  where
    neighTentsOfTree tents (x,y) = filter (`elem` tents)  [(x-1,y),(x,y-1), (x+1,y), (x,y+1)]

columns Puzzle{..} m = M.mapWithKey (\x n -> (n, M.filter (not . null) $ M.map (filter ((==x) . fst)) m)) horizontal
rows    Puzzle{..} m = M.mapWithKey (\y n -> (n, M.filter (not . null) $ M.map (filter ((==y) . snd)) m)) vertical






genSols = foldr (\x acc -> (:) <$> x <*> acc) [[]] . map (\(tr, ts) -> (,) <$> [tr] <*> ts)
