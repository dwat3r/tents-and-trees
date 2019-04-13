{-# LANGUAGE RecordWildCards #-}
module TentsAndTrees where

import qualified Data.Map as M
import qualified Data.Vector as V
import Debug.Trace

data Field = Grass | Tent | Tree deriving (Eq, Show)

data Puzzle = Puzzle {
    fields :: (M.Map (Int,Int) Field),
    horizontal :: (M.Map Int Int),
    vertical :: (M.Map Int Int)
  } deriving Show

solve puzzle = 
  reduceProblem puzzle [] $
  --traceShowId $
  removeOrphanTents puzzle $ 
  filter (\t -> snd t `M.notMember` (M.filter (==0) $ vertical puzzle)) $
  filter (\t -> fst t `M.notMember` (M.filter (==0) $ horizontal puzzle)) $
  M.keys $
  M.differenceWith (\_ _ -> Nothing)
    (szigetFest ((M.size $ horizontal puzzle) - 1))
    (fields puzzle)
    where
      szigetFest size = M.fromList $ zip ((,) <$> [0..size] <*> [0..size]) $ repeat Tent
      removeOrphanTents p = filter (not . null . (neighTrees p))
      removeInvalidTents ts1 ts = filter (not . hasNeighTents ts1) ts
      reduceProblem _ vts [] = reverse vts
      reduceProblem p@Puzzle{..} vts ((x,y):ts)
          | length (neighTrees p (x,y)) > 0 &&
            horizontal M.! x > 0 &&
            vertical M.! y > 0 = traceShow ts $ (reduceProblem reduced ((x,y):vts) $ 
                                          removeInvalidTents ((x,y):vts) $
                                          removeOrphanTents reduced ts)
          | horizontal M.! x == 0 ||
            vertical M.! y == 0 = reduceProblem p vts ts
          | otherwise = error "?? problem in code"
              where 
                reduced = Puzzle 
                    (M.delete (head $ neighTrees p (x,y)) fields)
                    (M.adjust (subtract 1) x horizontal)
                    (M.adjust (subtract 1) y vertical)


neighTrees Puzzle{..} (x,y) = filter (`M.member` fields) [(x-1,y),(x,y-1), (x+1,y), (x,y+1)]
hasNeighTents tents (x,y) = any (`elem` tents) [
                                                  (x-1,y-1),(x-1,y),(x-1,y+1),
                                                  (x,y-1), (x,y+1),
                                                  (x+1,y-1),(x+1,y),(x+1,y+1)
                                               ]