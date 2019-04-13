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
  --M.union (fields puzzle) $
  reduceProblem puzzle $
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
      reduceProblem _ [] = []
      reduceProblem p@Puzzle{..} ((x,y):ts)
          | length (neighTrees p (x,y)) > 0 &&
            horizontal M.! x > 0 &&
            vertical M.! y > 0 = (x,y):(reduceProblem reduced $ removeOrphanTents reduced ts)
          | horizontal M.! x == 0 ||
            vertical M.! y == 0 = reduceProblem p ts
          | otherwise = error "?? problem in code"
              where 
                reduced = Puzzle 
                    (M.delete (head $ neighTrees p (x,y)) fields)
                    (M.adjust (subtract 1) x horizontal)
                    (M.adjust (subtract 1) y vertical)


neighTrees Puzzle{..} (x,y) = filter (`M.member` fields) [(x-1,y),(x,y-1), (x+1,y), (x,y+1)]