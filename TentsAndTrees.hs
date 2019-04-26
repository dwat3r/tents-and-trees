{-# LANGUAGE TemplateHaskell, RecordWildCards #-}
module TentsAndTrees where

import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Set as S
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
      tnts =   treesAndTents puzzle $
                filter (not . null . (neighTrees puzzle)) $ 
                filter (\(x,y) -> (y `M.notMember` (M.filter (==0) $ _vertical puzzle)) &&
                                  (x `M.notMember` (M.filter (==0) $ _horizontal puzzle))) $
                filter (\t -> M.notMember t $ _fields puzzle) $
                (,) <$> [0..size] <*> [0..size]
      size = M.size (_vertical puzzle) - 1

      step ts trs
                  | length et_trs > 0  = trace ("step tree " ++ show (ts, trs)) $ reducePuzzle trs et_trs >>= step (ts ++ et_trs)
                  | length exy_trs > 0 = trace ("step xy "   ++ show (ts, trs)) $ reducePuzzle trs exy_trs >>= step (ts ++ exy_trs)
                  | otherwise = Just (ts, trs)
        where
          et_trs = filter (validTent trs ts) $ exactTentsByTree trs
          exy_trs = filter (validTent trs ts) $ exactTentsByXY trs



neighTrees Puzzle{..} (x,y) = filter (`M.member` _fields) [(x-1,y),(x,y-1), (x+1,y), (x,y+1)]

hasNoNeighTent (x,y) ts = all (`notElem` ts) $ (,) <$> [x-1..x+1] <*> [y-1..y+1]

treesAndTents p ts = over fields (M.mapWithKey (\tr _ -> (neighTentsOfTree ts tr))) p
  where
    neighTentsOfTree tents (x,y) = filter (`elem` tents)  [(x-1,y),(x,y-1), (x+1,y), (x,y+1)]

maximumm = M.foldlWithKey (\(maxInd, maxVal) i n -> if maxVal < n then (i, n) else (maxInd, maxVal)) (-1,0)

validTent p@Puzzle{..} ts (x,y) =  _horizontal M.! x > 0 &&
                                   _vertical M.! y > 0 &&
                                   (not $ null $ neighTrees p (x,y)) &&
                                   hasNoNeighTent (x,y) ts

reducePuzzle :: Puzzle [(Int, Int)] -> [(Int, Int)] -> Maybe (Puzzle [(Int, Int)])
reducePuzzle p@Puzzle{..} ts = trace ("reducing " ++ (show (ts, p))) $ Puzzle <$> reducedTents <*> reducedHorizontal <*> reducedVertical
  where
    reducedHorizontal = validXY $ M.mapWithKey (\x n -> n - (length $ filter ((==x) . fst) ts)) _horizontal
    reducedVertical   = validXY $ M.mapWithKey (\y n -> n - (length $ filter ((==y) . snd) ts)) _vertical
    validXY xy = if M.null $ M.filter (<0) xy then Just xy else Nothing
    reducedTents = M.foldlWithKey (\m tr trts -> M.insert tr <$> reduceTent trts <*> m) (Just M.empty) _fields
    reduceTent [] = Just []
    reduceTent [t] | t `elem` ts = trace ("kept []") $ Just []
    reduceTent trts = go trts [] []

      where
        mults = traceShowId $ foldl (\m t -> M.insert t (length $ concat $ M.elems $ M.filter (t `elem`) _fields) m) M.empty ts
        -- to remove : valids removed : valids left
        go [] [] [] = Nothing -- no removed tents are belonging to the solution
        go [] vts gts = trace ("kept " ++ show gts) $ Just gts
        go (t:rts) vts gts | t `elem` ts && mults M.! t == 1    = trace ("remove sol " ++ show (t:rts, vts, gts)) $ go [] (t:vts) [] -- empty other possible tents of tree iff it only belongs to this tree and it's not the only one for this
                           | t `notElem` ts && validTent p ts t = trace ("keep rem "   ++ show (t:rts, vts, gts)) $ go rts vts (t:gts)
                           | otherwise                          = trace ("remove inv " ++ show (t:rts, vts, gts)) $ go rts vts gts


exactTentsByTree p@Puzzle{..} = concat $ filter ((==1) . length) $ M.elems _fields

exactTentsByXY p@Puzzle{..} = (\(xm, ym) -> S.toList $ foldl S.union S.empty $
                                           (M.elems $ M.filterWithKey (\x ts -> length ts == _horizontal M.! x) xm) ++
                                           (M.elems $ M.filterWithKey (\y ts -> length ts == _vertical   M.! y) ym)) $
                     foldl (\(xm, ym) (x,y) -> (M.insertWith (S.union) x (S.singleton (x,y)) xm,
                                                M.insertWith (S.union) y (S.singleton (x,y)) ym))
                    (M.empty,M.empty) $
                    concat $ M.elems _fields
