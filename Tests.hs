module Tests where

import qualified Data.Map as M
import qualified Data.Vector as V
import TentsAndTrees

puzzle tree horizontal vertical = 
  Puzzle 
    (M.fromList $ zip tree $ repeat Tree)
    (M.fromList $ zip [0..] horizontal)
    (M.fromList $ zip [0..] vertical)

first = puzzle [(1,0),(4,1),(1,2),(3,2),(4,3)] [2,0,1,0,2] [2,0,2,0,1]

firstTents = [(0,0),(0,2),(2,2),(4,0),(4,4)]

second = puzzle [(0,1),(1,0),(2,2),(2,4),(3,4),(4,2),(4,4)] [2,1,1,1,1,1] [1,2,1,1,2,0]

secondTents = [(0,0),(0,2),(1,4),(2,1),(3,3),(4,1),(5,4)]

easy = puzzle [(0,0),(2,2)] [0,2,0] [1,0,1]
      
toPuzzle chars = concatMap (\(y, cs) -> map (\(x, _) -> (x,y)) $ filter (\(x, c) -> c == 't') $ zip [0..] cs) $ zip [0..] chars


easy8x8 = puzzle (toPuzzle 
    [
      " t      ",
      "   t  t ",
      "   t   t",
      "        ",
      "    t t ",
      "t  t   t",
      "      t ",
      "t       "
    ])
    [1, 3, 0, 1, 3, 1, 2, 1]
    [1, 2, 1, 2, 0, 3, 0, 3]
      
hard20x20 = puzzle (toPuzzle
  [
    " t   tt    tt   t   ",
    "tt            t    t",
    "     t  t         t ",
    "   t    t tt       t",
    " t    t       t  t  ",
    "  t       t t t     ",
    "          t   tt   t",
    " t    t         t   ",
    " t  t       t      t",
    "    t  t  tt  t     ",
    "             tt t   ",
    "      tt          t ",
    " t  t    t          ",
    " t  t               ",
    " t   t     t  t t   ",
    " t     t t          ",
    "         t  tt   t t",
    " tt    t            ",
    "     tt   t t t t  t",
      "  t     t      t t  "
  ])
  [6, 4, 3, 6, 2, 6, 1, 5, 3, 4, 5, 3, 3, 4, 3, 4, 2, 7, 1, 8]
  [4, 5, 2, 6, 2, 5, 4, 5, 2, 5, 4, 3, 5, 4, 5, 3, 3, 4, 3, 6]

-- "TentsAndTrees C - 13x13"
tntC13x13 = puzzle (toPuzzle
  [
    "t          t ",
    "t t t    tt  ",
    "      t     t",
    "         t t ",
    "    t   t  t ",
    " t t      t  ",
    "        t  t ",
    " t  t t     t",
    "t            ",
    "    t  tt    ",
    "     t       ",
    "    t  t t  t",
    " t   t    t  "
  ])
  [2, 4, 2, 4, 2, 2, 4, 2, 2, 3, 2, 2, 4]
  [2, 4, 0, 4, 2, 3, 2, 2, 4, 2, 4, 1, 5]

-- "TentsAndTrees C - 13x13 - 2"
tntC13x13n2 = puzzle (toPuzzle
  [
    "             ",
    "  t    t t  t",
    "t     tt     ",
    "   t t     t ",
    "         t   ",
    "t    t    t  ",
    "  t       t  ",
    "      t     t",
    "t tt      t  ",
    "      t    t ",
    " t  t t  t   ",
    "      t   tt ",
    "  t t t      "
  ])
  [1, 5, 0, 5, 1, 3, 2, 3, 2, 3, 3, 2, 4]
  [3, 2, 3, 3, 3, 2, 2, 3, 1, 3, 3, 2, 4]
   
-- "TentsAndTrees C - 13x13 - 3"
tntC13x13n3 = puzzle (toPuzzle
  [
    " t  t        ",
    "     t t    t",
    "      t  t t ",
    " t         t ",
    " t  t  tt    ",
    "    t     t t",
    " t  tt       ",
    "t     t     t",
    "  tt  t  t   ",
    "             ",
    "        t   t",
    "   t    t t  ",
    " t   t       "
  ])
  [4, 2, 1, 5, 2, 4, 2, 4, 1, 3, 1, 2, 3]
  [3, 2, 2, 4, 2, 2, 3, 3, 2, 4, 1, 1, 5]

-- "TentsAndTrees C - 15x15"
tntC15x15 = puzzle (toPuzzle 
  [
    "  t   t    t   ",
    "t         t  tt",
    "  t t  t  t t  ",
    "      t     t t",
    "               ",
    "   t     t   t ",
    " t             ",
    " t    tt t     ",
    "  t            ",
    "       t      t",
    "t       t t    ",
    "  t      t    t",
    "   tt t      t ",
    " t t     t  tt ",
    "      t t      "
  ])
  [6, 1, 4, 1, 3, 2, 3, 2, 4, 0, 5, 1, 4, 2, 5]
  [4, 2, 3, 3, 1, 4, 1, 6, 0, 3, 3, 3, 2, 3, 5]


tests = undefined