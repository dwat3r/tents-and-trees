module Tests where

import qualified Data.Map as M
import qualified Data.Vector as V
import TentsAndTrees

data Puzzle = Puzzle {
    fields :: (M.Map (Int,Int) Field),
    horizontal :: (V.Vector Int),
    vertical :: (V.Vector Int)
  } deriving Show

example = Puzzle (M.fromList [
  ((0,0),Tent),
  ((0,1),Tree),
  ((0,2),Tent),
  ((1,0),Tree),
  ((1,4),Tent),
  ((2,1),Tent),
  ((2,2),Tree),
  ((2,4),Tree),
  ((3,3),Tent),
  ((3,4),Tree),
  ((4,1),Tent),
  ((4,2),Tree),
  ((4,4),Tree),
  ((5,4),Tent)
  ])
  (V.fromList [2,1,1,1,1,1])
  (V.fromList [1,2,1,1,2,0])



-- easy = 
--       println(TentsAndTreesPuzzle(
--         Array(
--           Array('t', ' ', ' '),
--           Array(' ', ' ', ' '),
--           Array(' ', ' ', 't')
--         ),
--         Array(0, 2, 0),
--         Array(1, 0, 1)
--       ).solve)
--     }

--     it("8x8 easy") {
--       println(TentsAndTreesPuzzle(
--         Array(
--           " t      ",
--           "   t  t ",
--           "   t   t",
--           "        ",
--           "    t t ",
--           "t  t   t",
--           "      t ",
--           "t       "
--         ).map(_.toCharArray),
--         Array(1, 3, 0, 1, 3, 1, 2, 1),
--         Array(1, 2, 1, 2, 0, 3, 0, 3)
--       ).solve)
--     }

--     it("20x20 hard") {
--       println(TentsAndTreesPuzzle(
--         Array(
--           " t   tt    tt   t   ",
--           "tt            t    t",
--           "     t  t         t ",
--           "   t    t tt       t",
--           " t    t       t  t  ",
--           "  t       t t t     ",
--           "          t   tt   t",
--           " t    t         t   ",
--           " t  t       t      t",
--           "    t  t  tt  t     ",
--           "             tt t   ",
--           "      tt          t ",
--           " t  t    t          ",
--           " t  t               ",
--           " t   t     t  t t   ",
--           " t     t t          ",
--           "         t  tt   t t",
--           " tt    t            ",
--           "     tt   t t t t  t",
--           "  t     t      t t  "
--         ).map(_.toCharArray),
--         Array(6, 4, 3, 6, 2, 6, 1, 5, 3, 4, 5, 3, 3, 4, 3, 4, 2, 7, 1, 8),
--         Array(4, 5, 2, 6, 2, 5, 4, 5, 2, 5, 4, 3, 5, 4, 5, 3, 3, 4, 3, 6)
--       ).solve)
--     }

--     it("TentsAndTrees C - 13x13") {
--       println(TentsAndTreesPuzzle(
--         Array(
--           "t          t ",
--           "t t t    tt  ",
--           "      t     t",
--           "         t t ",
--           "    t   t  t ",
--           " t t      t  ",
--           "        t  t ",
--           " t  t t     t",
--           "t            ",
--           "    t  tt    ",
--           "     t       ",
--           "    t  t t  t",
--           " t   t    t  "
--         ).map(_.toCharArray),
--         Array(2, 4, 2, 4, 2, 2, 4, 2, 2, 3, 2, 2, 4),
--         Array(2, 4, 0, 4, 2, 3, 2, 2, 4, 2, 4, 1, 5)
--       ).solve)
--     }

--     it("TentsAndTrees C - 13x13 - 2") {
--       println(TentsAndTreesPuzzle(
--         Array(
--           "             ",
--           "  t    t t  t",
--           "t     tt     ",
--           "   t t     t ",
--           "         t   ",
--           "t    t    t  ",
--           "  t       t  ",
--           "      t     t",
--           "t tt      t  ",
--           "      t    t ",
--           " t  t t  t   ",
--           "      t   tt ",
--           "  t t t      "
--         ).map(_.toCharArray),
--         Array(1, 5, 0, 5, 1, 3, 2, 3, 2, 3, 3, 2, 4),
--         Array(3, 2, 3, 3, 3, 2, 2, 3, 1, 3, 3, 2, 4)
--       ).solve)
--     }

--     it("TentsAndTrees C - 13x13 - 3") {
--       println(TentsAndTreesPuzzle(
--         Array(
--           " t  t        ",
--           "     t t    t",
--           "      t  t t ",
--           " t         t ",
--           " t  t  tt    ",
--           "    t     t t",
--           " t  tt       ",
--           "t     t     t",
--           "  tt  t  t   ",
--           "             ",
--           "        t   t",
--           "   t    t t  ",
--           " t   t       "
--         ).map(_.toCharArray),
--         Array(4, 2, 1, 5, 2, 4, 2, 4, 1, 3, 1, 2, 3),
--         Array(3, 2, 2, 4, 2, 2, 3, 3, 2, 4, 1, 1, 5)
--       ).solve)
--     }

--     it("TentsAndTrees C - 15x15") {
--       println(TentsAndTreesPuzzle(
--         Array(
--           "  t   t    t   ",
--           "t         t  tt",
--           "  t t  t  t t  ",
--           "      t     t t",
--           "               ",
--           "   t     t   t ",
--           " t             ",
--           " t    tt t     ",
--           "  t            ",
--           "       t      t",
--           "t       t t    ",
--           "  t      t    t",
--           "   tt t      t ",
--           " t t     t  tt ",
--           "      t t      "
--         ).map(_.toCharArray),
--         Array(6, 1, 4, 1, 3, 2, 3, 2, 4, 0, 5, 1, 4, 2, 5),
--         Array(4, 2, 3, 3, 1, 4, 1, 6, 0, 3, 3, 3, 2, 3, 5)
--       ).solve)
--     }

--   }
-- }