module Sudoku
    ( solve
    , display
    , GridValues
    ) where

import qualified Data.Maybe as Maybe
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad

cross :: String -> String -> [String]
cross a b = [ x : y : [] | x <- a, y <- b ]

rows   = "ABCDEFGHIJKLMNOP"
cols   = "ABCDEFGHIJKLMNOP"

type Square = String
type Digit = Char

--each square in the puzzle
squares :: [Square]
squares = cross rows cols


sectionlist :: [[Square]]
sectionlist =
  [ cross rows (c:[]) | c <- cols ] ++
  [ cross (r:[]) cols | r <- rows ] ++
  [ cross rs cs | rs <- ["ABCD", "EFGH", "IJKL", "MNOP"], cs <- ["ABCD", "EFGH", "IJKL", "MNOP"] ]

--map from each square to the sections it is a part of
squareSectionMap :: Map Square [[Square]]
squareSectionMap = toMap [(s, u) | s <- squares, u <- sectionlist, elem s u]

toMap :: [(Square, [Square])] -> Map Square [[Square]]
toMap xs = foldl addToMap Map.empty xs where
  addToMap m (k, ys) = case Map.lookup k m of
    Just zs -> Map.insert k (ys : zs) m
    Nothing -> Map.insert k [ys] m

-- map from a square to other squares in all section it belongs to, each square has 39 such other squares

otherSquares :: Map Square (Set Square)
otherSquares = Map.mapWithKey f squareSectionMap where
  f k xss = Set.fromList $ filter (\x -> x /= k)(concat xss)


type Grid = String
type GridValues = Map Square String -- puzzle solved when a square has only one possible value

-- convert input to a gridvalues structure
parseGrid :: Grid -> Maybe GridValues
parseGrid grid = do
  let zero = Just (Map.fromList [(s, cols) | s <- squares])
  xs <- gridValues grid
  let gridValues' = filter (\(_, c) -> c `elem` cols) xs
  foldl (\acc pair -> acc >>= (\m -> assign m pair)) zero gridValues'

gridValues :: Grid -> Maybe ([(Square, Char)])
gridValues grid =
  let validChars = '0' : '.' : cols -- use 0 or . for unfilled squares
      chars = [ c | c <- grid, elem c validChars ] in
    if length chars /= 256 then Nothing
    else Just (zip squares chars)

-- Constraint propagation
-- 1. If a square has only one possible value, then eliminate that value from the square's otherSquares.
-- 2. If a unit has only one possible place for a value, then put the value there.
assign :: GridValues -> (Square, Digit) -> Maybe GridValues
assign values (s, d) = do
  let otherValues = List.delete d (Maybe.fromMaybe "" (Map.lookup s values))
  foldl eliminate' (Just values) otherValues where
    eliminate' mValues d2 = mValues >>= (\values' -> eliminate values' (s, d2))

-- use eliminate to cut down on possibel options for values based on other square values
eliminate :: GridValues -> (Square, Digit) -> Maybe GridValues
eliminate vs (s, d) =
  case Map.lookup s vs of

    Nothing -> Nothing

    Just ds ->

      case List.elemIndex d ds of
        Nothing -> Just vs
        Just _  -> do

          let ds' = List.delete d ds
          vs' <- if length ds' == 0 then Nothing
                 else Just $ Map.insert s ds' vs

          vs'' <- if length ds' == 1
                  then
                    let d2 = head ds'
                        ss = Maybe.fromMaybe Set.empty (Map.lookup s otherSquares) in

                      foldl (\mValues s2 -> do
                        values <- mValues
                        eliminate values (s2, d2)) (Just vs') ss
                  else Just vs'

          uss   <- Map.lookup s squareSectionMap
          vs''' <- foldl
            (\acc us -> do
                acc' <- acc
                let dplaces = [s | s <- us, d `elem` (Maybe.fromMaybe [] $ Map.lookup s acc')] in
                  case length dplaces of
                    0 -> Nothing -- Contradiction: no place for this value
                    1 -> assign acc' ((head dplaces), d)
                    _ -> acc) (Just vs'') uss
          return vs'''

-- Display these values as 2D grid.
display :: GridValues -> IO ()
display values = do
  let width = 1 + maximum [ Maybe.fromMaybe 0 $
                            fmap length $
                            Map.lookup s values | s <- squares]
      line  = List.intercalate "+" $ replicate 4 $ replicate (width * 4) '-'
      table = [ buildCell r c | r <- rows, c <- cols] where
        buildCell r c =
          let vs  = Maybe.fromMaybe "" $ Map.lookup (r : c : []) values
              pre = replicate (width - length vs) ' '
              pos = if c `elem` "DHL" then " |" else if c == 'P' then "\n" else ""
              ln  = if r `elem` "DHL" && c == 'P' then (line ++ "\n") else "" in
            pre ++ vs ++ pos ++ ln
  putStrLn $ List.concat table

search :: GridValues -> Maybe GridValues
search values =
  let xs            = Map.toList values
      allSizeOne    = List.all (\(_, vs) -> length vs == 1) xs
      -- Choose the infilled square s with the fewest possibilities
      ys            = filter (\(_, ds) -> length ds > 1) xs
      (s, ds)       = List.minimumBy (\(s1, v1) (s2, v2) ->
                                        compare (length v1) (length v2)) ys
      assignments   = fmap (\d -> assign values (s, d) >>= search) ds in
    if allSizeOne then Just values -- Solved
    else join $ List.find (Maybe.isJust) assignments

solve :: Grid -> Maybe GridValues
solve grid = parseGrid grid >>= search
