import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Control.Monad

cross :: String -> String -> [String]
cross a b = [ x : y : [] | x <- a, y <- b ]

-- rows/cols are numbered by letters A-P
rows = "ABCDEFGHIJKLMNOP"

-- allowed square values
validVals = [1..16]

type Square = String
type Grid = String

-- map of a square to current list of possible values
type GridValues = Map Square [Int]

-- generate list of squares by taking all possible combnation of row letters
squares = cross rows rows

-- each section corresponds to a row/ a column/ or 4x4 subgrid 
sectionList :: [[Square]]
sectionList =
    [ cross rows (r:[]) | r <- rows ] ++
    [ cross (r:[]) rows | r <- rows ] ++
    [ cross rs cs | rs <- ["ABCD", "EFGH", "IJKL", "MNOP" ], cs <- [ "ABCD", "EFGH", "IJKL", "MNOP" ] ]

-- map of a square to the sections that contain it ie the row column and 4x4 subgrid
squareToSectionMap :: Map Square [[Square]]
squareToSectionMap = toMap [ (s, se) | s <- squares, se <- sectionList, elem s se ]

-- create a map from a list of tuples where first element is k and second element is value
-- when key already exists, concat to list of vals
toMap :: [(Square, [Square])] -> Map Square [[Square]]
toMap xs = foldl addToMap Map.empty xs where
    addToMap m (k, v) = case Map.lookup k m of
        Just zs -> Map.insert k (v : zs) m
        Nothing -> Map.insert k [v] m

-- map of square to a set of all the squares in its section that are not it 
otherSquares :: Map Square (Set Square)
otherSquares = Map.mapWithKey f squareToSectionMap where
    f k xss = Set.fromList $ filter (\x -> x /= k)(concat xss)

-- convert input to a gridvalues structure
parseGrid :: Grid -> Maybe GridValues
parseGrid grid = do
    let zero = Just (Map.fromList [(s, validVals) | s <- squares])
    xs <- gridValues grid
    let gridValues' = filter (\(_, c) -> c `elem` validVals) xs
    foldl (\acc pair -> acc >>= (\m -> assign m pair)) zero gridValues'

-- create a grid from list of square values passed in
gridValues :: Grid -> Maybe ([(Square, Int)])
gridValues grid =
    -- use 0 for unfilled square
    let vals = [ read c::Int | c <- words grid ]
        digits = filter (\c -> elem c (0:validVals)) vals in
    if length digits /= 256 then Nothing
    else Just (zip squares digits)

-- assign a value to a square, and propagate constraints to other squares in the section
assign :: GridValues -> (Square, Int) -> Maybe GridValues
assign mvals (sq,val) = do
    let othervals = List.delete val (Maybe.fromMaybe [] (Map.lookup sq mvals))
    foldl eliminate' (Just mvals) othervals where
        eliminate' mvals val = mvals >>= (\mvals' -> eliminate mvals' (sq,val))

 
 -- use eliminate to cut down on possibel options for values based on other square values
eliminate :: GridValues -> (Square, Int) -> Maybe GridValues
eliminate vs (s, d) =    
    case Map.lookup s vs of
        Nothing -> Nothing -- should not happen
        Just ds ->
            case List.elemIndex d ds of
                Nothing -> Just vs -- if value already eliminated, skip
                Just _  -> do
                    let ds' = List.delete d ds -- delete value
                    vs' <- if length ds' == 0 then Nothing -- no possible values left nothing to do
                        else Just $ Map.insert s ds' vs -- update map
                    
                    -- if possible values reduce to one, eliminate that value recursively for 
                    -- other squares in this square's sections
                    vs'' <- if length ds' == 1
                        then
                            let d2 = head ds'
                                ss = Maybe.fromMaybe Set.empty (Map.lookup s otherSquares) in
                                    foldl (\mValues s2 -> do
                                        values <- mValues
                                        eliminate values (s2, d2)) (Just vs') ss
                        else Just vs'
                    
                    -- check if any section has only one possible value left for a square after elim
                    -- if so assign that value and propagate constraints recursively
                    uss <- Map.lookup s squareToSectionMap
                    vs''' <- foldl
                        (\acc us -> do
                            acc' <- acc
                            let dplaces = [s | s <- us, d `elem` (Maybe.fromMaybe [] $ Map.lookup s acc')] in
                                case length dplaces of
                                    0 -> Nothing -- Contradiction: no place for this value
                                    1 -> assign acc' ((head dplaces), d) -- found the square that can only have this value
                                    _ -> acc) (Just vs'') uss
                    return vs'''

-- search in remaining value options
search :: GridValues -> Maybe GridValues
search values =
    let xs = Map.toList values
        allSizeOne = List.all (\(_, vs) -> length vs == 1) xs
        
        -- sort by fewest possible values
        ys            = filter (\(_, ds) -> length ds > 1) xs
        (s, ds)       = List.minimumBy (\(s1, v1) (s2, v2) ->
            compare (length v1) (length v2)) ys

        -- recursively search for solutions
        assignments   = fmap (\d -> assign values (s, d) >>= search) ds in
            if allSizeOne then Just values -- completed
            else join $ List.find (Maybe.isJust) assignments -- convert to just if solved

solve :: Grid -> Maybe GridValues
solve grid = parseGrid grid >>= search