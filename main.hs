import System.IO (stdin, hSetEcho, hSetBuffering, BufferMode(NoBuffering))
import System.Random (getStdGen, randoms)
import Data.Bits (shiftL, shiftR, complement, (.&.), (.|.))
import Data.List (intercalate)
import Data.Ratio (denominator, (%))
import Data.Char (chr, ord)

type Board = Integer
type Coord = Int
type Tile = Integer
type Rand = Integer

data Action = L | R | U | D | Quit | None deriving (Eq,Ord,Enum,Show)

-- CONFIG ---------------------------------------------------------------------

-- the size of the board
columnCount = 4
rowCount = 4

-- tile values to seed into empty spaces
lowSeed = 1
highSeed = 2

-- the probability of seeding highSeed instead of lowSeed
highSeedProbability = 1 % 10

-- characters to show on tiles
firstLetter = 'A'
winningLetter = 'K'

-- BOARD STATE ----------------------------------------------------------------

columns = [0..(columnCount - 1)]
rows = [0..(rowCount - 1)]

rowCoords :: Coord -> [(Coord, Coord)]
rowCoords y = map (\x -> (x, y)) columns

columnCoords :: Coord -> [(Coord, Coord)]
columnCoords x = map (\y -> (x, y)) rows

allCoords :: [(Coord, Coord)]
allCoords = concat (map rowCoords rows)

emptyCoords :: Board -> [(Coord, Coord)]
emptyCoords b = filter (\c -> getTile b c == 0) allCoords

winningTile :: Tile
winningTile = fromIntegral ((winningIndex - firstIndex) + 1)
  where firstIndex = ord firstLetter
        winningIndex = ord winningLetter
bitsPerTile = fromIntegral (tryShift 1)
  where tryShift n = 
          if shiftL 1 n > winningTile then n else tryShift (n + 1)
tileMask = (shiftL 1 bitsPerTile) - 1

shiftForTile :: Coord -> Coord -> Int
shiftForTile x y = ((maxTileIndex - ((y * columnCount) + x)) * bitsPerTile)
  where maxTileIndex = (rowCount * columnCount) - 1

getTile :: Board -> (Coord, Coord) -> Tile
getTile b (x, y) = tileMask .&. (shiftR b (shiftForTile x y))

setTile :: Board -> (Coord, Coord) -> Tile -> Board
setTile b (x, y) t = 
  (b .&. (complement (shiftL tileMask (shiftForTile x y))))
  .|. (shiftL (t .&. tileMask) (shiftForTile x y))

getTiles :: Board -> [(Coord, Coord)] -> [Tile]
getTiles b cs = map (\c -> getTile b c) cs

setTiles :: Board -> [(Coord, Coord)] -> [Tile] -> Board
setTiles b (c:cs) (t:ts) = setTiles (setTile b c t) cs ts
setTiles b [] [] = b

initBoard :: Rand -> Board
initBoard rand = seedBoard (seedBoard 0 rand) rand

seedBoard :: Board -> Rand -> Board
seedBoard b rand = setTile b (pickCoord (emptyCoords b) rand) (seedValue rand)
  where pickCoord cs rand = 
          cs !! fromIntegral (mod rand (fromIntegral (length cs)))
        seedValue rand = 
          if (mod rand d) % d < highSeedProbability then highSeed else lowSeed
        d = denominator highSeedProbability

collapseCoordLists :: Board -> [[(Coord, Coord)]] -> Board
collapseCoordLists b [] = b
collapseCoordLists b (cs:rest) = collapseCoordLists (collapseCoords b cs) rest
  where collapseCoords b cs = 
          setTiles b cs (collapseTiles (getTiles b cs))
        collapseTiles ts = 
          padEnd (length ts) (combineTiles (removeEmptyTiles ts))
        removeEmptyTiles ts =
          filter (\t -> t > 0) ts
        combineTiles (a:rest) =
          if length rest == 0 then [ a ]
          else if a == (head rest) then
            [ a + 1 ] ++ (combineTiles (tail rest))
          else
            [ a ] ++ (combineTiles rest)
        combineTiles rest = rest
        padEnd len cs = 
          if length cs < len then padEnd len (cs ++ [0])
          else cs

collapseBoard :: Board -> Action -> Board
collapseBoard b dir = 
  case dir of
    L -> collapseCoordLists b (map rowCoords rows)
    R -> collapseCoordLists b (map reverse (map rowCoords rows))
    U -> collapseCoordLists b (map columnCoords rows)
    D -> collapseCoordLists b (map reverse (map columnCoords rows))
    _ -> b

isLosingBoard :: Board -> Bool
isLosingBoard b = (length (emptyCoords b) == 0)

isWinningBoard :: Board -> Bool
isWinningBoard b = maximum (getTiles b allCoords) >= winningTile

-- RENDERING ------------------------------------------------------------------

reset = "\ESC[" ++ (show (rowCount + 2)) ++ "A"

borderColor = "\ESC[90m"
defaultColor = "\ESC[39m"

tileColorCode :: Tile -> Int
tileColorCode t = case t of
  0  -> 8
  1  -> 244
  2  -> 249
  3  -> 136
  4  -> 166
  5  -> 214
  6  -> 196
  7  -> 197
  8  -> 198
  9  -> 199
  10 -> 200
  11 -> 201
  12 -> 213
  _  -> 231

tileColor :: Tile -> String
tileColor t = "\ESC[38;5;" ++ (show (tileColorCode t)) ++ "m"

renderBoard :: Board -> String
renderBoard b = 
  borderColor ++
  (renderBorder "┏" "┓") ++
  concat (map (\r -> "┃ " ++ (renderRow b r) ++ " ┃\n") rows) ++
  (renderBorder "┗" "┛") ++
  defaultColor
  where renderBorder lc rc = 
          lc ++ (concat (replicate columnCount "━━")) ++ "━" ++ rc ++ "\n"
        renderRow b y = 
          intercalate " " $ map renderTile (getTiles b (rowCoords y))
        renderTile t = 
          tileColor t ++
          (if t == 0 then "·" else letter (t - 1)) ++
          borderColor
        letter i =
          [ chr ((ord firstLetter) + (fromIntegral i)) ]

--- GAMEPLAY ------------------------------------------------------------------

start :: [Rand] -> String -> String
start rands input =
  (renderBoard 0) ++ (play (initBoard (head rands)) (tail rands) input)

play :: Board -> [Rand] -> String -> String
play b rands input =
  reset ++ (renderBoard b) ++
  if isWinningBoard b then
    "You win!\n"
  else if isLosingBoard b then
    "Game Over.\n"
  else if action input == Quit then
    "You quit.\n"
  else
    play (makeMove b (action input) (head rands)) (tail rands) (tail input)
  where action input =
          case (head input) of
            'A' -> U
            'B' -> D
            'C' -> R
            'D' -> L
            'q' -> Quit
            _   -> None
        makeMove b dir rand = do
          let b' = collapseBoard b dir
          if b == b' then b
          else seedBoard b' rand

-- IMPURITIES SINK TO THE BOTTOM ----------------------------------------------

main = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  g <- getStdGen
  interact $ start (randoms g :: [Rand])
