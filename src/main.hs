import System.IO (stdin, hSetEcho, hSetBuffering, hReady, BufferMode(NoBuffering))
import System.Random (getStdRandom, random)
import Data.Bits (shiftL, shiftR, complement, (.&.), (.|.))
import Data.List (intercalate)
import Data.Ratio (denominator, (%))
import Data.Char (chr, ord)

type Board = Integer
type Coord = Int
type Tile = Integer
type Rand = Integer

data Direction = L | R | U | D deriving (Eq,Ord,Enum,Show)

-- CONFIG ---------------------------------------------------------------------

-- the size of the board
columnCount = 4
rowCount = 4
tileCount = rowCount * columnCount

-- tile storage format
bitsPerTile = 4
tileMask = fromIntegral ((shiftL bitsPerTile 1) - 1)

-- the probability of seeding a B instead of an A
highSeedProbability = 1 % 10

-- letters to show on tiles
firstLetter = ord 'A'
winningLetter = ord 'K'
winningTile = fromIntegral ((winningLetter - firstLetter) + 1)

-- BOARD STATE ----------------------------------------------------------------

columns = [0..(columnCount - 1)]
rows = [0..(rowCount - 1)]

rowCoords :: Coord -> [(Coord, Coord)]
rowCoords y = map (\x -> (x, y)) columns

columnCoords :: Coord -> [(Coord, Coord)]
columnCoords x = map (\y -> (x, y)) columns

allCoords :: [(Coord, Coord)]
allCoords = concat $ map rowCoords rows

emptyCoords :: Board -> [(Coord, Coord)]
emptyCoords b = filter (\c -> getTile b c == 0) allCoords

shiftForTile :: Coord -> Coord -> Int
shiftForTile x y = (((tileCount - 1) - ((y * columnCount) + x)) * bitsPerTile)

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
        seedValue rand = if (mod rand d) % d < highSeedProbability then 2 else 1
        d = denominator highSeedProbability

collapseTiles :: [Tile] -> [Tile]
collapseTiles ts = 
  leftPad (length ts) (combineTilesFromEnd (removeEmptyTiles ts))
  where removeEmptyTiles ts = filter (\t -> t > 0) ts
        combineTilesFromEnd tiles = reverse (combineTiles (reverse tiles))
        combineTiles (a:rest) =
          if length rest == 0 then [ a ]
          else if a == (head rest) then
            [ a + 1 ] ++ (combineTiles (tail rest))
          else
            [ a ] ++ (combineTiles rest)
        combineTiles rest = rest
        leftPad len cs = 
          if length cs < len then leftPad len (0:cs)
          else cs

collapseCoordLists :: Board -> [[(Coord, Coord)]] -> Board
collapseCoordLists b [] = b
collapseCoordLists b (cs:rest) = collapseCoordLists (collapseCoords b cs) rest
  where collapseCoords b cs = 
          setTiles b cs (collapseTiles (getTiles b cs))

collapseBoard :: Board -> Direction -> Board
collapseBoard b dir = 
  case dir of
    R -> collapseCoordLists b (map rowCoords rows)
    L -> collapseCoordLists b (map reverse (map rowCoords rows))
    D -> collapseCoordLists b (map columnCoords rows)
    U -> collapseCoordLists b (map reverse (map columnCoords rows))

makeMove :: Board -> Direction -> Rand -> Board
makeMove b dir rand = do
  let b' = collapseBoard b dir
  if b == b' then b
  else (seedBoard b' rand)

isLosingBoard :: Board -> Bool
isLosingBoard b = (length (emptyCoords b) == 0)

isWinningBoard :: Board -> Bool
isWinningBoard b = maximum (getTiles b allCoords) >= winningTile

-- RENDERING ------------------------------------------------------------------

renderTile :: Tile -> String
renderTile t = 
  tileColor t ++
  (if t == 0 then "." else letter (t - 1)) ++
  borderColor
  where letter i =
          [ chr (firstLetter + (fromIntegral i)) ]

renderRow :: Board -> Coord -> String
renderRow b y = intercalate " " $ map renderTile (getTiles b (rowCoords y))

renderBorder :: String
renderBorder = "+" ++ (concat (replicate columnCount "--")) ++ "-+\n"

renderBoard :: Board -> String
renderBoard b = 
  borderColor ++
  renderBorder ++
  concat (map (\r -> "| " ++ (renderRow b r) ++ " |\n") rows) ++
  renderBorder ++
  defaultColor

-- TERMINAL CONTROL -----------------------------------------------------------

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

-- IMPURITIES SINK TO THE BOTTOM ----------------------------------------------

getKey :: IO [Char]
getKey = reverse <$> getKey' ""
  where getKey' chars = do
          char <- getChar
          more <- hReady stdin
          (if more then getKey' else return) (char:chars)

play :: Board -> IO ()
play b = do
  rand <- getStdRandom random
  putStr reset
  putStr (renderBoard b)
  if isWinningBoard b then
    putStrLn "You win!"
  else
    if isLosingBoard b then
      putStrLn "Game Over."
    else do
      key <- getKey
      case key of
        "\ESC[A" -> play (makeMove b U rand)
        "\ESC[B" -> play (makeMove b D rand)
        "\ESC[C" -> play (makeMove b R rand)
        "\ESC[D" -> play (makeMove b L rand)
        "q"      -> putStrLn "You quit."
        _        -> play b

main = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  putStr (renderBoard 0)
  seed <- getStdRandom random
  play (initBoard seed)
