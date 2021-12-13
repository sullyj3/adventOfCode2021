module Day12 where

import Utils (showSolutions)
import Text.Megaparsec
import Parsing
-- import Data.Map (Map)
import Data.Map qualified as Map
-- import Data.Set  (Set)
import Data.Set qualified as Set
import Prelude hiding (some)
import Text.Megaparsec.Char (string, lowerChar, upperChar, space)
import qualified Data.Text as T


data Node = Start | Little Text | Big Text | End
  deriving (Show, Eq, Ord)
type Edge = (Node, Node)
type Graph = Map Node (Set Node)

node :: Parser Node
node = try (Start <$ string "start") <|>
       try (End <$ string "end") <|>
       try (Little . T.pack <$> some lowerChar ) <|>
       (Big . T.pack <$> some upperChar)

edge :: Parser Edge
edge = (,) <$> (node <* single '-') <*> node

edges :: Parser [Edge]
edges = edge `sepEndBy` space

graphFromEdges :: [Edge] -> Graph
graphFromEdges = Map.fromListWith (<>) . map (second Set.singleton)

exampleGraph :: IO Graph
exampleGraph = do
  t <- readFileText "inputs/day12example1.txt"
  let Just g = graphFromEdges <$> parseMaybe edges t
  pure g


solve :: Text -> Text
solve input = showSolutions graph ()
  where Just theEdges = parseMaybe edges input
        graph = graphFromEdges theEdges
        -- p1 = part1 ()
        -- p2 = part2 ()

-- part1 :: () -> ()
-- part1 = undefined

-- part2 :: () -> ()
-- part2 = undefined
