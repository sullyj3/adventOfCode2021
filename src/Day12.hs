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
import Relude.Extra.Map ((!?))


data Node = Start | Small Text | Big Text | End
  deriving (Show, Eq, Ord)
type Edge = (Node, Node)
type Graph = Map Node (Set Node)

parseNode :: Parser Node
parseNode = try (Start <$ string "start") <|>
       try (End <$ string "end") <|>
       try (Small . T.pack <$> some lowerChar ) <|>
       Big . T.pack <$> some upperChar

edge :: Parser Edge
edge = (,) <$> (parseNode <* single '-') <*> parseNode

edges :: Parser [Edge]
edges = edge `sepEndBy` space

graphFromEdges :: [Edge] -> Graph
graphFromEdges = Map.fromListWith (<>) . map (second Set.singleton) . addReverseEdges
  where addReverseEdges es = es <> map swap es


exampleGraph :: IO Graph
exampleGraph = do
  t <- readFileText "inputs/day12example1.txt"
  let Just g = graphFromEdges <$> parseMaybe edges t
  pure g


solve :: Text -> Text
solve input = showSolutions p1 ()
  where Just theEdges = parseMaybe edges input
        graph = graphFromEdges theEdges
        p1 :: Int
        p1 = numPaths graph p1CanVisit p1Visit mempty
        -- p2 = part2 ()

-- part1 :: () -> ()
-- part1 = undefined

-- part2 :: () -> ()
-- part2 = undefined
type VisitedSet = Set Node

type CanVisit s = Node -> s -> Bool
type Visit s = Node -> s -> s

p1CanVisit :: CanVisit VisitedSet
p1CanVisit node = (node `Set.notMember`)

p1Visit :: Visit VisitedSet
p1Visit node = case node of
  Small _ -> Set.insert node
  Start -> Set.insert node
  _ -> id

numPaths :: forall s. Graph -> CanVisit s -> Visit s -> s -> Int
numPaths graph canVisit visit initialState =
  length $ flip evalStateT initialState $ go Start
  where
    go :: Node -> StateT s [] ()
    go = \case
      End -> pure ()
      node -> do
        guardM $ canVisit node <$> get
        modify $ visit node

        neighbour <- lift $ Set.toList $ (graph !? node) ?: mempty
        go neighbour
