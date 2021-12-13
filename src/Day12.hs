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
import Control.Arrow ((***))


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
solve input = showSolutions p1 p2
  where Just theEdges = parseMaybe edges input
        graph = graphFromEdges theEdges
        p1 = numPaths graph p1CanVisit p1Visit mempty
        p2 = numPaths graph p2CanVisit p2Visit (mempty, False)

-- part1 :: () -> ()
-- part1 = undefined

-- part2 :: () -> ()
-- part2 = undefined

-- todo can just use a visitedcount for part 2
type VisitedSet = Set Node
type VisitedCounts = (Map Node Int, Bool)

type CanVisit s = Node -> s -> Bool
type Visit s = Node -> s -> s

p1CanVisit :: CanVisit VisitedSet
p1CanVisit node = (node `Set.notMember`)

p1Visit :: Visit VisitedSet
p1Visit node = case node of
  Small _ -> Set.insert node
  Start -> Set.insert node
  _ -> id

p2CanVisit :: CanVisit VisitedCounts
p2CanVisit node (counts, hasVisitedSmallTwice) = 
  -- trace ("visited node " <> show node <> " " <> show visitedCount <> " times") $
  case node of
    Start -> visitedCount == 0
    End -> True
    Small _label
      | hasVisitedSmallTwice -> visitedCount == 0
      | otherwise -> visitedCount < 2
    Big _label -> True
  where visitedCount = Map.lookup node counts ?: 0

p2Visit :: Visit VisitedCounts
p2Visit node (visitedCounts, hasVisitedSmallTwice) =
  -- | hasVisitedSmallTwice' && trace "toggling hasVisitedSmallTwice" False = undefined
  -- | otherwise =
  (counts', hasVisitedSmallTwice')
  where counts' = Map.insertWith (+) node 1 visitedCounts
        hasVisitedSmallTwice' =
          hasVisitedSmallTwice || (Map.lookup node counts' ?: 0) >= 2

type Path = [Node]

paths :: forall s. Graph -> CanVisit s -> Visit s -> s -> [[Node]]
paths graph canVisit visit initialState = flip evalStateT ([], initialState) $ go Start
  where
    go :: Node -> StateT (Path, s) [] Path
    go node = do
      guardM $ canVisit node . snd <$> get
      -- traceM $ "visiting: " <> show node
      modify $ (node:) *** visit node
      case node of
        End -> do
          -- traceM "reached end, recording path"
          (path, _) <- get
          pure $ reverse path
        _ -> do
          -- traceM $ "trying to visit node: " <> show node
          -- cv <- canVisit node <$> get
          -- traceM $ "canVisit?: " <> show cv


          neighbour <- lift $ Set.toList $ (graph !? node) ?: mempty
          go neighbour

numPaths :: forall s. Graph -> CanVisit s -> Visit s -> s -> Int
numPaths graph canVisit visit initialState = length $ paths graph canVisit visit initialState
