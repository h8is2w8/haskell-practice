module Main where

import Data.List (intercalate, find)
import Data.Maybe (fromJust)
import Text.Read (readMaybe)


type Error = String
type PlayerLocation = Location
type PlayerInventory = [Item]
type ItemLocations = [(Item, Location)]

data Location = LivingRoom | Garden | Attic deriving (Show, Eq)
data Item = Whiskey | Bucket | Chain | Frog deriving (Show, Eq, Read)
data GameState = GameState
                 PlayerLocation
                 PlayerInventory
                 ItemLocations
                 deriving Show

nodes :: [(Location, String)]
nodes =
  [
    (LivingRoom, "you are in the living-room. a wizard is snoring loudly on the couch."),
    (Garden, "you are in a beautiful garden. there is a well in front of you."),
    (Attic, "you are in the attic. there is a giant welding torch in the corner.")
  ]

edges :: [(Location, [(Location, String, String)])]
edges =
  [
    (LivingRoom, [(Garden, "west", "door"),
                  (Attic, "upstairs", "ladder")]),
    (Garden, [(LivingRoom, "east", "door")]),
    (Attic, [(LivingRoom, "downstairs", "ladder")])
  ]

lookup' key alist = fromJust $ lookup key alist

look :: GameState -> String
look state =
  intercalate " " [f state | f <- [descLoc, descPaths, descItems],
                             let text = f state,
                             not (null text)]

descLoc (GameState loc _ _) = lookup' loc nodes
descPaths (GameState loc _ _) =
  intercalate " " $ map descPath $ lookup' loc edges
  where descPath (_, dir, portal) =
          "there is a " ++ portal ++ " going " ++ dir ++ " from here."

itemsAt :: Location -> ItemLocations -> [Item]
itemsAt loc locations =
  [item | (item, itemLoc) <- locations, itemLoc == loc]

descItems :: GameState -> String
descItems (GameState loc _ itemLoc) =
  intercalate " " $ map descItem $ itemsAt loc itemLoc
  where descItem obj = "you see a " ++ (show obj) ++ " on the floor."

walk :: GameState -> String -> Either Error GameState
walk (GameState loc inventory itemLoc) nextDir =
  let neighbours = lookup' loc edges
      nextLoc = find (\(_, dir, _) -> dir == nextDir) neighbours
  in case nextLoc of
    Nothing -> Left "i cannot go there"
    Just (loc, _, _) -> Right $ GameState loc inventory itemLoc

inventory :: GameState -> String
inventory (GameState _ storage _) =
  if length(storage) > 0
  then
    "you have: " ++ (intercalate ", " $ map show storage)
  else
    "you have nothing in your pockets!"

pick :: GameState -> Item -> Either Error GameState
pick (GameState loc inventory itemLoc) itemCandidate =
  let itemsAtLoc = itemsAt loc itemLoc
  in case find (\i -> i == itemCandidate) itemsAtLoc of
    Nothing -> Left "there is nothing to pick up."
    Just item ->
      let newItemLoc = filter (\(i, _) -> i /= item) itemLoc
      in Right $ GameState loc (item : inventory) newItemLoc

parseItem :: [String] -> Either Error Item
parseItem [] = Left "you should specify what you want to pick up."
parseItem args =
  case (readMaybe (head args)) :: Maybe Item of
    Nothing -> do Left "i cannot pick up that."
    Just item -> do Right item

parseDir :: [String] -> Either Error String
parseDir [] = Left  "you should specify a direction."
parseDir args = Right (head args)

launch :: GameState -> IO ()
launch state = do
  input <- getLine

  let (cmd:args) = words $ input

  case cmd of
    "quit" -> do
      putStrLn "Bye!"
    "look" -> do
      putStrLn $ look state
      launch state
    "walk" -> do
      case parseDir args >>= (walk state) of
        Left err -> do
          putStrLn err
          launch state
        Right newState -> do
          putStrLn $ look newState
          launch newState
    "inventory" -> do
      putStrLn $ inventory state
      launch state
    "pick" -> do
      case parseItem args >>= (pick state) of
        Left err -> do
          putStrLn err
          launch state
        Right newState -> do
          putStrLn "you obtained a new item. check your inventory."
          launch newState
    _ -> do
      putStrLn "i cannot do this."
      launch state

main :: IO ()
main = do
  putStrLn "Tip: you should look around first."
  launch (GameState
          LivingRoom
          []
          [
            (Whiskey, LivingRoom),
            (Bucket, LivingRoom),
            (Chain, Garden),
            (Frog, Garden)
          ])
