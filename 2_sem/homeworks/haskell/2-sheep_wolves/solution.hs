module WolfsAndSheep where

import Data.Char (chr, ord)
import Data.List (intercalate, find)
import Control.Monad (guard, liftM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Except
import Control.Monad.Except (throwError, liftIO)
import Control.Monad.Trans.State (StateT, get, put, runStateT)
import System.Random (StdGen, randomR, newStdGen)

data Position = Position { f :: !Char, r :: !Int }
  deriving (Eq)

instance Show Position where
  show (Position f r) = f : show r

data Step = Step { from :: !Position, to :: !Position }
  deriving (Eq)

instance Show Step where
  show (Step f t) = show f ++ " -> " ++ show t

data Game = Game { sheep :: !Position, wolfs :: ![Position] }
  deriving (Eq)

data GameResult = SheepWin | WolfsWin
  deriving (Show)

startSheep :: Position
startSheep = Position 'a' 1

startWolfs :: [Position]
startWolfs = [ Position f 8 | f <- "bdfh" ]

startGame :: Game
startGame = Game startSheep startWolfs

instance Show Game where
  show (Game sPos wPos) = "Board:\n" ++ unlines (map showRow [8,7..1])
    where
      showRow :: Int -> String
      showRow row = unwords [ cellGroup ['a','b'] row, cellGroup ['c','d'] row
                   , cellGroup ['e','f'] row, cellGroup ['g','h'] row ]
        where
          cellGroup files r
            | any (\f -> Position f r == sPos) files = "S"
            | any (\f -> Position f r `elem` wPos) files = "W"
            | otherwise = "."

isOccupied :: Position -> Game -> Bool
isOccupied pos game = pos == sheep game || pos `elem` wolfs game

possibleSheepSteps :: Game -> [Step]
possibleSheepSteps game =
  [ Step currentPos (Position newF newR) | currentPos <- [sheep game],
   (df, dr) <- [ (1,1), (-1,1), (1,-1), (-1,-1) ],
   let newF = chr (ord (f currentPos) + df),
   newF `elem` ['a'..'h'],
   let newR = r currentPos + dr,
   newR >= 1,
   newR <= 8,
   not (isOccupied (Position newF newR) game)
  ]

possibleWolfsSteps :: Game -> [Step]
possibleWolfsSteps game = do
    wolf <- wolfs game
    (df, dr) <- [ (-1, -1), (1, -1) ]
    let newF = chr (ord (f wolf) + df)
    let newR = r wolf + dr
    guard (newF `elem` ['a'..'h'])
    guard (newR >= 1)
    guard (not (isOccupied (Position newF newR) game))
    return $ Step wolf (Position newF newR)

simpleTurn :: Game -> Either GameResult Game
simpleTurn game =
  case possibleSheepSteps game of
    [] -> Left WolfsWin
    (sheepStep:_) ->
      let newSheepPos = to sheepStep
          newGameAfterSheep = game { sheep = newSheepPos }
      in if r newSheepPos == 8
         then Left SheepWin
         else case possibleWolfsSteps newGameAfterSheep of
                [] -> Left SheepWin
                (wolfStep:_) ->
                  let newWolfs = map (\w -> if w == from wolfStep then to wolfStep else w) (wolfs newGameAfterSheep)
                  in Right $ newGameAfterSheep { wolfs = newWolfs }

run :: Monad m => (a -> m a) -> a -> m a
run f a = f a >>= run f

loggedTurn :: Game -> WriterT [Step] (Either GameResult) Game
loggedTurn game = do
  case possibleSheepSteps game of
    [] -> lift (Left WolfsWin)
    (sheepStep:_) -> do
      tell [sheepStep]
      let newSheepPos = to sheepStep
      let newGameAfterSheep = game { sheep = newSheepPos }
      if r newSheepPos == 8
        then lift (Left SheepWin)
        else case possibleWolfsSteps newGameAfterSheep of
              [] -> lift (Left SheepWin)
              (wolfStep:_) -> do
                tell [wolfStep]
                let newWolfs = map (\w -> if w == from wolfStep then to wolfStep else w) (wolfs newGameAfterSheep)
                return $ newGameAfterSheep { wolfs = newWolfs }

loggedTurn' :: Game -> ExceptT GameResult (Writer [Step]) Game
loggedTurn' game = do
  case possibleSheepSteps game of
    [] -> throwError WolfsWin
    (sheepStep:_) -> do
      lift (tell [sheepStep])
      let newSheepPos = to sheepStep
      let newGameAfterSheep = game { sheep = newSheepPos }
      if r newSheepPos == 8
        then throwError SheepWin
        else case possibleWolfsSteps newGameAfterSheep of
              [] -> throwError SheepWin
              (wolfStep:_) -> do
                lift (tell [wolfStep])
                let newWolfs = map (\w -> if w == from wolfStep then to wolfStep else w) (wolfs newGameAfterSheep)
                return $ newGameAfterSheep { wolfs = newWolfs }

randomTurn :: Game -> StateT StdGen (WriterT [Step] (ExceptT GameResult IO)) Game
randomTurn game = do
  let sheepSteps = possibleSheepSteps game
  if null sheepSteps
    then lift (lift (throwError WolfsWin))
    else do
      gen <- get
      let (idx, newGen) = randomR (0, length sheepSteps - 1) gen
      put newGen
      let sheepStep = sheepSteps !! idx
      lift (tell [sheepStep])
      let newSheepPos = to sheepStep
      let newGame = game { sheep = newSheepPos }
      if r newSheepPos == 8
        then lift (lift (throwError SheepWin))
        else do
          let wolfSteps = possibleWolfsSteps newGame
          if null wolfSteps
            then lift (lift (throwError SheepWin))
            else do
              gen' <- get
              let (idx', newGen') = randomR (0, length wolfSteps - 1) gen'
              put newGen'
              let wolfStep = wolfSteps !! idx'
              lift (tell [wolfStep])
              let newWolfs = map (\w -> if w == from wolfStep then to wolfStep else w) (wolfs newGame)
              return $ newGame { wolfs = newWolfs }

interactiveTurn :: Game -> StateT StdGen (WriterT [Step] (ExceptT GameResult IO)) Game
interactiveTurn game = do
  let sheepSteps = possibleSheepSteps game
  if null sheepSteps
    then lift (lift (throwError WolfsWin))
    else do
      liftIO $ putStrLn "Choose a step for the sheep:"
      liftIO $ mapM_ print sheepSteps
      input <- liftIO getLine
      let selected = parseStep input
      case find (== selected) sheepSteps of
        Nothing -> lift (lift (throwError WolfsWin))
        Just sheepStep -> do
          lift (tell [sheepStep])
          let newSheepPos = to sheepStep
          let newGame = game { sheep = newSheepPos }
          if r newSheepPos == 8
            then lift (lift (throwError SheepWin))
            else do
              let wolfSteps = possibleWolfsSteps newGame
              if null wolfSteps
                then lift (lift (throwError SheepWin))
                else do
                  gen <- get
                  let (idx, newGen) = randomR (0, length wolfSteps - 1) gen
                  put newGen
                  let wolfStep = wolfSteps !! idx
                  lift (tell [wolfStep])
                  let newWolfs = map (\w -> if w == from wolfStep then to wolfStep else w) (wolfs newGame)
                  return $ newGame { wolfs = newWolfs }
  where
    parseStep s = let [fromS, "->", toS] = words s
                  in Step (parsePos fromS) (parsePos toS)
    parsePos (f:rStr) = Position f (read rStr)
