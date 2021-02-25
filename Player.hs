module Player where

import Othello
import System.Random
import Text.Read
import Data.Maybe

-- A human player. Asks the user for input from the console.
humanPlayer :: Player
humanPlayer board color = do
    if hasValidMoves board color then (do
        putStr "Enter a row: "
        r <- getLine
        putStr "Enter a col: "
        c <- getLine
        case (readMaybe r, readMaybe c) of 
            (Nothing, _) -> do
                putStrLn "\nInvalid input."
                humanPlayer board color
            (_, Nothing) -> do
                putStrLn "\nInvalid input."
                humanPlayer board color
            (Just row, Just col) -> do
                let move = ((row, col), color)
                case doMove board move of
                    Nothing -> do
                        putStrLn "\nNot a valid move. Try again."
                        humanPlayer board color
                    Just nextBoard -> return $ Just move) 
    else return Nothing
            
-- A random player. Randomly chooses from the list of available moves.
randomPlayer :: Player
randomPlayer board color = do
    if hasValidMoves board color then (do
        gen <- newStdGen
        let moves = getValidMoves board color
        let (choice, _) = randomR (0, length moves - 1) gen
        let move = moves !! choice
        return $ Just move) 
    else return Nothing 

-- A moderate player. Picks the best move at the time based off weighted moves.
heuristicPlayer :: Player
heuristicPlayer board color = do 
    if hasValidMoves board color then (do
        let move = heuristicPlayerDecision board color
        return $ Just move
        )
    else return Nothing

-- gets the best move at the current point in time
heuristicPlayerDecision :: Board -> Color -> Move
heuristicPlayerDecision board color = (getValidMoves board color) !! indexOfGreatestValue
  where indexOfGreatestValue = maxIndex (map fst (valueMoveTuple board color))

-- Taken from stackoverflow
maxIndex :: Ord a => [a] -> Int
maxIndex xs = head $ filter ((== maximum xs) . (xs !!)) [0..]

-- Calculates the score for each valid move 
valueMoveTuple :: Board -> Color -> [(Int, Move)]
valueMoveTuple board color =
  [( 1 + flippedVal + currentVal + bonusCornerVal, moves)| moves <- getValidMoves board color,
     let flippedVal = length(getFlipped board moves),
     let currentVal = scoreBoard board color, 
     let bonusCornerVal = cornerVal moves]

isCorner :: Move -> Bool 
isCorner move 
    | fst move == (0,0) = True
    | fst move == (0,7) = True
    | fst move == (7,0) = True
    | fst move == (7,7) = True
    | otherwise = False 

cornerVal :: Move -> Int
cornerVal move 
    | isCorner move = 50
    | otherwise = 0

-- Maximum score it could possibly attain
-- If a possible move includes a corner position, more weight/value added 
getMoveMaxVal :: [(Move, [Board])] -> Color -> [(Move, Int)]
getMoveMaxVal moveB color = [if x  `elem` badMoves then (x, (getMaximumVal y color)-10) 
  else (x, getMaximumVal y color) | (x, y) <- moveB, 
  let corner = [((0,7), color),((7,0), color),((0,0),color),((7,7),color)],
  let badMoves = [((1,0), color),((1,1), color),((0,1),color),
                  ((1,7),color),((1,6),color),((0,6),color),
                  ((6,7),color),((7,6),color),((6,6),color),
                  ((6,0),color),((6,1),color),((7,1),color)]]

-- For each set of boards, calculate the value and return the maximum
getMaximumVal :: [Board] -> Color -> Int 
getMaximumVal boards color = maximum [scoreBoard b color | b <- boards]

-- AI player. Chooses the best move out of the available moves, looking 5 moves ahead
lookaheadPlayer :: Player
lookaheadPlayer board color = do 
    if hasValidMoves board color then (do 
        let move = lookaheadDecision board color
        return $ Just move)
    else return Nothing

-- Returns the best move, looking 5 turns ahead
-- Assumes that there is always a valid move when this is called
lookaheadDecision :: Board -> Color -> Move
lookaheadDecision board color = possibleMoves !! indexOfGreatestValue
  where moveAndValues = moveBoardList board color
        possibleMoves = map fst (getMoveMaxVal moveAndValues color)
        indexOfGreatestValue = maxIndex (map snd (getMoveMaxVal moveAndValues color))

-- Returns a list of all the possible board states per move
moveBoardList :: Board -> Color -> [(Move, [Board])]
moveBoardList board color = [finalBoardState x ((:[]) board) color 1 | x <- getValidMoves board color]

-- This gives us all the possible boards, looking 3 turns ahead
finalBoardState :: Move -> [Board] -> Color -> Int -> (Move, [Board])
finalBoardState move boards color depth
  | depth == 1 = finalBoardState move allBoards color (depth + 1)
  | depth == 3 = (move, allBoards)
  | otherwise = finalBoardState move allBoards color (depth + 1)
    where allBoards = getAllBoards boards color depth 

-- For each board in the list, I want to get the next possible boards
-- Changes color accordingly 
getAllBoards :: [Board] -> Color -> Int -> [Board]
getAllBoards boards color depth = concat ((map (\x -> nextGameBoards x colorMove)) boards) 
    where colorMove        
            | depth `mod` 2 == 0 && color == White = Black 
            | depth  `mod` 2 == 0 && color == Black = White 
            | depth `mod` 2 /= 0 && color == Black = Black 
            | otherwise = White

-- This returns a list of boards that are possible after each valid move is made
-- If there are no valid moves, the original board is returned
nextGameBoards :: Board -> Color -> [Board]
nextGameBoards board color = if hasValidMoves board color 
  then [fromJust $ doMove board y | y <- (getValidMoves board color)]
  else (:[]) board

-- Returns the move that will give the greatest value
-- Assumes that there is always a valid move when this is called
-- TODO: is this ever called?
-- miniMaxDecision :: Board -> Color -> Move
-- miniMaxDecision board color = (getValidMoves board color) !! indexOfGreatestValue
--   where indexOfGreatestValue = maxIndex (map fst (valueMoveTuple board color))