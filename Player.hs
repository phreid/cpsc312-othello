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
heuristicPlayerDecision board color = getValidMoves board color !! indexOfGreatestValue
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

-- For each set of boards, calculate the value and return the maximum
getMaximumVal :: [Board] -> Color -> Int
getMaximumVal boards color = maximum [scoreBoard b color | b <- boards]

-- AI player. Chooses the best move out of the available moves, looking 3 moves ahead
lookaheadPlayer :: Player
lookaheadPlayer board color = do
    if hasValidMoves board color then (do
        let move = lookaheadDecision board color
        return $ Just move)
    else return Nothing

-- Returns the best move, looking 3 turns ahead
-- Assumes that there is always a valid move when this is called
lookaheadDecision :: Board -> Color -> Move
lookaheadDecision board color = possibleMoves !! indexOfGreatestValue
  where moveAndValues = moveBoardList board color
        possibleMoves = map fst (getMoveMaxVal moveAndValues color)
        indexOfGreatestValue = maxIndex (map snd (getMoveMaxVal moveAndValues color))

-- Maximum score it could possibly attain
-- Also evaluates depending on the position of the move
getMoveMaxVal :: [(Move, [Board])] -> Color -> [(Move, Int)]
getMoveMaxVal moveB color = [ value | (x, y) <- moveB,
  let value = (x, getMaximumVal y color + checkPosition x color)]

-- Checks where the valid move is, and assigns more weight if a corner,  
-- and subtracts if it's around the corner, otherwise just 0
checkPosition :: Move -> Color -> Int
checkPosition move color
  | move `elem` corner = 10
  | move `elem` badMove = -10
  | otherwise = 0
  where corner = [((0, maxCol), color), ((0,0), color), 
                  ((maxRow, 0), color), ((maxRow, maxCol), color)]
        badMove = [((1,0), color),((1,1), color),((0,1),color),
                  ((1,maxCol),color),((1,6),color),((0,6),color),
                  ((6,maxCol),color),((maxRow,6),color),((6,6),color),
                  ((6,0),color),((6,1),color),((maxRow,1),color)]

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

-- argmax function taken from class
argmax :: Ord v => (e -> v) -> [e] -> (e,v)
argmax f [e] = (e, f e)
argmax f (h:t)
   | fh > ft = (h,fh)
   | otherwise = (bt, ft)
   where
      (bt,ft) = argmax f t
      fh = f h

-- A minimax player. The game tree for Othello grows fast,
--  so can't search very deep.
minimaxPlayer :: Player
minimaxPlayer board color = do
    return $ chooseMinimax board color 3

chooseMinimax :: Board -> Color -> Int -> Maybe Move
chooseMinimax board color maxDepth =
    if hasValidMoves board color then
        Just $ fst $ argmax
            (\move -> minimax (fromJust $ doMove board move) color maxDepth)
            (getValidMoves board color)
    else Nothing

-- Produces the minimax value. One difference from standard minimax is that
--  in Othello it's possible for a player to have two or more moves in a row
--  if the opposing player has no valid moves. 
minimax :: Board -> Color -> Int -> Int
minimax board color depth
    | isGameOver board = if won then 1000 else -1000
    | depth == 0 = evaluate board color
    | otherwise = if nextTurn /= color then -value else value
    where won = netScore board color > 0
          nextTurn = if hasValidMoves board (flipColor color)
                     then flipColor color
                     else color
          value = maximum $
                    map (\move -> minimax (fromJust $ doMove board move) nextTurn (depth-1))
                        (getValidMoves board nextTurn)



-- Board evaluation function for minimax. Gives high weight to moves in the corner,
--  and also prefers boards where the player has lots of available moves. Also takes
--  into account the player's net score (number of pieces - number of opposing pieces),
--  but this is de-emphasized as it's not a major part of Othello strategy.
evaluate :: Board -> Color -> Int
evaluate board color = 20 * numCorners + 5 * numMoves + netScore board color
    where numCorners = length $ filter (corner color) board
          corner color ((row, col), clr) =
            color == clr &&
             ((row, col) == (0, 0) ||
              (row, col) == (0, maxCol) ||
              (row, col) == (maxRow, 0) ||
              (row, col) == (maxRow, maxCol))
          numMoves = length $ getValidMoves board color

netScore :: Board -> Color -> Int
netScore board color = scoreBoard board color - scoreBoard board (flipColor color)