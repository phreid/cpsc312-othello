module Tournament where

import Othello

data TournamentState = Tournament {
    game :: GameState,
    startGame :: GameState,
    gamesLeft :: Int,
    blackWon :: Int,
    whiteWon :: Int,
    ties :: Int
} | TournamentOver {blackWon :: Int, whiteWon :: Int, ties :: Int}

-- Given the next game state, produces the next tournament state. If the game is over, 
--  updates the win/loss records and starts a new game until the number of games 
--  left in the tournament is zero.
nextTournamentState :: TournamentState -> GameState -> TournamentState
nextTournamentState Tournament{gamesLeft = 0, blackWon = bw, whiteWon = ww, ties = t} _ = 
    TournamentOver{blackWon = bw, whiteWon = ww, ties = t}

nextTournamentState tment@Tournament{startGame = start, 
                                     gamesLeft = gl, blackWon = bw, whiteWon = ww, ties = t} 
                    GameOver {winner = w} = 
    tment{game = start, gamesLeft = gl - 1, blackWon = nextBW bw w, whiteWon = nextWW ww w, ties = nextTies t w}
        where nextBW bw Nothing = bw
              nextBW bw (Just Black) = bw + 1
              nextBW bw _ = bw
              nextWW ww Nothing = ww
              nextWW ww (Just White) = ww + 1
              nextWW ww _ = ww
              nextTies t Nothing = t + 1
              nextTies t _ = t

nextTournamentState tment gameState = tment{game = gameState}
