module Chess where

-- See https://en.wikipedia.org/wiki/Chess for more details
-- We only consider the situation where there is only a single
-- piece on the board

-- see Rules - Set up for basic definitions

type File     = Char         -- column index
                             -- valid files are 'a','b',...,'h'
type Rank     = Int          -- row index
                             -- valid ranks are 1,2,...8
type Position = (File,Rank)   

data Color =
  Black | White
  deriving (Eq,Show)

data Piece =
  King | Queen | Rook | Bishop | Knight | Pawn
  deriving (Eq,Show)

isLegalPosition :: Position -> Bool
-- implement isLegalPosition
isLegalPosition q | fst q `elem` ['a'..'h'] && snd q `elem` [1..8] = True
                  | otherwise = False

-- see Rules - Movement for legal movements 

isLegalMove :: Color -> Piece -> Position -> Position -> Bool
-- implement isLegalMove
isLegalMove a b q1 q2 | not (isLegalPosition q1 && isLegalPosition q2) = False
			-- King
                      | b == King && dist q1 q2 2 `elem` [-1,0,1] && dist q1 q2 2 `elem` [-1,0,1] = True
			-- Queen
				-- Diagonal/Left/Right movements
                      | b == Queen && dist q1 q2 1 /= 0 && dist q1 q2 2 `elem` [0 - dist q1 q2 1,0,dist q1 q2 1] = True
				-- Up/Down movements
                      | b == Queen && dist q1 q2 1 == 0 && dist q1 q2 2 /= 0 = True
			-- Rook
				-- Left/Right movements
                      | b == Rook && dist q1 q2 1 /= 0 && dist q1 q2 2 == 0 = True
				-- Up/Down Movements
                      | b == Rook && dist q1 q2 1 == 0 && dist q1 q2 2 /= 0 = True
			-- Bishop
				-- Black placements (x+y should be odd)
                      | b == Bishop && a == Black && (even (fromEnum (fst q1) + snd q1) || even (fromEnum (fst q2) + snd q2)) = False
				-- White Placements (x+y should be even)
                      | b == Bishop && a == White && (even (fromEnum (fst q1) + snd q1) || even (fromEnum (fst q2) + snd q2)) = False
				-- Diagonal Movements
                      | b == Bishop && dist q1 q2 1 `elem` [0-dist q1 q2 2,dist q1 q2 2] = True
			-- Knight
				-- 2 Up/Down steps + 1 Left/Right step
                      | b == Knight && dist q1 q2 1 `elem` [-2,2] && dist q1 q2 2 `elem` [-1,1] = True
				-- 2 Left/Right steps + 1 Up/Down step
                      | b == Knight && dist q1 q2 1 `elem` [-1,1] && dist q1 q2 2 `elem` [-2,2] = True
			-- Pawn
				-- Black 1/2 steps (down only)
                      | b == Pawn && a == Black && dist q1 q2 1 == 0 && dist q1 q2 2 `elem` [-2,-1] = True
				-- White 1/2 steps (up only)
                      | b == Pawn && a == White && dist q1 q2 1 == 0 && dist q1 q2 2 `elem` [1,2] = True
                      | otherwise = False

-- function to calculate distance of a movement for x and y
dist :: Position -> Position -> Int -> Int
dist q1 q2 a | not (a `elem` [1,2]) = error "Index is out of range" 
             | a == 1 = fromEnum (fst q2) - fromEnum (fst q1)
             | a == 2 = snd q2 - snd q1
