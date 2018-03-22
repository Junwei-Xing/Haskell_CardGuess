{-
 -- This projejct is an implementation of Card Game for project 2

 -- About the game
   -- This project implement a two-player logical guessing game
   -- Each player has a complete standard deck of western playing cards without Jokers
   -- User will act as a answer, who will begin the game by selecting some number of cards from a deck
     -- The cards selected by the answerer will form the answer for this game
   -- Then program will act as a guesser, who will try to guess the answer based on feedback for each guess

 -- About implementation strategy 
   -- Use GameState to hold all possible guesses, and filter the guesses based on feedback from each guess.
   -- Initial game state is the mathematical combination of all cards from a deck,
   -- then choose first element in GameState as next guess, then use feedback to further filter possible guesses list.
-}

module Proj2 (feedback, initialGuess, nextGuess, GameState) where
import Card
import Data.List

-- use GameState to record game state, which contains a list of all possible guesses
-- GameSate will be updated after each guess based on feedback
type GameState = [[Card]]

-- take the answer and a guess, each represented as a list of Cards
-- returns the five feedback numbers as required in specification
feedback:: [Card] -> [Card] -> (Int, Int, Int, Int, Int)
feddback [] [] = (0, 0, 0, 0, 0)
feedback answer guess = (f1, f2, f3 ,f4, f5)
  where 
    f1 = countCorrectCards guess answer
    f2 = countLowerRankCards guess answer
    f3 = countSameRankCards guess answer 
    f4 = countHigherRankCards guess answer
    f5 = countSameSuitCards guess answer

-- Calculate the number of Cards in the answer which are also in the guess
countCorrectCards :: [Card] -> [Card] -> Int
countCorrectCards [] _ = 0
countCorrectCards (g:gs) answer
  | elem g answer 
       -- remove the same card from answer in case of duplicate
     = 1 + countCorrectCards gs (removeCard g answer)
  | otherwise = countCorrectCards gs answer

-- remove the card from a cards list
removeCard :: Card -> [Card] -> [Card]
removeCard _ [] = []
removeCard t (c:cs)
  | t == c = cs
  | otherwise = c: removeCard t cs


-- Calculate the number of cards in the answer which have rank lower than the lowest rank in the guess
countLowerRankCards :: [Card] -> [Card] -> Int
countLowerRankCards _ [] = 0
countLowerRankCards guess (an:ans)
  | rank an < lowestRank = 1 + countLowerRankCards guess ans
  | otherwise = countLowerRankCards guess ans
  where lowestRank = minimum (map rank guess)


-- Calculate the number of cards in the answer which have the same rank as a card in the guess
countSameRankCards :: [Card] -> [Card] -> Int
countSameRankCards [] _ = 0
countSameRankCards (g:gs) answer
  | hasSameRankCard g answer == True 
      -- remove card with same rank in the list in case of duplication
        = 1 + countSameRankCards gs (removeCardByRank g answer)
  | otherwise 
        = countSameRankCards gs answer

-- find if there is card which has same rank with a given card
-- if there is card has same rank, return True;
-- otherwise, return False
hasSameRankCard :: Card -> [Card] -> Bool
hasSameRankCard _ [] = False
hasSameRankCard c (t:ts)
  | rank c == rank t  = True
  | otherwise = hasSameRankCard c ts

-- remove first element from a list of Cards which has equal rank with target card
-- if there is no card has same rank, the list keep the same
removeCardByRank :: Card -> [Card] -> [Card]
removeCardByRank _ [] = []
removeCardByRank t (c:cs)
  | rank t == rank c = cs
  | otherwise = c: removeCardByRank t cs

-- calculate the number of cards in the answer which have rank higher
-- than the highest rank in the guess
countHigherRankCards :: [Card] -> [Card] -> Int
countHigherRankCards _ [] = 0
countHigherRankCards guess (an:ans)
  | rank an > highestRank = 1 + countHigherRankCards guess ans
  | otherwise = countHigherRankCards guess ans
  where highestRank = maximum (map rank guess)


-- calculate the number of cards in the answer which
-- have the same suit as a card in the guess
countSameSuitCards :: [Card] -> [Card] -> Int
countSameSuitCards [] _ = 0
countSameSuitCards _ [] = 0
countSameSuitCards (g:gs) answer 
  | hasSameSuit g answer == True 
      = 1 + countSameSuitCards gs (removeCardBySuit g answer)
    -- remove card with same suit from the list in case of duplication
  | otherwise 
      = countSameSuitCards gs answer

-- find if there is card which has same suit with a given card
-- if there is card has same suit, return True;
-- otherwise, return False
hasSameSuit :: Card -> [Card] -> Bool
hasSameSuit _ [] = False
hasSameSuit c (t:ts)
  | suit c == suit t  = True
  | otherwise = hasSameSuit c ts

-- remove first element from a list of Cards which has equal suit with target card
-- if there is no card has same suit, the list keep the same
removeCardBySuit :: Card -> [Card] -> [Card]
removeCardBySuit _ [] = []
removeCardBySuit t (c:cs)
  | suit t == suit c = cs
  | otherwise = c : removeCardBySuit t cs

-- this function is used to make initial guess and initialize the GameState
-- let n = the number of the anwser cards,
-- choose the first card list in initial GameSate as initial guess list;
-- when n == 2 or 3 or 4, specify the card list in order to improve efficiency
initialGuess :: Int -> ([Card],GameState)
initialGuess n
    | n == 2  = ([Card Club R5, Card Diamond R10], initialGameState)
    | n == 3  = ([Card Club R4, Card Diamond R8 ,Card Heart Queen], initialGameState)
    | n == 4  = ([Card Club R4, Card Diamond R7 ,Card Heart R10, Card Spade King], initialGameState)
    | otherwise = (head initialGameState, tail initialGameState)
    where initialGameState = createGameState n

-- generate GameState which contains all possible guesses
-- based on the amount of cards in the answer cards
createGameState :: Int -> GameState
createGameState n = filter noSameCardFilter (generateCardsCombination n)

-- filter the combination which contain same card 
-- before generate initial game state
noSameCardFilter :: [Card] -> Bool
noSameCardFilter [] = True
noSameCardFilter (c:cs)
  | cs == [] = True
  | otherwise = (notElem c cs) && noSameCardFilter cs

-- generate all possible combination of cards
-- based on the amount of cards in the answer
generateCardsCombination :: Int -> [[Card]]
-- use sequence function to generate combination
generateCardsCombination n = sequence (generateCardsCollection n)

-- generate a collection of cards list
-- each cards list contains all cards from a deck
generateCardsCollection :: Int -> [[Card]]
generateCardsCollection 0 = []
generateCardsCollection n = allCards: generateCardsCollection (n-1)
  where allCards = [minBound..maxBound]

-- take a pair of the previous guess and game state as well as corresponding feedback as input
-- returns a pair of the next guess and new game state
nextGuess :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)
nextGuess (guess, currentState) (correctNum, lowRankNum, sameRankNum, highRankNum, sameSuitNum) 
  = (head newGameState, tail newGameState)
   -- use feedback to filter impossible guess 
   -- then generate new game state
   where 
     -- game state after filter impossible guess based on correct cards
     newGS_1 = filter (\p -> countCorrectCards guess p == correctNum) currentState

     -- game state after filter impossibe guess based on lower rank feedback
     newGS_2 = filter (\p -> countLowerRankCards guess p == lowRankNum) newGS_1

     -- game state after filter impossible guess based on same rank feedback
     newGS_3 = filter (\p -> countSameRankCards guess p == sameRankNum) newGS_2

     --game state after filter impossible guess based on higher rank feedback
     newGS_4 = filter (\p -> countHigherRankCards guess p == highRankNum) newGS_3

     --game state after filter impossible guess based on same suit feedback
     newGameState = filter (\p -> countSameSuitCards guess p == sameSuitNum) newGS_4
