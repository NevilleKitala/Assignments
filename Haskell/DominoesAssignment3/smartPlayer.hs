module SmartPlayer where
 import DomsMatch

{-Assignment3
 Creating a smart player
 Make a few tactics that can be used by the player
 codding from:
 01/12/2017
 to:
 15/12/2017
 -}

 type Tactics = [Dom]->Hand->History->Dom
---------------------------------------------------------------------
 --function to find members in a list of dominoes
 
 member :: Dom->[Dom]->Bool
 
 member a (h:t)
  |a == h = True
  |otherwise = member a t
 
---------------------------------------------------------------------
 --function to find similar dominoes
 similar :: Dom->[Dom]->[Dom]
 
 similar _ [] = []
 
 similar a (h:t)
  |b == d || c == d = h:similar a t
  |c == e || b == e = h:similar a t
  |otherwise = similar a t
  where
  (b,c) = a
  (d,e) = h
---------------------------------------------------------------------
 --see if the domino has been played or is in the players hand
 possibleDoms :: [Dom]->Hand->Hand->[Dom]

 
 possibleDoms a@(h1:t1) b@(h2:t2) c@(h3:t3)
  |member h1 b == False || member h1 c == False = possibleDoms (remove h1 a) b c
  |member h1 b == True || member h1 c == True = h1:possibleDoms t1 b c
  |otherwise = possibleDoms t1 b c

---------------------------------------------------------------------
 --function to remove a domino from a list of dominoes
 remove :: Eq a => a->[a]->[a]
 remove _ [] = []   
 remove x (h:t)         
  |x==h = remove x t       
  |otherwise = (h:remove x t)
 
--------------------------------------------------------------------=
 --function to adding some tactics to be played
 tactic1 :: Tactics
 
 tactic1 board playerHand opponent 
  |if board = 