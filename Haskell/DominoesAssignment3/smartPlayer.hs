 import DomsMatch

{-Assignment3
Creating a smart player
Make a few tactics that can be used by the player
codding from:
01/12/2017
to:
15/12/2017
-}

 type Tactics = DomBoard->Hand->History->Dom
---------------------------------------------------------------------
 --function to find members in a list of dominoes
 
 member :: Dom->DomBoard->Bool
 
 member _ [] = false
 
 member a (h:t)
  |a == h = true
  |otherwise = member a t
 
---------------------------------------------------------------------
 --function to find similar dominoes
 similar :: Dom->[(Dom)]->[(Dom)]
 
 similar a domset@(h:t)
  |fst a == fst h || snd a == fst h = h ++ similar a t
  |snd h == snd h || fst a == snd h = h ++ similar a t
  |otherwise = similar a t
---------------------------------------------------------------------
 possibleDoms :: DomBoard->[(Dom)]

 possibleDoms [] = [()]