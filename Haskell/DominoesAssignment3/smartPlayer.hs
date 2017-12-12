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

---------------------------------------------------------------------
 --function to find members in a list of dominoes
 
 member :: Dom->[Dom]->Bool
 
 member a (h:t)
  |a == h = True
  |otherwise = False
 
---------------------------------------------------------------------
 --function to find similar dominoes
 similar :: Dom->[Dom]->Bool
 
 similar _ [] = False
 
 similar a@(b,c) (h:t)
  |b == d || c == d = True
  |b == e || c == e = True
  |otherwise = False
  where
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
  |x==h = t       
  |otherwise = (h:remove x t)
 
---------------------------------------------------------------------
 --function to adding some tactics to be played
 {-using b to stand for the history body and n to represent the
 newest addition to history-}
 smartPlayer :: DomsPlayer
 
 smartPlayer hand InitBoard player scores
  |member (5,4) hand  == True = ((5,4),L)
  |otherwise = hsdPlayer hand InitBoard player scores
 
 smartPlayer hand c@(Board (l1,l2) (r1,r2) h@(b:n)) player scores@(s1,s2)
  |a == sp = ((h1,h2),end)
  |similar (h1,h2) hand == True = ((h1,h2),end)
  |similar (h1,h2) hand == False = smartPlayer newHand c player scores
  |otherwise = ((h1,h2),end)
  where 
   sp = if(player == P1) then s1 else s2
   a = remScore player scores
   ((h1,h2),end) = hsdPlayer hand c player scores
   newHand = remove (h1,h2) hand
 --------------------------------------------------------------------
 --find out how many points left to close the game
 remScore :: Player->Scores->Int
 
 remScore player scores@(p1,p2)
  |player == P1 = 61 - p1
  |player == P2 = 61 - p2
  |otherwise = 0
  
 --------------------------------------------------------------------
 