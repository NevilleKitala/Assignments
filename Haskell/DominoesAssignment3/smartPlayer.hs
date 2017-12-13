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
  |otherwise = similar a t
  where
  (d,e) = h
---------------------------------------------------------------------
 --see if the domino has been played or is in the players hand
 possibleDoms :: DomBoard->Hand->[Dom]->[Dom]
 
 possibleDoms _ [] [] = []
 
 possibleDoms domBoard@(Board (l1,l2) (r1,r2) h) hand domSet@(h1:t)
  |member h1 hand == True = x
  |similar dom domSet == True = x 
  |otherwise = h1 : x
  where
  x = possibleDoms domBoard hand t 
  y = similar dom domSet
  (dom,_,_) = last h
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
   np = notPlayed domSet (played c)
   opp = possibleDoms c hand np
---------------------------------------------------------------------
 --find out how many points left to close the game
 remScore :: Player->Scores->Int
 
 remScore player scores@(p1,p2)
  |player == P1 = 61 - p1
  |player == P2 = 61 - p2
  |otherwise = 0
  
---------------------------------------------------------------------
 notPlayed :: [Dom]->[Dom]->[Dom]
 
 notPlayed _ [] = []
 
 notPlayed a@(h:t) b
  |equal h b = notPlayed t b
  |otherwise = h : notPlayed t b
---------------------------------------------------------------------
 played :: DomBoard->[Dom]
 
 played _ = []
 
 played a@(Board e1 e2 (h1:t1)) = d1 : played (Board e1 e2 t1)
  where
   (d1,_,_) = h1
---------------------------------------------------------------------
 --find if there is a domino equal to the domino in the dom list
 
 equal :: Dom->[Dom]->Bool
 
 equal _ [] = False
 
 equal a@(h1,t1) b@(h:t)
  |h1 == h2 && t2 == t1 = True
  |h2 == t1 && t2 == h1 = True
  |otherwise = equal a t
  where
  (h2,t2) = h
  
 --------------------------------------------------------------------
 --define a loosing function to tell the smart player it is about to loose
  losing :: Scores->Player->Bool
  
  losing score@(h,t) p
   |p == P1 && t>h = True
   |p == P2 && h>t = True
   |otherwise = False