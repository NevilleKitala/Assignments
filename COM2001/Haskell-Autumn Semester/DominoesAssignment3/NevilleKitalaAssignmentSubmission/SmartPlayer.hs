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
 smartPlayer1 :: DomsPlayer
 
 smartPlayer1 hand InitBoard player scores
  |member (5,4) hand  == True = ((5,4),L)
  |otherwise = hsdPlayer hand InitBoard player scores
 
 smartPlayer1 hand c player scores@(s1,s2)
  |a < 9 && length pd > 0 = hsdPlayer pd c player scores
  |losing (s1,s2) player == True = hsdPlayer oppStitch c player scores
  |similar (h1,h2) hand == True = ((h1,h2),end)
  |similar (h1,h2) hand == False = smartPlayer1 newHand c player scores
  |otherwise = ((h1,h2),end)
  where 
   a = remScore player scores
   ((h1,h2),end) = hsdPlayer hand c player scores
   newHand = remove (h1,h2) hand
   oppStitch = stitch c hand player
   pd = pickDom (findDom (listScore hand c) hand a)
 
 testPlayers :: DomsPlayer
 
 testPlayers hand InitBoard player scores
  |member (5,4) hand  == True = ((5,4),L)
  |otherwise = hsdPlayer hand InitBoard player scores
 
 testPlayers hand c player scores@(s1,s2)
  |losing (s1,s2) player == True = hsdPlayer oppStitch c player scores
  |losing (s1,s2) player == True = hsdPlayer oppStitch c player scores
  |similar (h1,h2) hand == True = ((h1,h2),end)
  |similar (h1,h2) hand == False = smartPlayer1 newHand c player scores
  |a < 9 && length pd > 0 = hsdPlayer pd c player scores
  |otherwise = ((h1,h2),end)
  where 
   a = remScore player scores
   ((h1,h2),end) = hsdPlayer hand c player scores
   newHand = remove (h1,h2) hand
   oppStitch = stitch c hand player
   pd = pickDom (findDom (listScore hand c) hand a)
---------------------------------------------------------------------
 --find out how many points left to close the game
 remScore :: Player->Scores->Int
 
 remScore player scores@(p1,p2)
  |player == P1 = 61 - p1
  |player == P2 = 61 - p2
  |otherwise = 0
  
 --------------------------------------------------------------------
 --define a loosing function to tell the smart player it is losing
 losing :: Scores->Player->Bool
  
 losing (0,0) _ = False
  
 losing (h,t) p
  |p == P1 && t>h && t>52 = True
  |p == P2 && h>t && h>52 = True
  |otherwise = False
   
 --------------------------------------------------------------------
 --Returns a list of dominoes from a hand containing a cerain value.
 contain :: Hand->Int->Hand
 
 contain [] _ = []
 
 contain a@(h:t) b
  |l1 == b || l2 == b = h: contain t b
  |otherwise = contain t b
  where
   (l1,l2) = h
 --------------------------------------------------------------------
 --Checks if opponent is knocking and then get the dominoes that make it knock.
 stitch :: DomBoard->Hand->Player->[Dom]
 
 stitch InitBoard hand player= []
 
 stitch (Board (l1,l2) (r1,r2) h) hand player
  |player1 == player = contain hand l1 ++ contain hand r2 
  |otherwise = hand
  where
  (d,player1,_) = last h
  (i1,i2) = d
 --------------------------------------------------------------------
 --
 
 listScore :: Hand->DomBoard->[Int]
 
 listScore [] _ = []
 
 listScore (h:t) b 
  |goesLP h b== True = scoreDom h L b:listScore t b
  |goesRP h b== True = scoreDom h R b:listScore t b
  |otherwise = 0:listScore t b

 findDom :: [Int]->[Dom]->Int->[(Dom,Int)]
  
 findDom a b c= 
  let 
   d = filter(\(_,n) ->(n == c))(zip b a)
  in
   d
   
 pickDom :: [(Dom,Int)]->[Dom]
 
 pickDom [] = []
 
 pickDom (h:t) = a : pickDom t
  where
  (a,_) = h