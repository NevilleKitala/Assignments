data End = LeftEnd | RightEnd | None
    deriving (Eq, Show)

--Creates a domino

type Domino = (Int, Int)

--function to reverse a domino

reverseD :: Domino-> Domino
reverseD (a, b) = (b, a)

--Board to which dominos will be played

type Board = [Domino]

--creates a hand of dominos for the player
  
type Hand = [Domino]

--Creates a deck of dominos

type Deck = [Domino]
deck :: Deck
deck = [(0,0),(0,1),(0,2),(0,3),(0,5),(0,6),(1,1),
    (1,2),(1,3),(1,5),(1,6),(2,2),(2,3),(2,5),
    (2,6),(3,3),(3,5),(3,6),(4,4),(4,5),(4,6),
    (5,5),(5,6),(6,6)]

--Check if a domino can be played on either parts of the board

goesP :: Domino -> Board ->End ->Bool
goesP a [] c= False
goesP a (h:t) c
    |c == LeftEnd && snd a == fst h = True
    |c == LeftEnd && fst a == fst h = True
    |c == RightEnd && fst a == snd (last t) = True
    |c == RightEnd && snd a == snd (last t) = True
    |otherwise = False

--Check if the player has a playable domino in hand 	

knockingP :: Hand -> Board -> Bool
knockingP a [] = False
knockingP [] a = True
knockingP (h1:t1)(h2:t2)
    |goesP h1 (h2:t2) LeftEnd= False 
    |goesP h1 (h2:t2) RightEnd= False 
    |otherwise = knockingP t1 (h2:t2)

--Check if a domino has already been played on the board

playedP :: Domino -> Board -> Bool
playedP a [] = False
playedP a (h:t)
    |a == h = True
    |otherwise = playedP a t

--checking which dominos can be played at either end of the board

possPlays :: Hand -> Board -> [(Domino, End)]
possPlays [] a= []
possPlays (h1:t1)(h2:t2) 
    |snd h1 == fst h2 =  possPlays t1 (h2:t2) ++ 
        [(h1, LeftEnd)]
    |fst h1 == snd(last t2) =  possPlays t1 (h2:t2)
        ++ [(h1, RightEnd)]
    |fst h1 == fst h2 =  possPlays t1 (h2:t2) ++ 
        [((reverseD h1), LeftEnd)]
    |snd h1 == snd(last t2) =  possPlays t1 (h2:t2)
        ++ [((reverseD h1), RightEnd)]
    |otherwise = possPlays t1 (h2:t2)

-- Check if a domino can be played at the end of a board

playDom :: Domino-> Board -> End -> Maybe Board
playDom a b c 
    |c == LeftEnd  && goesP a b c == True = Just(a:b)
    |c == RightEnd && goesP a b c == True = Just(b ++ [a])
    |otherwise = Nothing

--Check score on the provided board

scoreBoard :: Board -> Int 
scoreBoard [] = 0
scoreBoard a 
    |fst(head a) == snd(head a) && ((fst(head a)
        * 2 + snd(last a)) `mod` 3) == 0 && 
        ((fst(head a) * 2 + snd(last a)) `mod` 5)
        == 0 =  (((fst(head a))*2 + snd(last a))
        `div` 3) + (((fst(head a))*2 + snd(last a))
        `div` 5)
    |fst(last a) == snd(last a) && ((fst (head a)
        + snd(last a) *2) `mod` 3) == 0 &&
        ((fst(head a) * 2 + snd(last a))
        `mod` 5) == 0 = ((fst(head a) +
        (snd(last a))*2) `div` 3) +
        (((fst(head a))*2 + snd(last a)) `div` 5)
    |((fst(head a) + snd(last a)) `mod` 3) == 0
        && ((fst(head a) + snd(last a)) `mod` 5)
        == 0 =(fst(head a) + (snd(last a))
        `div` 3) + (((fst(head a)) + 
        snd(last a)) `div` 5)
    |fst(head a) == snd(head a) && ((fst(head a)
        * 2 + snd(last a)) `mod` 3) == 0 = 
        (((fst(head a))*2 + snd(last a)) `div` 3)
    |fst(last a) == snd(last a) && ((fst (head a)
        + snd(last a) *2 ) `mod` 3) == 0 =  
        ((fst(head a) + (snd(last a))*2) `div` 3)
    |((fst(head a) + snd(last a)) `mod` 3) == 0 
        =(fst(head a) + (snd(last a)) `div` 3)
    |fst(head a) == snd(head a) && 
        ((fst (head a) *2 + snd(last a)) 
        `mod` 5) == 0 = (fst(head a)*2 + 
        snd(last a)) `div` 5
    |fst(last a) == snd(last a) && 
        ((fst(head a) + snd(last a) *2)
        `mod` 5) == 0 = (fst(head a) +
        snd(last a)*2) `div` 5
    |((fst (head a) + snd(last a)) 
        `mod` 5) == 0  = (fst(head a) + 
        snd(last a)) `div` 5
    |otherwise = 0

{-Returns a list of dominos that would give the 
score provided in the function and the end to 
which they would be played-}

scoreDeck :: Board -> Deck -> Int -> [(Domino,End)]
scoreDeck _ [] _ = []
scoreDeck a (h:t) b 
    |playedP h a == False && goesP h a LeftEnd ==
        True && (b*3 - snd(last a) == 2*fst h) && 
        fst h == snd h = [((h), LeftEnd)] ++ 
        scoreDeck a t b
    |playedP h a == False && goesP h a LeftEnd == 
        True && (b*5 - snd(last a) == 2 * fst h) 
        && fst h == snd h = [((h), LeftEnd)] ++ 
        scoreDeck a t b
    |playedP h a == False && goesP h a LeftEnd == 
        True && (b*3 - snd(last a) == fst h) = 
        [((h), LeftEnd)] ++ scoreDeck a t b
    |playedP h a == False && goesP h a LeftEnd == 
        True && (b*5 - snd(last a) == fst h) = 
        [((h), LeftEnd)] ++ scoreDeck a t b
    |playedP h a == False && goesP h a RightEnd == 
        True && (b*3 - fst(head a) == 2*fst h) && 
        fst h == snd h = [((h), RightEnd)]++
        scoreDeck a t b
    |playedP h a == False && goesP h a RightEnd ==
        True && (b*5 - fst(head a) == 2*fst h) &&
        fst h == snd h = [((h), RightEnd)]++ 
        scoreDeck a t b
    |playedP h a == False && goesP h a RightEnd ==
        True && (b*3 - fst(head a) == snd h) = 
        [((h), RightEnd)] ++ scoreDeck a t b
    |playedP h a == False && goesP h a RightEnd ==
        True && (b*5 - fst(head a) == snd h) = 
        [((h), RightEnd)] ++ scoreDeck a t b
    |otherwise = scoreDeck a t b

-- calls the function score deck


scoreN :: Board -> Int -> [(Domino, End)]
scoreN [] a = []
scoreN a b 
    |b > 0 = scoreDeck a deck b
    |otherwise = []