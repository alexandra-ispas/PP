{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

{-
    Ispas Alexandra-Petrina 322CDb
-}

module Basics where
{-
    Expune funcțiile necesare reprezentării jocului.
-}

import ProblemState
import Data.List
import Data.Maybe ( fromJust, isNothing )

import Data.Array as A

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc.
    Colțul stânga-sus este (0, 0).
-}
type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea Target-urilor.
    Acestea conțin informații atât despre poziția curentă a
    Target-ului cât și despre comportamentul acestuia.
    Tipul Behavior este definit mai jos.
-}
data Target = Target {
    position :: Position,
    behavior :: Behavior
}

instance Eq Target where
    Target p1 _ == Target p2 _ = p1 == p2

instance Ord Target where
    Target p1 _ <= Target p2 _ = p1 <= p2

{-
    Tip de date pentru reprezentarea comportamentului unui Target.
    Tipul Behavior este utilizat pentru a modela tranziția Target-urilor
    din starea curentă în starea următoare. Primul parametru este poziția
    actuală a target-ului, iar al doilea, starea curentă a jocului.
    Tipul Game este definit mai jos.
    
    Observați că, din moment ce un Behavior produce un Target nou,
    acesta din urmă ar putea fi caracterizat de un alt Behavior
    decât cel anterior.
-}
type Behavior = Position -> Game -> Target

{-
    Direcțiile de deplasare pe tablă
-}
data Direction = North | South | West | East
    deriving (Eq, Show)

{-
    *** TODO ***
    
    Tip de date pentru reprezentarea stării jocului, la un anumit
    moment. Completați-l cu orice informație aveți nevoie pentru
    stocarea stării jocului (hunter, target, obstacole, gateways).
-}

data Cell = HunterCell | TargetCell | GatewaysCell | ObstaclesCell | BlankCell
    deriving (Eq, Ord)

instance Show Cell
    where
        show HunterCell = "!"
        show TargetCell = "*"
        show GatewaysCell = "#"
        show ObstaclesCell = "@"
        show BlankCell = " "


data Game = G {
    gameBoard :: A.Array Position [Cell],
    gameSize :: (Int, Int),
    gameTargets :: [Target],
    gameGateways :: [(Position, Position)],
    gameHunter :: Position
} deriving (Eq, Ord)

{-
    *** Optional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară,
    instantiați explicit clasele Eq și Ord pentru Game.
    În cazul acesta, eliminați deriving (Eq, Ord) din Game.
-}

{-
    *** TODO ***

    Reprezentați starea jocului ca șir de caractere, pentru afișarea
    la consolă.
    
    Atenție! Fiecare linie, mai puțin ultima, este urmată de \n.
    Celule goale vor fi reprezentate ca ' '.
    Hunter-ul va fi reprezentat ca '!'.
    Target-urile vor fi reprezentate ca '*'
    Gateways-urile vor fi reprezentate ca '#'.
    Obstacolele vor fi reprezentate de '@'.

    Hint: S-ar putea să vă fie utile list comprehensions,
    precum și funcțiile elem, any și intercalate din Data.List.
-}
--head inainte de getelem
instance Show Game
    where
        show (G board (lin, col) _ _ _) =
            intercalate "\n" [concat [getElem x y | y <- [0..col-1]] | x <- [0..lin-1]]
                where
                    getElem x y = show $ head (board A.! (x, y))

gameAsString :: Game -> String
gameAsString = show

{-
    *** TODO ***
    
    Primește numărul de linii și numărul de coloane ale tablei de joc.
    Intoarce un obiect de tip Game în care tabla conține spații goale în interior, fiind
    împrejmuită de obstacole pe toate laturile. Implicit, colțul din stânga sus este (0,0),
    iar Hunterul se găsește pe poziția (1, 1).
-}

emptyGame :: Int -> Int -> Game
emptyGame lin col = G board (lin, col) [] [] (1, 1)
    where board = A.array ((0, 0), (lin-1, col-1)) $ [((x, y), [BlankCell]) | x <- [1..(lin-2)], y<- [1..(col-2)], x+y /= 2]++
                [((x, y), [ObstaclesCell]) | x <- [0, lin-1], y <- [0..col-1]] ++
                [((x, y), [ObstaclesCell]) | x <- [1..lin-2], y <- [0, col-1]] ++ [((1, 1), [HunterCell])]

{-
    *** TODO ***

    Primește o poziție și un joc și întoarce un nou joc, cu Hunter-ul pus
    pe poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugat Hunter-ul
    Daca poziția este invalidă (ocupată sau în afara tablei de joc) se va întoarce
    același joc.
-}

removeElement :: Position->Cell->Game->[Cell]
removeElement pos cel (G board _ _ _ _)
    | board A.! pos == [BlankCell] = [BlankCell]
    | board A.! pos == [cel] = [BlankCell]
    | elem cel $ board A.! pos = delete cel $ board A.! pos
    | otherwise = board A.! pos

addElement :: Position -> Game -> Cell -> [Cell]
addElement poz (G board _ _ _ _) cel
    |   elem TargetCell (board A.! poz) && cel == TargetCell = cel : board A.! poz
    |   elem TargetCell (board A.! poz) && cel == GatewaysCell = cel : board A.! poz
    |   elem GatewaysCell (board A.! poz) && cel == TargetCell = cel : board A.! poz
    |   elem GatewaysCell (board A.! poz) && cel == HunterCell = cel : board A.! poz
    |   elem HunterCell (board A.! poz) && cel == GatewaysCell = cel : board A.! poz
    |   board A.! poz == [BlankCell] = [cel]
    |   otherwise = board A.! poz

addHunter :: Position -> Game -> Game
addHunter poz@(pozl, pozc) game@(G board size@(lin, col) t g _)
    | pozl >= lin || pozl < 0 || pozc >= col || pozc < 0 || board A.! poz /= [BlankCell] = game
    | otherwise = G newBoard size t g poz
        where
            newBoard = helper A.// [(poz, addElement poz game HunterCell)]
                where
                    helper = board A.// [((i, j), removeElement (i, j) HunterCell game) |
                        i <- [0..lin-1], j <- [0..col-1], elem HunterCell $ board A.! (i, j)]


{-
    *** TODO ***

    Primește un comportament, o poziție și un joc și întoarce un nou joc, în care a fost
    adăugat Target-ul descris de comportament și poziție.
    Parametrul Behavior reprezintă comportamentul Hunter-ului care va fi adăugat.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat Target-ul.
-}
addTarget :: Behavior -> Position -> Game -> Game
addTarget newBeh poz game@(G board size t g hunter) = G newBoard size newT g hunter
    where
        newT = Target poz newBeh : t
        newBoard = board A.// [(poz, addElement poz game TargetCell)]

{-
    *** TODO ***

    Primește o pereche de poziții și un joc și întoarce un nou joc, în care au fost adăugate
    cele două gateway-uri interconectate.
    Parametrul (Position, Position) reprezintă pozițiile de pe hartă la care vor fi adăugate 
    cele două gateway-uri interconectate printr-un canal bidirecțional.
-}
addGateway :: (Position, Position) -> Game -> Game
addGateway (poz1, poz2) (G board size t g hunter) = G newBoard size t ((poz1, poz2) : g) hunter
    where newBoard = board A.// [(poz1, addElement poz1 (G board size t g hunter) GatewaysCell),
            (poz2, addElement poz2 (G board size t g hunter) GatewaysCell)]
{-
    *** TODO ***

    Primește o poziție și un joc și întoarce un nou joc, în care a fost adăugat un obstacol
    la poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat obstacolul.
-}
addObstacle :: Position -> Game -> Game
addObstacle poz@(pozl, pozc) (G board size@(lin, col) t g hunter)
    |   pozl >= lin || pozl < 0 || pozc >= col || pozc < 0 = G board size t g hunter
    |   otherwise = G newBoard size t g hunter
        where
            newBoard = board A.// [(poz, [ObstaclesCell])]
{-
    *** TODO ***
    
    Primește o poziție destinație înspre care vrea să se deplaseze o entitate (Hunter sau Target)
    și verifică daca deplasarea este posibilă, întorcând noua poziție, luând în considerare
    și Gateway-urile.
    Avem următoarele cazuri:
    - dacă poziția corespunde unui spațiu gol, se întoarce acea poziție;
    - dacă poziția corespunde unui gateway, se întoarce poziția gateway-ului pereche;
    - dacă poziția corespunde unui obstacol, se întoarce Nothing.
    Parametrul Position reprezintă poziția destinație.
-}

getMatchingGateway :: Position -> [(Position, Position)] -> Maybe Position
getMatchingGateway poz gates = Just $ head [x | x <- [fst list, snd list],  x /= poz]
    where
        list = head $ filter (\x-> fst x == poz || snd x == poz) gates


attemptMove :: Position -> Game -> Maybe Position
attemptMove poz@(pozl, pozc) (G board (lin, col) _ gates _)
    | pozl >= lin || pozl < 0 || pozc >= col || pozc < 0 = Nothing
    | board A.! poz == [BlankCell] = Just poz
    | elem GatewaysCell $ board A.! poz  = getMatchingGateway poz gates
    | otherwise = Nothing

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre est. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
    
    Conform definiției, tipul Behavior corespunde tipului funcție
    Position -> Game -> Target.
    
    Având în vedere că cele patru funcții definite în continuare (goEast, goWest,
    goNorth, goSouth) sunt foarte similare, încercați să implementați o funcție
    mai generală, pe baza căreia să le definiți apoi pe acestea patru.
-}

checkNextPosition :: Position->Direction->Game-> Maybe Position
checkNextPosition (x, y) dir game@(G _ (lin, col) _ _ _)
    | dir == South && x+1 <= lin - 1 = attemptMove (x+1, y) game
    | dir == North && x - 1 >= 0 = attemptMove (x-1, y) game
    | dir == East && y+1 <= col - 1 = attemptMove (x, y+1) game
    | dir == West && y - 1 >= 0 = attemptMove (x, y-1) game
    | otherwise = Nothing

go :: Direction -> Behavior
go dir poz (G board size t g hunter)
    |   isNothing nextPos = if elem GatewaysCell $ board A.! poz 
                            then Target (fromJust $ getMatchingGateway poz g) $ go dir
                            else Target poz $ go dir
    |   otherwise = Target (fromJust nextPos) $ go dir
    where
        nextPos = checkNextPosition poz dir (G board size t g hunter)

goEast :: Behavior
goEast = go East

{-.
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre vest. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}

goWest :: Behavior
goWest = go West

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre nord. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}

goNorth :: Behavior
goNorth = go North

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre sud. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}

goSouth :: Behavior
goSouth = go South

{-
    *** TODO ***

    Comportamentul unui Target de a-și oscila mișcarea, când înspre nord, când înspre sud. 
    Mișcarea se poate face doar dacă poziția este validă (se află pe tablă de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul iși va schimba
    direcția de mers astfel:
    - daca mergea inspre nord, își va modifica direcția miscării înspre sud;
    - daca mergea inspre sud, își va continua mișcarea înspre nord.
    Daca Target-ul întâlneste un Gateway pe traseul său, va trece prin acesta,
    către Gateway-ul pereche conectat și își va continua mișcarea în același sens la ieșire
    din acesta.
    Puteți folosit parametrul Int pentru a surprinde deplasamentul Target-ului (de exemplu,
    1 pentru sud, -1 pentru nord).
-}

bounce :: Int -> Behavior
bounce dir poz@(i, j) game
    |   dir == 1 = case nextPozS of
            Nothing -> Target (i - 1, j) (bounce (-1))
            _ -> Target (fromJust nextPozS) (bounce 1)

    |   otherwise = case nextPozN of
            Nothing -> Target (i + 1, j) (bounce 1)
            _ -> Target (fromJust nextPozN) (bounce (-1))
    where
        nextPozS = checkNextPosition poz South game
        nextPozN = checkNextPosition poz North game

{-
    *** TODO ***
    Funcție care mută toate Target-urile din Game-ul dat o poziție, în functie
    de behavior-ul fiecăreia și întoarce noul Game în care pozițiile Target-urilor
    sunt actualizate.
    
-}

moveTargets :: Game -> Game
moveTargets game@(G board size target gateway hunter) = G newBoard size newTarget gateway hunter
    where
        -- sunt eliminate toate target-urile din joc
        auxBoard = board A.// [(poz, removeElement poz TargetCell game) | (Target poz _) <- target]
        -- se face update la lista de target-uri
        newTarget = [beh pos game | (Target pos beh) <- target]
        -- se muta target-urile
        newBoard = auxBoard A.// [(poz, addElement poz game TargetCell) | (Target poz _) <- newTarget]

{-
    *** TODO ***

    Verifică dacă Targetul va fi eliminat de Hunter.
    Un Target este eliminat de Hunter daca se află pe o poziție adiacentă
    cu acesta.
    Parametrul Position reprezintă poziția Hunterului pe tabla
    de joc.
    Parametrul Target reprezintă Targetul pentru care se face verificarea. 
-}

isTargetKilled :: Position -> Target -> Bool
isTargetKilled (lin, col) (Target (linT, colT) _)
    |   abs(linT - lin) == 1 && col == colT = True
    |   abs(colT - col) == 1 && lin == linT = True
    |   otherwise = False
{-
    *** TODO ***

    Avansează starea jocului curent, rezultând starea următoare a jocului.
    Parametrul Direction reprezintă direcția în care se va deplasa Hunter-ul.
    Parametrul Bool specifică dacă, după mutarea Hunter-ului, vor fi
    mutate și Target-urile sau nu, și dacă vor fi eliminate din joc sau nu.
    Este folosit pentru a distinge între desfășurarea reală a jocului (True)
    și planificarea „imaginată” de hunter (False) în partea a doua a temei.

    Avansarea stării jocului respectă următoarea ordine:
    1. Se deplasează Hunter-ul.
    2. În funcție de parametrul Bool, se elimină Target-urile omorâte de către Hunter.
    3. In funcție de parametrul Bool, se deplasează Target-urile rămase pe tablă.
    4. Se elimină Targeturile omorâte de către Hunter și după deplasarea acestora.
    
    Dubla verificare a anihilării Target-urilor, în pașii 2 și 4, îi oferă Hunter-ului
    un avantaj în prinderea lor.
-}
advanceGameState :: Direction -> Bool -> Game -> Game
advanceGameState dir kill game@(G _ size target gateway hunter)
    |   isNothing nextPoz = game
    |   otherwise = G newBoard size newTarget gateway $ fromJust nextPoz
    where
        nextPoz = checkNextPosition hunter dir game
        game1@(G board1 _ _ _ _) = addHunter (fromJust nextPoz) game

        remainedTargets = if kill 
                          then [x | x <- target, not $ isTargetKilled (fromJust nextPoz) x] 
                          else target

        eliminatedTargets = [x | x <- target, isTargetKilled (fromJust nextPoz) x]

        board2 = if kill 
                then board1 A.// [(poz, removeElement poz TargetCell game1) | (Target poz _) <- eliminatedTargets] 
                else board1 

        game2 = G board2 size remainedTargets gateway $ fromJust nextPoz
        game3@(G board3 _ target1 _ _) = if kill 
                                        then moveTargets game2 
                                        else game2
        
        -- se mai scot cele eliminate inca odata
        newTarget = if kill 
                    then [x | x <- target1, not $ isTargetKilled (fromJust nextPoz) x] 
                    else target1
        eliminatedTargets1 = [x | x <- target1, isTargetKilled (fromJust nextPoz) x]
        newBoard = if kill 
                   then board3 A.// [(poz, removeElement poz TargetCell game3) | (Target poz _) <- eliminatedTargets1] 
                   else board3 
{-
    ***  TODO ***

    Verifică dacă mai există Target-uri pe table de joc.
-}
areTargetsLeft :: Game -> Bool
areTargetsLeft (G _ _ target _ _) = not $ null target

{-
    *** BONUS TODO ***

    Comportamentul unui Target de a se deplasa în cerc, în jurul unui Position, având
    o rază fixată.
    Primul parametru, Position, reprezintă centrul cercului.
    Parametrul Int reprezintă raza cercului.
    Puteți testa utilizând terenul circle.txt din directorul terrains, în conjuncție
    cu funcția interactive.
-}
circle :: Position -> Int -> Behavior
circle = undefined

instance ProblemState Game Direction where
    {-
        *** TODO ***
        
        Generează succesorii stării curente a jocului.
        Utilizați advanceGameState, cu parametrul Bool ales corespunzător.
    -}
    successors game = [(dir, games) | dir <- [North, South, East, West], games <- [advanceGameState dir False game]]

    {-
        *** TODO ***
        
        Verifică dacă starea curentă este una în care Hunter-ul poate anihila
        un Target. Puteți alege Target-ul cum doriți, în prezența mai multora.
    -}
    isGoal (G _ _ target _ hunter) = not $ null $ filter (isTargetKilled hunter) target

    {-
        *** TODO ***
        
        Euristica euclidiană (vezi hEuclidian mai jos) până la Target-ul ales
        de isGoal.
    -}
    h game@(G _ _ target _ hunter) = if isGoal game 
                                     then hEuclidean hunter targetPoz
                                     else 0
        where
            (Target targetPoz _) = head $ filter (isTargetKilled hunter) target

{-
     ** NU MODIFICATI **
-}
hEuclidean :: Position -> Position -> Float
hEuclidean (x1, y1) (x2, y2) = fromIntegral $ (x1 - x2) ^ pow + (y1 - y2) ^ pow
  where
    pow = 2 :: Int

{-
    *** BONUS ***

    Acesta reprezintă un artificiu necesar pentru testarea bonusului,
    deoarece nu pot exista două instanțe diferite ale aceleiași clase
    pentru același tip.

    OBSERVAȚIE: Testarea bonusului pentru Seach este făcută separat.
-}

newtype BonusGame = BonusGame Game
    deriving (Eq, Ord, Show)

{-
    *** BONUS TODO ***

    Folosind wrapper-ul peste tipul Game de mai sus instanțiați
    ProblemState astfel încât să fie folosită noua euristică. 
-}
instance ProblemState BonusGame Direction where
    {-
        *** BONUS TODO ***

        Pentru a ne asigura că toțî succesorii unei stări sunt de tipul
        BonusGame și folosesc noua euristică trebuie să aplicăm wrapper-ul
        definit mai sus peste toți succesorii unei stări.

        Hint: Puteți să folosiți funcția fmap pe perechi pentru acest lucru.
        https://wiki.haskell.org/Functor
    -}
    successors = undefined

    {-
        *** BONUS TODO ***

        Definiți funcția isGoal pentru BonusGame.

        Hint: Folosiți funcția isGoal deja implementată pentru tipul Game.
    -}
    isGoal = undefined

    {-
        *** BONUS TODO ***

        Definiți o funcție euristică care este capabilă să găsească un drum mai scurt
        comparativ cu cel găsit de euristica implementată pentru Game.

        ATENȚIE: Noua euristică NU trebuie să fie una trivială.

        OBSERVAȚIE: Pentru testare se va folosi fișierul terrains/game-6.txt.
    -}
    h = undefined
