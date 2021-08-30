{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where

import ProblemState
import qualified Data.PSQueue as PQ
import Data.Maybe
import Prelude
import qualified Data.Set as S

{-
    *** TODO ***
    Tipul unei nod utilizat în procesul de căutare. Recomandăm reținerea unor
    informații legate de:
    * stare;
    * acțiunea care a condus la această stare;
    * nodul părinte, prin explorarea căruia a fost obținut nodul curent;
    * adâncime;
    * estimarea costului până la starea finală;
    * copiii, ce vor desemna stările învecinate;
-}

data Node s a = Node {
    state :: s,
    action :: Maybe a,
    parent :: Maybe (Node s a),
    depth :: Int,
    cost :: Float,
    children :: [Node s a]
} 

{-
    *** TODO ***
    Instanțiați Eq și Ord pe baza stării.
-}

instance Eq s => Eq (Node s a) where
   Node s1 _ _ _ _ _ == Node s2 _ _ _ _ _ = s1 == s2

instance Ord s => Ord (Node s a) where
    Node s1 _ _ _ _ _ <= Node s2 _ _ _ _ _ = s1 <= s2

{-
    *** TODO ***
    Gettere folosite pentru accesul la câmpurile nodului
-}

nodeState :: Node s a -> s
nodeState (Node st _ _ _ _ _) = st

nodeParent :: Node s a -> Maybe (Node s a)
nodeParent (Node _ _ par _ _ _) = par

nodeDepth :: Node s a -> Int
nodeDepth (Node _ _ _ dep _ _) = dep

nodeChildren :: Node s a -> [Node s a]
nodeChildren (Node _ _ _ _ _ chil) = chil

nodeHeuristic :: Node s a -> Float
nodeHeuristic (Node _ _ _ dep c _) = c - fromIntegral dep

nodeAction :: Node s a -> Maybe a
nodeAction (Node _ act _ _ _ _) = act

{-
    *** TODO ***
    Generarea întregului spațiu al stărilor.
    Primește starea inițială și creează nodul corespunzător acestei stări,
    având drept copii nodurile succesorilor stării curente, și așa mai
    departe, recursiv.
-}

makeStates :: (ProblemState s a) => s -> a -> Maybe (Node s a) -> Int -> Node s a
makeStates s a par dep = node
  where
    node = Node s (Just a) par dep (h s + fromIntegral dep) newSuccs
    newSuccs = map (\(st, act) -> makeStates act st (Just node) (dep + 1)) $ successors s

createStateSpace :: (ProblemState s a) => s -> Node s a
createStateSpace initialState = node
    where
        node = Node initialState Nothing Nothing 0 (h initialState) newSuccs
        newSuccs = map (\(st, act) -> makeStates act st (Just node) 1) $ successors initialState

{-
    Funcție ce primește o coadă de priorități și întoarce o pereche
    formată din cheia cu prioritatea minimă și coada din care a fost ștearsă
    aceasta.
    Hint: O puteți folosi pentru a extrage și a șterge un nod din frontieră.
-}

deleteFindMin :: (Ord k, Ord p) => PQ.PSQ k p -> (k, PQ.PSQ k p)
deleteFindMin pq = (minK, pq')
    where minK = PQ.key $ fromJust $ PQ.findMin pq
          pq' = PQ.deleteMin pq

-- {-
--     *** TODO ***
--     Primește nodul curent și mulțimea stărilor vizitate și întoarce
--     o listă cu nodurile succesor nevizitate, care ar putea fi introduse
--     în frontieră.
-- -}

suitableSuccs :: (ProblemState s a, Ord s) => Node s a -> S.Set s -> [Node s a]
suitableSuccs (Node _ _ _ _ _ chil) visited = result
    where
        result = filter (\(Node st _ _ _ _ _) -> not $ S.member st visited) chil
{-
    *** TODO ***
    Primește o frontieră (o coadă de priorități) și un nod ce trebuie inserat în aceasta,
    întorcând o nouă frontieră.
    ATENȚIE: Dacă la introducerea unui nod exiă deja în frontieră un alt nod cu aceeași
    stare, dar cu cost mai mare, nodul nou, cu cost mai mic îl va înlocui pe cel vechi.
    
    Hints:
    1. Vedeți funcția insertWith din pachetul PSQueue.
        (https://hackage.haskell.org/package/PSQueue-1.1.0.1/docs/Data-PSQueue.html#v:insertWith)
    2. Costul se calculează ca suma dintre adâncime și euristică.
-}

insertSucc :: (ProblemState s a, Ord s) => PQ.PSQ (Node s a) Float -> Node s a -> PQ.PSQ (Node s a) Float
insertSucc frontier node@(Node st _ _ _ c _)
    |   null nod = PQ.insert node c frontier
    |   null costb = frontier
    |   not $ null costb = PQ.insert node c (PQ.delete (fromJust aux) frontier)
    |   otherwise = PQ.insert node c frontier
    where
        nod = filter (\x -> let (Node stx _ _ _ _ _) = PQ.key x in stx == st) $ PQ.toAscList frontier
        costb = filter (\x -> c < PQ.prio x) nod
        aux = case costb of
            [] -> Nothing
            _ -> Just (PQ.key $ head costb)
{-
    *** TODO ***
    Primește nodul curent, frontiera și mulțimea stărilor vizitate, întorcând noua
    frontieră (coadă de priorități) în care au fost adăugate nodurile succesor validate
    de suitableSuccs.
-}

insertSuccs :: (ProblemState s a, Ord s) => Node s a -> PQ.PSQ (Node s a) Float -> S.Set s -> PQ.PSQ (Node s a) Float
insertSuccs node frontier visited = foldr (\x@(Node _ _ _ _ costx _) acc -> PQ.insert x costx acc) frontier notvis
    where
        notvis = suitableSuccs node visited

{-
    *** TODO ***
    Funcție helper care implementează A-star.
    Primește o mulțime de noduri vizitate și o coadă de priorități (aka frontiera) și
    întoarce starea finală.
    Se procedează astfel până la întâlnirea unei stări scop:
        - se extrage un nod adecvat din frontireră
        - se marchează starea acestuia ca fiind vizitată
        - se introduc succesorii în frontieră
-}

astar' :: (ProblemState s a, Ord s) => S.Set s -> PQ.PSQ (Node s a) Float -> Node s a
astar' visited frontier
    |   isGoal st = nod
    |   otherwise = astar' newVisited newFrontier
    where 
        (nod@(Node st _ _ _ _ _), auxFrontier) = deleteFindMin frontier
        newVisited = S.insert st visited
        newFrontier = insertSuccs nod auxFrontier newVisited
{-
    *** TODO ***
  
    Primește starea inițială și întoarce starea finală pentru o singură aplicare
    a algoritmului.
    Asigură parametrii inițiali corecți pentru aplicarea funcției astar'.
-}

astar :: (ProblemState s a, Ord s) => Node s a -> Node s a
astar initialNode@(Node _ _ _ _ c _) = astar' S.empty (PQ.insert initialNode c PQ.empty) -- goalNode

{-
    *** TODO ***
    Pornind de la un nod, reface parțial calea către nodul inițial, urmând legăturile
    către părinți.
    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea următoare
    stării inițiale și se încheie la starea finală.
    ATENȚIE: Nodul inițial este singurul exclus!
-}

extractPath :: Node s a -> [(a, s)]
extractPath node 
    | nodeDepth node <= 0 = []
    | otherwise = reverse path
  where
    parents = takeWhile (\n -> nodeDepth n > 0) $ iterate (fromJust . nodeParent)  node
    path = map (\n -> (fromJust (nodeAction n), nodeState n)) parents  
    