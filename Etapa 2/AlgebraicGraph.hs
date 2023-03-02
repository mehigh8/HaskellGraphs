module AlgebraicGraph where

import qualified Data.Set as S

data AlgebraicGraph a
    = Empty
    | Node a
    | Overlay (AlgebraicGraph a) (AlgebraicGraph a)
    | Connect (AlgebraicGraph a) (AlgebraicGraph a)
    deriving (Eq, Show)

-- (1, 2), (1, 3)
angle :: AlgebraicGraph Int
angle = Connect (Node 1) (Overlay (Node 2) (Node 3))

-- (1, 2), (1, 3), (2, 3)
triangle :: AlgebraicGraph Int
triangle = Connect (Node 1) (Connect (Node 2) (Node 3))

{-
    *** TODO ***

    Mulțimea nodurilor grafului.

    Hint: S.union
-}
nodes :: Ord a => AlgebraicGraph a -> S.Set a
nodes Empty = S.empty
nodes (Node val) = S.fromList [val]
nodes (Overlay g1 g2) = S.union (nodes g1) (nodes g2)
nodes (Connect g1 g2) = S.union (nodes g1) (nodes g2)

{-
    *** TODO ***

    Mulțimea arcelor grafului.

    Hint: S.union, S.cartesianProduct
-}
edges :: Ord a => AlgebraicGraph a -> S.Set (a, a)
edges Empty = S.empty
edges (Node val) = S.empty
edges (Overlay g1 g2) = S.union (edges g1) (edges g2)
edges (Connect g1 g2) = S.union (S.union (edges g1) (edges g2)) (S.cartesianProduct (nodes g1) (nodes g2))

{-
    *** TODO ***

    Mulțimea nodurilor înspre care pleacă arce dinspre nodul curent.

    ATENȚIE! NU folosiți funcția edges definită mai sus, pentru că ar genera
    prea multe muchii inutile.
-}
outNeighbors :: Ord a => a -> AlgebraicGraph a -> S.Set a
outNeighbors node Empty = S.empty
outNeighbors node (Node val) = S.empty
outNeighbors node (Overlay g1 g2) = S.union (outNeighbors node g1) (outNeighbors node g2)
-- Intorc reuniunea nodurilor destinatie din subgraful 1, subgraful 2 si, daca nodul se afla
-- in subgraful 1 (ceea ce inseamna ca are arce spre toate nodurile din subgraful 2), nodurile
-- subgrafului 2.
outNeighbors node (Connect g1 g2) = S.union
                                    (S.union (outNeighbors node g1) (outNeighbors node g2))
                                    (if S.member node (nodes g1) then (nodes g2) else S.empty)

{-
    *** TODO ***

    Mulțimea nodurilor dinspre care pleacă arce înspre nodul curent.

    ATENȚIE! NU folosiți funcția edges definită mai sus, pentru că ar genera
    prea multe muchii inutile.
-}
inNeighbors :: Ord a => a -> AlgebraicGraph a -> S.Set a
inNeighbors node Empty = S.empty
inNeighbors node (Node val) = S.empty
inNeighbors node (Overlay g1 g2) = S.union (inNeighbors node g1) (inNeighbors node g2)
-- Intorc reuniunea nodurilor sursa din subgraful 1, subgraful 2 si, daca nodul se afla
-- in subgraful 2 (ceea ce inseamna ca primeste arce dinspre toate nodurile din subgraful 1), nodurile
-- subgrafului 1.
inNeighbors node (Connect g1 g2) = S.union
                                    (S.union (inNeighbors node g1) (inNeighbors node g2))
                                    (if S.member node (nodes g2) then (nodes g1) else S.empty)

{-
    *** TODO ***

    Întoarce graful rezultat prin eliminarea unui nod și a arcelor în care
    acesta este implicat. Dacă nodul nu există, se întoarce același graf.

    Hint: Definiți o funcție recursivă locală (de exemplu, în where),
    care să primească drept parametri doar entități variabile de la un apel
    recursiv la altul, astfel încât să nu copiați la fiecare apel parametrii
    nemodificați. De exemplu, parametrul node nu se modifică, în timp ce
    parametrul graph se modifică.
-}
removeNode :: Eq a => a -> AlgebraicGraph a -> AlgebraicGraph a
removeNode node graph = removeNodeHelper graph
    where
        removeNodeHelper Empty = Empty
        -- Daca nodul curent are valoarea ce trebuie scoasa, il inlocuiesc cu Empty.
        removeNodeHelper (Node val) = if val == node then Empty else Node val
        -- Altfel reapelez functia pe subgrafuri.
        removeNodeHelper (Overlay g1 g2) = Overlay (removeNodeHelper g1) (removeNodeHelper g2)
        removeNodeHelper (Connect g1 g2) = Connect (removeNodeHelper g1) (removeNodeHelper g2)

{-
    *** TODO ***

    Divizează un nod în mai multe noduri, cu eliminarea nodului inițial.
    Arcele în care era implicat vechiul nod trebuie să devină valabile
    pentru noile noduri.
    
    Hint: Funcție recursivă locală, ca la removeNode.
-}
splitNode :: Eq a
          => a                 -- nodul divizat
          -> [a]               -- nodurile cu care este înlocuit
          -> AlgebraicGraph a  -- graful existent
          -> AlgebraicGraph a  -- graful obținut
splitNode old news graph = splitNodeHelper graph
    where
        splitNodeHelper Empty = Empty
        -- Daca nodul curent are valoarea ce trebuie scoasa, il voi inlocui cu un subgraf ce cuprinde
        -- nodurile noi.
        splitNodeHelper (Node val) = if val == old then construct (fst newsHalfs) (snd newsHalfs) else Node val
            where
                -- Pereche cu doua liste, jumatati ale listei news
                newsHalfs = splitAt (div (length news) 2) news
                -- Functie care construieste un graf dde tip Overlay intre nodurile din doua liste.
                construct l1 l2 = Overlay
                                  -- Listele au 3 cazuri:
                                  (case l1 of
                                    -- Daca lista este goala, nodul din Overlay va fi Empty.
                                    [] -> Empty
                                    -- Daca lista contine un singur element, nodul va fi de tip Node.
                                    [x] -> Node x
                                    -- Altfel, nodul va fi de fapt un subgraf de tip Overlay.
                                    (h : t) -> construct (fst l1Halfs) (snd l1Halfs))
                                  -- Analog pentru lista 2.
                                  (case l2 of
                                    [] -> Empty
                                    [x] -> Node x
                                    (h : t) -> construct (fst l2Halfs) (snd l2Halfs))
                    where
                        -- Pereche cu doua liste, jumatati ale listei l1.
                        l1Halfs = splitAt (div (length l1) 2) l1
                        -- Analog pentru lista l2.
                        l2Halfs = splitAt (div (length l2) 2) l2
        -- Altfel, reapelez functia pentru subgrafuri.
        splitNodeHelper (Overlay g1 g2) = Overlay (splitNodeHelper g1) (splitNodeHelper g2)
        splitNodeHelper (Connect g1 g2) = Connect (splitNodeHelper g1) (splitNodeHelper g2)

{-
    *** TODO ***

    Îmbină mai multe noduri într-unul singur, pe baza unei proprietăți
    respectate de nodurile îmbinate, cu eliminarea acestora. Arcele în care
    erau implicate vechile noduri vor referi nodul nou.

    Hint: Funcție recursivă locală, ca la removeNode.
-}
mergeNodes :: (a -> Bool)       -- proprietatea îndeplinită de nodurile îmbinate
           -> a                 -- noul nod
           -> AlgebraicGraph a  -- graful existent
           -> AlgebraicGraph a  -- graful obținut
mergeNodes prop node graph = mergeNodesHelper graph
 where
    mergeNodesHelper Empty = Empty
    -- Daca nodul curent are o valoare ce respecta proprietatea primita, este inlocuit de
    -- un nou nod cu valoarea node.
    mergeNodesHelper (Node val) = if prop val then Node node else Node val
    -- Altfel, reapelez functie pe subgrafuri.
    mergeNodesHelper (Overlay g1 g2) = Overlay (mergeNodesHelper g1) (mergeNodesHelper g2)
    mergeNodesHelper (Connect g1 g2) = Connect (mergeNodesHelper g1) (mergeNodesHelper g2)
