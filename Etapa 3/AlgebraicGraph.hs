module AlgebraicGraph where

import qualified Data.Set as S

data AlgebraicGraph a
    = Empty
    | Node a
    | Overlay (AlgebraicGraph a) (AlgebraicGraph a)
    | Connect (AlgebraicGraph a) (AlgebraicGraph a)

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
    As defini clasa cu tipul concret (AlgebraicGraph a), intrucat pentru
    functia nodes :: Ord a => AlgebraicGraph a -> S.Set a, necesita tipul a
    pentru a specifica tipul rezultatului (S.Set a).
-}

{-
    *** TODO ***

    Instanțiați clasa Num cu tipul (AlgebraicGraph a), astfel încât:
    - un literal întreg să fie interpretat ca un singur nod cu eticheta egală
      cu acel literal
    - operația de adunare să fie intepretată ca Overlay
    - operația de înmulțire să fie interpretată drept Connect.

    Celelalte funcții din clasă nu sunt relevante. Veți obține warning-uri
    pentru neimplementarea lor, dar puteți să le ignorați.

    După instanțiere, veți putea evalua în consolă expresii ca:

    > 1 :: AlgebraicGraph Int
    Node 1
    
    > 1*(2+3) :: AlgebraicGraph Int
    Connect (Node 1) (Overlay (Node 2) (Node 3))
-}
instance Num a => Num (AlgebraicGraph a) where
    -- Transform intregul primit din Integer in a si creez un nod cu el.
    fromInteger = Node . fromInteger
    (+) = Overlay
    (*) = Connect

{-
    *** TODO ***

    Instanțiați clasa Show cu tipul (AlgebraicGraph a), astfel încât
    reprezentarea sub formă de șir de caractere a unui graf să reflecte
    expresiile aritmetice definite mai sus. Puteți pune un nou rând de paranteze
    la fiecare subexpresie compusă.

    Exemple:

    > Node 1
    1

    > Connect (Node 1) (Overlay (Node 2) (Node 3))
    (1*(2+3))
-}
instance Show a => Show (AlgebraicGraph a) where
    show (Node val) = show val
    show (Overlay g1 g2) = "(" ++ show g1 ++ "+" ++ show g2 ++ ")"
    show (Connect g1 g2) = "(" ++ show g1 ++ "*" ++ show g2 ++ ")"

{-
    *** TODO ***

    Observați că instanța predefinită de Eq pentru tipul (AlgebraicGraph a)
    nu surprinde corect egalitatea a două grafuri, deoarece același graf
    conceptual poate avea două descrieri simbolice diferite.
    
    Prin urmare, instanțiați clasa Eq cu tipul (AlgebraicGraph a), astfel încât
    să comparați propriu-zis mulțimile de noduri și de arce.

    Exemple:

    > Node 1 == 1
    True

    > Node 1 == 2
    False

    > angle == 1*2 + 1*3
    True

    > triangle == (1*2)*3
    True
-}
instance Ord a => Eq (AlgebraicGraph a) where
    g1 == g2 = (nodes g1 == nodes g2 && edges g1 == edges g2)

{-
    *** TODO ***

    Extinde un graf existent, atașând noi subgrafuri arbitrare în locul nodurilor
    individuale. Funcția primită ca prim parametru determină această
    corespondență între noduri și subgrafuri. Observați că tipul etichetelor
    noi (b) poate diferi de al etichetelor vechi (a).

    Exemplu:

    > extend (\n -> if n == 1 then 4+5 else Node n) $ 1*(2+3)
    ((4+5)*(2+3))
-}
extend :: (a -> AlgebraicGraph b) -> AlgebraicGraph a -> AlgebraicGraph b
extend f graph = extendHelper graph
    where
        -- Aplic functia pe valoarea fiecarui nod (functia intoarce ea noul graf).
        extendHelper (Node val) = f val
        -- Altfel, reapelez functia pe subgrafuri.
        extendHelper (Overlay g1 g2) = Overlay (extendHelper g1) (extendHelper g2)
        extendHelper (Connect g1 g2) = Connect (extendHelper g1) (extendHelper g2)

{-
    *** TODO ***

    Divizează un nod în mai multe noduri, cu eliminarea nodului inițial.
    Arcele în care era implicat vechiul nod trebuie să devină valabile
    pentru noile noduri.
    
    Implementați splitNode folosind extend!
-}
splitNode :: Eq a
          => a                 -- nodul divizat
          -> [a]               -- nodurile cu care este înlocuit
          -> AlgebraicGraph a  -- graful existent
          -> AlgebraicGraph a  -- graful obținut
-- Folosesc functia extend careia ii dau ca parametru o functie ce verifica daca valoarea
-- unui nod este egala cu node, caz in care intoarce un graf de tip Overlay cu mai multe
-- nivele, incat sa contina toate nodurile din targets.
splitNode node targets = extend (\n -> if n == node then f targets else Node n)
    where
        -- Functie care construieste Overlay-ul multiplu.
        f news = if null news
                    then Empty
                    else Overlay (Node (head news)) (f (tail news))

{-
    *** TODO ***

    Instanțiați clasa Functor cu constructorul de tip AlgebraicGraph, astfel
    încât să puteți aplica o funcție pe toate etichetele unui graf.
    fmap reprezintă generalizarea lui map pentru orice fel de structură.

    Implementați fmap folosind extend!

    Exemplu:

    > fmap (+ 10) $ 1*(2+3) :: AlgebraicGraph Int
    (11*(12+13))
-}
instance Functor AlgebraicGraph where
    -- fmap :: (a -> b) -> AlgebraicGraph a -> AlgebraicGraph b
    -- Folosesc functia extend careia ii dau o functie ce primeste
    -- valoarea unui nod si intoarce un nou nod cu valoarea obtinuta
    -- prin aplicarea lui f.
    fmap f graph = extend (\n -> Node (f n)) graph

{-
    *** TODO ***

    Îmbină mai multe noduri într-unul singur, pe baza unei proprietăți
    respectate de nodurile îmbinate, cu eliminarea acestora. Arcele în care
    erau implicate vechile noduri vor referi nodul nou.

    Implementați mergeNodes folosind fmap!
-}
mergeNodes :: (a -> Bool)       -- proprietatea îndeplinită de nodurile îmbinate
           -> a                 -- noul nod
           -> AlgebraicGraph a  -- graful existent
           -> AlgebraicGraph a  -- graful obținut
-- Folosesc functia fmap careia ii dau o functie ce primeste valoarea unui
-- nod si, daca indeplineste proprietatea, intoarce node.
mergeNodes prop node = fmap (\n -> if prop n then node else n)

{-
    *** TODO ***

    Filtrează un graf, păstrând doar nodurile care satisfac proprietatea dată.

    Implementați filterGraph folosind extend!
    
    Exemplu:

    > nodes $ filterGraph odd $ 1*(2+3)
    fromList [1,3]

    > edges $ filterGraph odd $ 1*(2+3)
    fromList [(1,3)]
-}
filterGraph :: (a -> Bool) -> AlgebraicGraph a -> AlgebraicGraph a
-- Folosesc functia extend careia ii dau o functie ce verifica daca valoarea
-- unui nod indeplineste proprietatea, caz in care pastreaza nodul, altfel il
-- inlocuieste cu Empty.
filterGraph prop graph = extend (\n -> if prop n then Node n else Empty) graph

{-
    *** TODO ***

    Întoarce graful rezultat prin eliminarea unui nod și a arcelor în care
    acesta este implicat. Dacă nodul nu există, se întoarce același graf.

    Implementați removeNode folosind filterGraph!
-}
removeNode :: Eq a => a -> AlgebraicGraph a -> AlgebraicGraph a
-- Folosesc functia filterGraph ca sa filtrez graful astfel incat sa
-- pastreze doar nodurile diferite de node.
removeNode node graph = filterGraph (node /=) graph