{-# LANGUAGE TupleSections #-}
module StandardGraph where

import qualified Data.Set as S

{-
    Graf ORIENTAT cu noduri de tipul a, reprezentat prin mulțimile (set)
    de noduri și de arce.

    Mulțimile sunt utile pentru că gestionează duplicatele și permit
    testarea egalității a două grafuri fără a ține cont de ordinea nodurilor
    și a arcelor.

    type introduce un sinonim de tip, similar cu typedef din C.
-}
-- type StandardGraph a = (S.Set a, S.Set (a, a))
data StandardGraph a = StGraph { nodes :: S.Set a
                               , edges :: S.Set (a, a)
                               }

{-
    *** TODO ***

    Construiește un graf pe baza listelor de noduri și de arce.

    Hint: S.fromList.

    Constrângerea (Ord a) afirmă că valorile tipului a trebuie să fie
    ordonabile, lucru necesar pentru reprezentarea internă a mulțimilor.
    Este doar un detaliu, cu care nu veți opera explicit în această etapă.
    Veți întâlni această constrângere și în tipurile funcțiilor de mai jos.
-}
fromComponents :: Ord a
               => [a]              -- lista nodurilor
               -> [(a, a)]         -- lista arcelor
               -> StandardGraph a  -- graful construit
fromComponents ns es = StGraph (S.fromList ns) (S.fromList es)

{-
    Exemple de grafuri
-}
graph1 :: StandardGraph Int
graph1 = fromComponents [1, 2, 3, 3, 4] [(1, 2), (1, 3), (1, 2)]

graph2 :: StandardGraph Int
graph2 = fromComponents [4, 3, 3, 2, 1] [(1, 3), (1, 3), (1, 2)]

graph3 :: StandardGraph Int
graph3 = fromComponents [1, 2, 3, 4] [(1, 2), (1, 4), (4, 1), (2, 3), (1, 3)]

graph4 :: StandardGraph Int
graph4 = fromComponents [1, 2, 3, 4] [(1, 2), (1, 4), (4, 1), (2, 4), (1, 3)]

-- shouldBeTrue :: Bool
-- shouldBeTrue = graph1 == graph2

{-
    *** TODO ***

    Mulțimea nodurilor înspre care pleacă arce dinspre nodul curent.

    Exemplu:

    > outNeighbors 1 graph3
    fromList [2,3,4]
-}
outNeighbors :: Ord a => a -> StandardGraph a -> S.Set a
-- Filtrez muchiile care au primul element egal cu nodul (arcele care pornesc din nod),
-- si pentru fiecare muschie gasita preiau al doilea element (nodul destinatie)
outNeighbors node graph = S.map snd (S.filter f (edges graph))
    where f edge = (fst edge) == node

{-
    *** TODO ***

    Mulțimea nodurilor dinspre care pleacă arce înspre nodul curent.

    Exemplu:

    > inNeighbors 1 graph3 
    fromList [4]
-}
inNeighbors :: Ord a => a -> StandardGraph a -> S.Set a
-- Filtrez muchiile care au al doilea element egal cu nodul (arcele care ajung in nod),
-- si pentru fiecare muschie gasita preiau primul element (nodul sursa)
inNeighbors node graph = S.map fst (S.filter f (edges graph))
    where f edge = (snd edge) == node

{-
    *** TODO ***

    Întoarce graful rezultat prin eliminarea unui nod și a arcelor în care
    acesta este implicat. Dacă nodul nu există, întoarce același graf.

    Exemplu:

    > removeNode 1 graph3
    (fromList [2,3,4],fromList [(2,3)])
-}
removeNode :: Ord a => a -> StandardGraph a -> StandardGraph a
-- Construiesc un nou graf folosind multimea precedenta a nodurilor fara nodul
-- ce trebuie scos, si cu muschiile din graful preceddent care nu contin nodul scos.
removeNode node graph = StGraph (S.delete node (nodes graph)) (S.filter f (edges graph))
    where f edge = not (fst edge == node || snd edge == node)

{-
    *** TODO ***

    Divizează un nod în mai multe noduri, cu eliminarea nodului inițial.
    Arcele în care era implicat vechiul nod trebuie să devină valabile
    pentru noile noduri.

    Exemplu:

    > splitNode 2 [5,6] graph3
    (fromList [1,3,4,5,6],fromList [(1,3),(1,4),(1,5),(1,6),(4,1),(5,3),(6,3)])
-}
splitNode :: Ord a
          => a                -- nodul divizat
          -> [a]              -- nodurile cu care este înlocuit
          -> StandardGraph a  -- graful existent
          -> StandardGraph a  -- graful obținut
-- Construiesc un nou graf cu nodurile din graful precedent, fara nodul scos si cu noile noduri,
-- si cu muchiile fara nodul scos, la care adaug muchiile inlocuitoare.
splitNode old news graph = StGraph (S.union (nodes oldGraph) (S.fromList news)) (S.union (edges oldGraph) (S.fromList newes))
    where
        -- Graful din care am scos vechiul nod
        oldGraph = removeNode old graph
        -- Noile muchii, pentru fiecare muchie veche care contine nodul scos, adaug cate o muchie noua,
        -- in care nodul vechi este inlocuit cu cate un nod nou. Muchiile de tip (old, x) sunt gasite
        -- folosind functie outNeighbors, iar cele de tip (x, old) sunt gasite cu inNeighbors
        newes = [(x, y) | x <- news, y <- (S.toList (outNeighbors old graph))] ++ [(x, y) | x <- (S.toList (inNeighbors old graph)), y <- news]


{-
    *** TODO ***

    Îmbină mai multe noduri într-unul singur, pe baza unei proprietăți
    respectate de nodurile îmbinate, cu eliminarea acestora. Arcele în care
    erau implicate vechile noduri vor referi nodul nou.

    Exemplu:

    > mergeNodes even 5 graph3
    (fromList [1,3,5],fromList [(1,3),(1,5),(5,1),(5,3)])
-}
mergeNodes :: Ord a
           => (a -> Bool)      -- proprietatea îndeplinită de nodurile îmbinate
           -> a                -- noul nod
           -> StandardGraph a  -- graful existent
           -> StandardGraph a  -- graful obținut
-- Construiesc un nou graf folosind nodurile din graful precedent, din care scot nodurile ce indeplinesc conditia
-- si adaug noul nod, daca exista astfel de noduri, iar pentru muchii le scot pe cele care contin nodurile scoase
-- si le inlocuiesc cu noile muchii, cu noul nod.
mergeNodes prop node graph = StGraph (if filtered == (nodes graph)
                            then (nodes graph)
                            else S.insert node filtered) (S.union (S.filter (\e -> not (f e)) (edges graph)) newes)
    where
        -- Multimea nodurilor care nu indeplinesc proprietatea primita.
        filtered = S.filter (\n -> not (prop n)) (nodes graph)
        -- Functie care verifica daca macar un capat al muchiei indeplineste proprietatea.
        f edge = prop (fst edge) || prop (snd edge)
        -- Multimea noilor muchii ce trebuie adaugate in graf.
        newes = S.map g (S.filter f (edges graph))
        -- Functie care primeste o muchie si inlocuieste capetele care indeplinesc conditia cu noul nod.
        -- Ambele capete
        g pair = if prop (fst pair) && prop (snd pair)
            then (node, node)
            -- Doar primul capat
            else if prop (fst pair)
                then (node, snd pair)
                -- Doar al doilea capat
                else if prop (snd pair)
                    then (fst pair, node)
                    -- Niciun capat
                    else pair