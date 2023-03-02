module Algorithms where

import qualified Data.Set as S
import StandardGraph

{-
    În etapa 1, prin graf înțelegem un graf cu reprezentare standard.
    În etapele următoare, vom experimenta și cu altă reprezentare.

    type introduce un sinonim de tip, similar cu typedef din C.
-}
type Graph a = StandardGraph a

{-
    *** TODO ***

    Funcție generală care abstractizează BFS și DFS pornind dintr-un anumit nod,
    prin funcția de îmbinare a listelor care constituie primul parametru.
    
    Cele două liste primite ca parametru de funcția de îmbinare sunt lista
    elementelor deja aflate în structură (coadă/stivă), respectiv lista
    vecinilor nodului curent, proaspăt expandați.

    Căutarea ar trebui să țină cont de eventualele cicluri.

    Hint: Scrieți o funcție auxiliară care primește ca parametru suplimentar
    o mulțime (set) care reține nodurile vizitate până în momentul curent.
-}
search :: Ord a
       => ([a] -> [a] -> [a])  -- funcția de îmbinare a listelor de noduri
       -> a                    -- nodul de pornire
       -> Graph a              -- graful
       -> [a]                  -- lista obținută în urma parcurgerii
search f node graph = searchHelper f graph [node] []
    where
        -- Functie care primeste functia de imbinare, graful, coada/stiva curenta si lista nodurilor vizitate.
        -- Daca coada/stiva este goala inseamna ca am terminat parcurgerea.
        searchHelper f graph currentList visited = if null currentList
            then []
            -- Altfel, daca deja am vizitat nodul curent, trec la urmatorul element din coada/stiva.
            else if elem currentNode visited
                then searchHelper f graph (tail currentList) visited
                -- Altfel inseamna ca am gasit un nou nod din parcurgere si ca atunci cand reapelez scot nodul din graf (pentru a evita ciclurile), adaug vecinii in
                -- coada/stiva, conform functiei de imbinare si adaug nodul curent in lista de noduri vizitate.
                else currentNode : searchHelper f (removeNode currentNode graph) (f (S.toList (outNeighbors currentNode graph)) (tail currentList)) (currentNode : visited)
            -- Nodul curent este primul element din coada/stiva.
            where currentNode = head currentList

{-
    *** TODO ***

    Strategia BFS, derivată prin aplicarea parțială a funcției search.

    Exemple:

    > bfs 1 graph4
    [1,2,3,4]

    > bfs 4 graph4
    [4,1,2,3]
-}
bfs :: Ord a => a -> Graph a -> [a]
-- BFS-ul foloseste coada, deci voi trimite ca parametru functiei search o functie de imbinare
-- care sa simuleze o coada (adaug la final, deoarece in search scot de la inceput).
bfs = search (\l1 l2 -> l2 ++ l1)

{-
    *** TODO ***

    Strategia DFS, derivată prin aplicarea parțială a funcției search.

    Exemple:

    > dfs 1 graph4 
    [1,2,4,3]
    
    > dfs 4 graph4
    [4,1,2,3]
-}
dfs :: Ord a => a -> Graph a -> [a]
-- DFS-ul foloseste stiva, deci voi trimite ca parametru functiei search o functie de imbinare
-- care sa simuleze o stiva (adaug la inceput, deoarece in search scot de la inceput).
dfs = search (\l1 l2 -> l1 ++ l2)

{-
    *** TODO ***

    Funcția numără câte noduri intermediare expandează strategiile BFS,
    respectiv DFS, în încercarea de găsire a unei căi între un nod sursă
    și unul destinație, ținând cont de posibilitatea absenței acesteia din graf.
    Numărul exclude nodurile sursă și destinație.

    Modalitatea uzuală în Haskell de a preciza că o funcție poate să nu fie
    definită pentru anumite valori ale parametrului este constructorul de tip
    Maybe. Astfel, dacă o cale există, funcția întoarce
    Just (numărBFS, numărDFS), iar altfel, Nothing.

    Hint: funcția span.

    Exemple:

    > countIntermediate 1 3 graph4
    Just (1,2)

    Aici, bfs din nodul 1 întoarce [1,2,3,4], deci există un singur nod
    intermediar (2) între 1 și 3. dfs întoarce [1,2,4,3], deci sunt două noduri
    intermediare (2, 4) între 1 și 3.

    > countIntermediate 3 1 graph4
    Nothing

    Aici nu există cale între 3 și 1.
-}
countIntermediate :: Ord a
                  => a                 -- nodul sursă
                  -> a                 -- nodul destinație
                  -> StandardGraph a   -- graful
                  -> Maybe (Int, Int)  -- numărul de noduri expandate de BFS/DFS
-- Daca distanta de la sursa la destinatie este egala cu lungimea intregii parcurgeri
-- inseamna ca nu a fost gasit nodul destinatie, deci voi intoarce Nothing.
countIntermediate from to graph = if resBFS == length (bfs from graph)
                                then Nothing
                                -- Altfel creez perechea de rezultate, scazand din fiecare 1,
                                -- intrucat am nevoie de noduri intermediare si nu de distanta.
                                else Just(resBFS - 1, resDFS - 1)
    where
        -- Distanta de la sursa la destinatie in parcurgerea BFS.
        resBFS = length $ fst $ span (\n -> n /= to) (bfs from graph)
        -- Distanta de la sursa la destinatie in parcurgerea DFS.
        resDFS = length $ fst $ span (\n -> n /= to) (dfs from graph)
