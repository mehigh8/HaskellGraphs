module Modular where

import Data.List
import Data.Function (on)
import qualified Data.Set as S
import AlgebraicGraph

type Graph a = AlgebraicGraph a

-- Graful descris în diagrama din enunțul temei
diagram :: AlgebraicGraph Int
diagram = ((1*2) * (3+4)) * 5

{-
    O partiție este o mulțime de submulțimi ale unei alte mulțimi, disjuncte
    (fără elemente comune) și care împreună conțin toate elementele originale.
    
    De exemplu, pentru mulțimea [1,2,3], o posibilă partiție este [[1], [2,3]].
-}
type Partition a = S.Set (S.Set a)

{-
    *** TODO ***

    Aplică o funcție pe fiecare element al unei liste, însă doar pe unul singur
    la un moment dat, păstrându-le pe celalte nemodificate. Prin urmare, pentru
    fiecare element din lista inițială rezultă câte o listă în lista finală,
    aferentă modificării doar a acelui element.

    Exemplu:

    > mapSingle (+ 10) [1,2,3]
    [[11,2,3],[1,12,3],[1,2,13]]
-}
mapSingle :: (a -> a) -> [a] -> [[a]]
-- Verific mai intai daca lista primita este nula. Daca nu incep apelarea recursiva.
mapSingle f xs = if null xs then [] else mapHelper 0 []
    where
        -- Functie care simuleaza un for, folosit pentru a genera lista finala, introduce
        -- la fiecare iteratie cate o lista in lista de liste.
        mapHelper cnt finalList = if cnt == length xs
                                  then finalList
                                  else mapHelper (cnt + 1) (finalList ++ [getList cnt])
            where
                -- Functie care creeaza o lista cu elementele lui xs, dar aplica f pe elementul
                -- cu indicele number.
                getList number = fst splitted ++ [f (head (snd splitted))] ++ tail (snd splitted)
                    where
                        -- Pereche de liste, prima cuprinde elementele pana la cel cu indicele
                        -- number, exclusiv, iar a doua pe restul.
                        splitted = splitAt number xs

{-
    *** TODO ***

    Determină lista tuturor partițiilor unei liste. Deși mai sus tipul
    Partition a este definit utilizând mulțimi, aici utilizăm liste,
    pentru simplitate.

    Dacă vi se pare greu de urmărit tipul întors de funcție, cu 3 niveluri
    ale constructorului de tip listă, gândiți-vă așa:
    - avem nevoie de un nivel pentru o submulțime
    - încă un nivel pentru o partiție, care este o mulțime de submulțimi
    - încă un nivel pentru mulțimea tuturor partițiilor.

    Hint: Folosiți list comprehensions pentru a răspunde la întrebarea:
    dacă am obținut o partiție a restului listei, cum obținem o partiție
    a întregii liste, care include capul? (folosiți și mapSingle)

    Exemple:

    > partitions [2,3]
    [[[2],[3]],[[2,3]]]

    > partitions [1,2,3]
    [[[1],[2],[3]],[[1,2],[3]],[[2],[1,3]],[[1],[2,3]],[[1,2,3]]]
-}
partitions :: [a] -> [[[a]]]
partitions xs = createPartitions $ generatePositions $ length xs
    where
        -- Functie care creeaza partitiile in functie de lista de liste de pozitii.
        createPartitions positions = case positions of
                                        -- Daca lista de liste mai are o singura lista, atunci nu mai este necesara
                                        -- apelarea recursiva.
                                        [pos] -> [createPartition pos (generateEmpty (getMax pos 0)) xs]
                                        (h : t) -> [createPartition h (generateEmpty (getMax h 0)) xs] ++ createPartitions t
            where
                -- Functie care genereaza o partitie goala, cu 'len' submultimi.
                generateEmpty len = if len == 1
                                    then [[]]
                                    else [[]] ++ (generateEmpty (len - 1))
                -- Functie care creaza o partitie, populand partitia goala (primita in partition), folosind lista de pozitii
                -- position si lista list (care este lista primita ca parametru in functia partitions).
                createPartition position partition list = if null position
                                                          -- Daca lista de pozitii este goala, intorc partitia curenta.
                                                          then partition
                                                          -- Altfel, reapelez functia si actualizez parametrii, inclusiv partitia
                                                          -- folosind functia editPartition.
                                                          else createPartition
                                                               (tail position)
                                                               (editPartition partition (head position) (head list) 1)
                                                               (tail list)
                    where
                        -- Functie care adauga un element in submultimea corespunzatoare din partitie. Acest lucru are loc prin
                        -- crearea unei partitii noi cu elementele celei vechi, si cu elementul nou adaugat.
                        editPartition part dest element currPos = if null part
                                                                  -- Daca partitia veche s-a golit, nu mai apelez recursiv.
                                                                  then []
                                                                  -- Altfel, in functie de numarul (currPos) submultimii curente,
                                                                  -- daca are numarul cautat (dest)
                                                                  else if currPos == dest
                                                                       -- adaug noul element in submultime si appenduiesc rezultatul
                                                                       -- apelului recursiv
                                                                       then [(head part) ++ [element]]
                                                                            ++ editPartition (tail part) dest element (currPos + 1)
                                                                       -- sau doar appenduiesc.
                                                                       else [head part] ++ editPartition (tail part) dest element (currPos + 1)
        -- Functie care gaseste elementul cu valoarea maxima dintr-o lista.
        getMax list currMax = if null list
                              then currMax
                              else if head list > currMax
                                   then getMax (tail list) (head list)
                                   else getMax (tail list) currMax
        -- Functie care genereaza lista de liste de pozitii, pe baza carora se creeaza partitiile. Implementarea mea se bazeaza
        -- pe ideea backtracking-ului. De exemplu: pentru lista [1, 2, 3], lista de pozitii [1, 1, 1] produce partitia [[1, 2, 3]],
        -- iar pentru aceeasi lista, lista de pozitii [1, 2, 2] produce partitia [[1], [2, 3]].
        generatePositions len = if len == 1
                                -- Daca lungimea pentru care generez pozitii este 1, exista o singura posibilitate: [1]. Este intors
                                -- ca [[1]] pentru ca rezultatul trebuie sa fie o lista de liste de pozitii.
                                then [[1]]
                                -- Altfel, folosind list comprehensions generez toate posibilitatile pozitiilor plecand de la pozitiile
                                -- pentru o lungime cu 1 mai mica.
                                -- Generarea are loc preluand fiecare lista de pozitii de len - 1, la care adaug noua pozitie, care
                                -- are conditia sa nu fie mai mare decat maximul listei de pozitii + 1 (de exemplu daca lista de
                                -- pozitii era [1, 1], y = 3 nu are sens intrucat nu putem avea submultimile 1 si 3, fara submultimea 2).
                                -- De asemenea, y-ul, noua pozitie, ia valori pana la cel mult len, intrucat pentru o lista de lungime
                                -- len nu vom putea avea mai mult de len submultimi.
                                else [x ++ [y] | x <- generatePositions (len - 1), y <- [1..len], y <= (getMax x 0) + 1]

{-
    *** TODO ***

    Verifică dacă o mulțime este un modul, i.e. dacă toate nodurile din mulțime
    au aceeași mulțime de vecini out și aceeași mulțime de vecini in,
    în exteriorul mulțimii de plecare. Cu alte cuvinte, excludem din verificare
    vecinii din interiorul mulțimii de plecare.

    Hint: S.map poate reduce dimensiunea unei mulțimi dacă elemente diferite
    inițial sunt asociate cu același element final, întrucât nu pot exista
    duplicate.

    Exemple:

    > isModule (S.fromList [1,2,3,4]) diagram
    True

    > isModule (S.fromList [5]) diagram
    True
    
    > isModule (S.fromList [1,2]) diagram
    True

    > isModule (S.fromList [3,4]) diagram
    True

    > isModule (S.fromList [1,3]) diagram
    False
-}
isModule :: Ord a
         => S.Set a
         -> Graph a
         -> Bool
-- Aplic un map pe elementele setului, astfel incat fiecare nod sa fie inlocuit
-- cu o pereche formata din vecinii out si vecinii in nodului, dar care sa nu
-- faca parte din modul (set). Datorita faptului ca intr-un set nu pot fi duplicate
-- inseamna ca daca setul este un modul, atunci setul in urma map-ului va avea un
-- singur element, intrucat toate nodurile trebuie sa aiba aceeasi vecini.
isModule set graph = S.size (S.map getInOut set) == 1
    where
        -- Functie care creeaza o pereche de vecini out si vecini in.
        getInOut x = (outNeighbors x filtered, inNeighbors x filtered)
            where
                -- Graf filtrat incat sa nu contina nodurile din set, in afara de
                -- cel curent.
                filtered = filterGraph (\n -> n == x || S.notMember n set) graph

{-
    *** TODO ***

    Verifică dacă o partiție a mulțimii de noduri constituie o descompunere
    modulară. Partiția este reprezentată ca o mulțime de mulțimi.

    Hint: la fel ca la isModule.

    Exemple:

    > isModularPartition
        (S.fromList [S.fromList [1], S.fromList [2],
                     S.fromList [3], S.fromList [4], S.fromList [5]])
        diagram
    True

    > isModularPartition (S.fromList [S.fromList [1,2,3,4,5]]) diagram
    True

    > isModularPartition (S.fromList [S.fromList [1,2,3,4], S.fromList [5]])
                         diagram
    True

    > isModularPartition
        (S.fromList [S.fromList [1,2], S.fromList [3,4],
                     S.fromList [5]])
        diagram
    True

    > isModularPartition (S.fromList [S.fromList [1,3], S.fromList [2,4,5]])
                         diagram
    False
-}
isModularPartition :: Ord a
                   => Partition a
                   -> Graph a
                   -> Bool
-- Folosesc un map pe setul partitiei incat sa aplic functia isModule pe fiecare submultime.
-- Daca setul in urma map-ului nu contine False, inseamna ca toate submultimile sunt module,
-- deci partitia este este modulara.
isModularPartition partition graph = S.notMember False $ S.map (\x -> isModule x graph) partition

{-
    *** TODO ***

    Determină partiția maximală dintr-o listă de partiții. Partiția maximală
    conține cele mai acoperitoare submulțimi ale mulțimii de noduri. Cu alte
    cuvinte, partiția maximală conține cel mai mic număr de submulțimi mai mare
    strict decât 1, pentru a exlcude partiția care conține doar întreaga mulțime
    de noduri.

    Hint: minimumBy din Data.List. Funcția este folosită pentru a stabili
    un criteriu ad hoc de ordonare, conform valorii întoarse de o funcție f
    când este aplicată pe elementele listei, printr-o construcție de forma:

    minimumBy (compare `on` f) lista.

    Exemple:

    > maximalModularPartition <lista partițiilor> diagram
    fromList [fromList [1,2,3,4],fromList [5]]

    > maximalModularPartition <lista partițiilor> $ removeNode 5 diagram
    fromList [fromList [1,2],fromList [3,4]]
-}
maximalModularPartition :: Ord a
                        => [Partition a]
                        -> Graph a
                        -> Partition a
-- Mai intai filtrez lista de partitii incat toate sa aiba size-ul mai mare decat 1
-- pentru a scoate partitia cu intreaga multime. Apoi filtrez lista de partitii incat
-- sa contina doar partitiile care sunt modulare, iar in final aleg minimul in functie
-- de size, folosind functia minimumBy.
maximalModularPartition partitions graph = minimumBy (compare `on` S.size) $
                                           filter (\x -> isModularPartition x graph) $
                                           filter (\x -> S.size x > 1) partitions

{-
    Obține descompunerea modulară a unui graf. O puteți utiliza pentru
    a experimenta manual cu maximalModularPartition.
    
    Exemple:

    > modularlyDecompose diagram                        
    fromList [fromList [1,2,3,4],fromList [5]]

    > modularlyDecompose $ removeNode 5 diagram
    fromList [fromList [1,2],fromList [3,4]]
-}
modularlyDecompose :: Ord a
                   => Graph a
                   -> Partition a
modularlyDecompose graph = maximalModularPartition partList graph
  where
    parts = partitions $ S.toList $ nodes graph
    partList = map (S.fromList . map S.fromList) parts