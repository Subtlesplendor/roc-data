interface Bag 
    exposes [Bag, empty, single, insert, repeat, size, remove, removeAll,
        isEmpty, contains, count, toList, fromList, fromSet, walk, walkUntil, union,
        intersection, difference, join, includedIn, map, joinMap, scaleWith,
        #keepIf, dropIf
    ]
    imports []


Bag a := Dict a Nat

# -- Core -------

empty: {} -> Bag *
empty = \{} ->
    Dict.empty {} |> @Bag 

single: a -> Bag a
single = \x ->
    Dict.empty {} 
        |> Dict.insert x 1
        |> @Bag

repeat: a, Nat -> Bag a
repeat = \x, n ->
    if n == 0 then
        @Bag (Dict.empty {})
    else
        Dict.empty {} 
            |> Dict.insert x 1
            |> @Bag        

insert: Bag a, Nat, a -> Bag a
insert = \@Bag dict, n, x ->
    dict 
        |> Dict.update x \possibleValue ->
            when possibleValue is
                Missing -> if n == 0 then Missing else Present n
                Present val -> Present (val + n)
        |> @Bag   
        
# expect 
#     expected = Dict.empty {} |> Dict.insert "a" 1
#     @Bag actual = empty {} |> insert "a"
#     expected == actual

size: Bag * -> Nat
size = \@Bag dict ->
    dict |> Dict.walk 0 \state, _, value ->
             state + value

isEmpty: Bag * -> Bool
isEmpty = \@Bag dict -> dict |> Dict.isEmpty

remove: Bag k, Nat, k -> Bag k
remove = \@Bag dict, n, x ->
    if n == 0 then @Bag dict else 
        when dict |> Dict.get x is 
            Err KeyNotFound -> @Bag dict 
            Ok val -> 
                if val <= n then
                    dict |> Dict.remove x |> @Bag
                else
                    dict |> Dict.insert x (val - n) |> @Bag

removeAll: Bag k, k -> Bag k
removeAll = \@Bag dict, x ->
    dict |> Dict.remove x |> @Bag


contains: Bag k, k -> Bool 
contains = \@Bag dict, x ->
    dict |> Dict.contains x

count: Bag k, k -> Nat 
count = \@Bag dict, k ->
    dict |> Dict.get k |> Result.withDefault 0

toList: Bag k -> List k
toList = \@Bag dict ->
    dict |> Dict.walk [] \state, key, value ->
        state |> List.concat (key |> List.repeat value)
        
fromList: List k -> Bag k
fromList = \lst ->
    lst |> List.walk (empty {}) \state, elem ->
        state |> insert 1 elem

fromSet: Set k -> Bag k 
fromSet = \set ->
    set |> Set.walk (empty {}) \state, elem ->
        state |> insert 1 elem
 

walk: Bag k, state, (state, k, Nat -> state) -> state
walk = \@Bag dict, state, func ->
    dict |> Dict.walk state func

walkUntil: Bag k, state, (state, k, Nat -> [Continue state, Break state]) -> state
walkUntil = \@Bag dict, state, func ->
    dict |> Dict.walkUntil state func  

join: Bag k, Bag k -> Bag k
join = \bagA, bagB ->
    bagA |> walk bagB \state, elem, number ->
        numberInB = bagB |> count elem
        state |> insert (number + numberInB) elem        

union: Bag k, Bag k -> Bag k
union = \bagA, bagB ->
    bagA |> walk bagB \state, elem, number ->
        numberInB = bagB |> count elem     
        state |> insert (Num.max number numberInB) elem  

intersection: Bag k, Bag k -> Bag k
intersection = \bagA, bagB ->
    bagA |> walk (empty {}) \state, elem, number ->
        numberInB = bagB |> count elem 
        state |> insert (Num.min number numberInB) elem          

difference: Bag k, Bag k -> Bag k
difference = \bagA, bagB ->
    bagB |> walk bagA \state, elem, number ->
        state |> remove number elem
        
includedIn: Bag k, Bag k -> Bool
includedIn = \@Bag dictA, bagB ->        
    dictA |> Dict.walkUntil Bool.true \_, key, number ->
        numberInB = bagB |> count key
        if number <= numberInB then
            Continue Bool.true
        else
            Break Bool.false 
            
scaleWith: Bag k, Nat -> Bag k
scaleWith = \@Bag dict, n ->
    dict 
        |> Dict.map \_, number -> number * n 
        |> @Bag          

map: Bag a, (a -> b) -> Bag b 
map = \bag, func ->
    bag |> walk (empty {}) \state, elem, number ->
        state |> insert number (func elem)
        
joinMap: Bag a, (a -> Bag b) -> Bag b
joinMap = \bag, func ->
    bag |> walk (empty {}) \state, elem, number ->
        elem 
            |> func 
            |> scaleWith number 
            |> join state
       

# keepIf: Bag a, ((a, Nat) -> Bool) -> Bag a 
# keepIf = \@Bag dict, pred ->    
#     dict |> Dict.keepIf pred

# dropIf: Bag a, ((a, Nat) -> Bool) -> Bag a 
# dropIf = \@Bag dict, pred ->
#     dict |> Dict.dropIf pred    



