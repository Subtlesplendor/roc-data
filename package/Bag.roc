## #Bag
## A bag is also known as a multiset. It is a modificaiton of the concept of a set which allows for multiple instances of each element. Each item then also has a multiplicity, defined by the number of instances of that item in the multiset.
## Example:
## ```
## Coin = Copper | Silver | Gold
## Purse = Bag Coin
##
## value: Coin -> Nat
## value = \coin ->
##     when coin is
##         Copper -> 1
##         Silver -> 100
##         Gold -> 10000
##
## totalValue: Purse -> Nat
## totalValue = \purse ->
##     purse |> Bag.walk 0 \total, coin, mult ->
##         total + (value coin) * mult
##
## ```
interface Bag
    exposes [
        Bag,
        empty,
        single,
        insert,
        repeat,
        size,
        remove,
        removeAll,
        isEmpty,
        containsAny,
        contains,
        count,
        toList,
        fromList,
        fromSet,
        walk,
        union,
        intersection,
        difference,
        join,
        includedIn,
        map,
        # mapWithMultiplicity,
        joinMap,
        # joinMapWithMultiplicity,
        scaleWith,
        keepIf,
        dropIf,
    ]
    imports []

Bag a := Dict a Nat implements [Eq]

## #Construct

## Create an empty bag
empty : {} -> Bag *
empty = \{} ->
    Dict.empty {} |> @Bag

## Create a bag with a single instance of an item in it
single : a -> Bag a
single = \x ->
    Dict.empty {}
    |> Dict.insert x 1
    |> @Bag

## Create a bag with multiple instances of an item in it
repeat : a, Nat -> Bag a
repeat = \x, n ->
    if n == 0 then
        @Bag (Dict.empty {})
    else
        Dict.empty {}
        |> Dict.insert x n
        |> @Bag

expect "a" |> repeat 10 == empty {} |> insert 10 "a"

## Create a bag from a list.
## ```
## expect
##     bag = ["a", "a", "b"] |> Bag.fromList
##     (bag |> Bag.count "a" == 2) && (bag |> Bag.count "b" == 1)
## ```
fromList : List k -> Bag k
fromList = \lst ->
    lst
    |> List.walk (empty {}) \state, elem ->
        state |> insert 1 elem

expect
    bag = ["a", "a", "b"] |> fromList
    (bag |> count "a" == 2) && (bag |> count "b" == 1)

## Construct a bag from a set.
## ```
## expect
##     bag = Set.empty {} |> Set.insert "a" |> Set.insert "a" |> Set.insert "b" |> Bag.fromSet
##     (bag |> Bag.count "a" == 1) && (bag |> Bag.count "b" == 1)
## ```
fromSet : Set k -> Bag k
fromSet = \set ->
    set
    |> Set.walk (empty {}) \state, elem ->
        state |> insert 1 elem

expect
    bag = Set.empty {} |> Set.insert "a" |> Set.insert "a" |> Set.insert "b" |> fromSet
    (bag |> count "a" == 1) && (bag |> count "b" == 1)

# separator

## #Combine

## Join two bags together, adding the number of items in each bag.
join : Bag k, Bag k -> Bag k
join = \bagA, bagB ->
    bagA
    |> walk bagB \state, elem, number ->
        state |> insert number elem

expect
    bagA = empty {} |> insert 3 "a" |> insert 2 "b"
    bagB = empty {} |> insert 2 "a" |> insert 4 "c"
    resultBag1 = bagA |> join bagB
    resultBag2 = bagB |> join bagA
    expectedBag = empty {} |> insert 5 "a" |> insert 2 "b" |> insert 4 "c"
    resultBag1 == expectedBag && resultBag1 == resultBag2

## Take the union of the two bags, defined by the biggest number of instances per item in both bags.
union : Bag k, Bag k -> Bag k
union = \@Bag dictA, @Bag dictB ->
    dictA
    |> Dict.walk dictB \state, elem, number ->
        numberInB = dictB |> Dict.get elem |> Result.withDefault 0
        n = Num.max number numberInB
        state |> Dict.insert elem n
    |> @Bag

expect
    bagA = empty {} |> insert 3 "a" |> insert 2 "b"
    bagB = empty {} |> insert 2 "a" |> insert 4 "c"
    resultBag1 = bagA |> union bagB
    resultBag2 = bagB |> union bagA
    expectedBag = empty {} |> insert 3 "a" |> insert 2 "b" |> insert 4 "c"
    resultBag1 == expectedBag && resultBag1 == resultBag2

## Take the intersection of the two bags, defined by the smallest number of instances per item in both bags. If an item is only present in one of the bags, the smallest number is zero and then it is not included in the resulting bag.
intersection : Bag k, Bag k -> Bag k
intersection = \@Bag dictA, @Bag dictB ->
    dictA
    |> Dict.walk (Dict.empty {}) \state, elem, number ->
        numberInB = dictB |> Dict.get elem |> Result.withDefault 0
        n = Num.min number numberInB
        if n == 0 then
            state
        else
            state |> Dict.insert elem n
    |> @Bag

expect
    bagA = empty {} |> insert 3 "a" |> insert 1 "b" |> insert 5 "c"
    bagB = empty {} |> insert 2 "a" |> insert 2 "b" |> insert 4 "d"
    resultBag1 = bagA |> intersection bagB
    resultBag2 = bagB |> intersection bagA
    expectedBag = empty {} |> insert 2 "a" |> insert 1 "b"
    resultBag1 == expectedBag && resultBag1 == resultBag2

## Take the difference of the two bags, defined by removing the number of instances of the items in the second bag from the first bag.
difference : Bag k, Bag k -> Bag k
difference = \bagA, bagB ->
    bagB
    |> walk bagA \state, elem, number ->
        state |> remove number elem

expect
    bagA = empty {} |> insert 3 "a" |> insert 2 "b" |> insert 1 "c"
    bagB = empty {} |> insert 4 "a" |> insert 1 "b" |> insert 1 "c"
    resultBag = bagA |> difference bagB
    expectedBag = empty {} |> insert 1 "b"
    resultBag == expectedBag

expect
    bagA = empty {} |> insert 3 "a" |> insert 2 "b" |> insert 1 "c"
    bagB = empty {} |> insert 4 "a" |> insert 1 "b" |> insert 1 "c"
    resultBag = bagB |> difference bagA
    expectedBag = empty {} |> insert 1 "a"
    resultBag == expectedBag

## #Inspect

## Get the total number of items in the bag
size : Bag * -> Nat
size = \@Bag dict ->
    dict
    |> Dict.walk 0 \state, _, value ->
        state + value

## Is the bag empty?
isEmpty : Bag * -> Bool
isEmpty = \@Bag dict -> dict |> Dict.isEmpty

## Does the bag contain any of the item?
containsAny : Bag k, k -> Bool
containsAny = \@Bag dict, x ->
    dict |> Dict.contains x

expect empty {} |> insert 1 "a" |> containsAny "a"
expect empty {} |> containsAny "a" |> Bool.not

## Does the bag contain the given number, or more, of the item?
contains : Bag k, Nat, k -> Bool
contains = \@Bag dict, n, x ->
    number = dict |> Dict.get x |> Result.withDefault 0
    n <= number

expect empty {} |> insert 1 "a" |> contains 0 "a"
expect empty {} |> contains 0 "a"
expect empty {} |> insert 1 "a" |> contains 1 "a"
expect empty {} |> insert 2 "a" |> contains 1 "a"
expect empty {} |> insert 1 "a" |> contains 2 "a" |> Bool.not
expect empty {} |> contains 1 "a" |> Bool.not

## Get the number of instances (multiplicity) of the given item
count : Bag k, k -> Nat
count = \@Bag dict, k ->
    dict |> Dict.get k |> Result.withDefault 0

expect empty {} |> insert 2 "a" |> count "a" == 2

## Is the first bag included in the second bag? This is a modification of the concept of subset. Bag A is included in bag B if all the items in bag A have lower or equal multiplicity compared with the items in bag B.
includedIn : Bag k, Bag k -> Bool
includedIn = \@Bag dictA, bagB ->
    dictA
    |> Dict.walkUntil Bool.true \_, key, number ->
        numberInB = bagB |> count key
        if number <= numberInB then
            Continue Bool.true
        else
            Break Bool.false

expect
    bagA = empty {} |> insert 3 "a" |> insert 1 "b"
    bagB = empty {} |> insert 4 "a" |> insert 1 "b" |> insert 1 "c"
    bagA |> includedIn bagB

expect
    bagA = empty {} |> insert 5 "a" |> insert 1 "b"
    bagB = empty {} |> insert 4 "a" |> insert 1 "b" |> insert 1 "c"
    bagA |> includedIn bagB |> Bool.not

## #Modify

## Insert a number of instances of an item into a bag
insert : Bag a, Nat, a -> Bag a
insert = \@Bag dict, n, x ->
    dict
    |> Dict.update x \possibleValue ->
        when possibleValue is
            Missing -> if n == 0 then Missing else Present n
            Present val -> Present (val + n)
    |> @Bag

expect
    empty {} |> insert 0 "a" == empty {}

## Remove a number of instances of an item in the bag. If the same number or more of instances are removed than there are in the bag, remove all the items from the bag.
remove : Bag k, Nat, k -> Bag k
remove = \@Bag dict, n, x ->
    if n == 0 then
        @Bag dict
    else
        when dict |> Dict.get x is
            Err KeyNotFound -> @Bag dict
            Ok val ->
                if val <= n then
                    dict |> Dict.remove x |> @Bag
                else
                    dict |> Dict.insert x (val - n) |> @Bag

expect empty {} |> insert 3 "a" |> remove 2 "a" |> count "a" == 1
expect empty {} |> insert 3 "a" |> remove 4 "a" == empty {}

## Remove all instances of an item from the bag.
removeAll : Bag k, k -> Bag k
removeAll = \@Bag dict, x ->
    dict |> Dict.remove x |> @Bag

expect empty {} |> insert 3 "a" |> removeAll "a" == empty {}

## #Consume

## Convert the bag to a list
## ```
## expect empty {} |> insert 2 "a" |> insert 3 "b" |> toList == ["a", "a", "b", "b", "b"]
## ```
toList : Bag k -> List k
toList = \@Bag dict ->
    dict
    |> Dict.walk [] \state, key, value ->
        state |> List.concat (key |> List.repeat value)

expect empty {} |> insert 2 "a" |> insert 3 "b" |> toList == ["a", "a", "b", "b", "b"]

## Walk the items of the bag and their multiplicities, threading state along the way.
walk : Bag k, state, (state, k, Nat -> state) -> state
walk = \@Bag dict, state, func ->
    dict |> Dict.walk state func

expect
    bag = empty {} |> insert 2 "a" |> insert 5 "b"
    bag
    |> walk 0 \state, elem, mult ->
        value = if elem == "a" then 10 else 1
        state + mult * value
    == 25

## Rescale the multiplicities of the items with the given integer.
scaleWith : Bag k, Nat -> Bag k
scaleWith = \@Bag dict, n ->
    if n == 0 then
        empty {}
    else
        dict
        |> Dict.map \_, number -> number * n
        |> @Bag

expect
    bagA = empty {} |> insert 5 "a" |> insert 1 "b" |> scaleWith 4
    expectedBag = empty {} |> insert 20 "a" |> insert 4 "b"
    bagA == expectedBag

expect
    bagA = empty {} |> insert 5 "a" |> insert 1 "b" |> scaleWith 0
    expectedBag = empty {}
    bagA == expectedBag

## Map a function over the elements of the bag, combining the multiplicities of the new items appropriately.
map : Bag a, (a -> b) -> Bag b
map = \bag, func ->
    bag
    |> walk (empty {}) \state, elem, number ->
        state |> insert number (func elem)

expect
    bagA =
        empty {}
        |> insert 5 "a"
        |> insert 1 "b"
        |> insert 1 "c"
        |> map \s ->
            when s is
                "a" -> "b"
                x -> x
    expectedBag = empty {} |> insert 6 "b" |> insert 1 "c"
    bagA == expectedBag

# ## Map a function over the elements of the bag, combining the multiplicities of the new items appropriately.
# mapWithMultiplicity : Bag a, (a, Nat -> { value : b, number : Nat }) -> Bag b
# mapWithMultiplicity = \bag, func ->
#     bag
#     |> walk (empty {}) \state, elem, num ->
#         { value, number } = func elem num
#         state |> insert number value

# expect
#     bagA =
#         empty {}
#         |> insert 5 "a"
#         |> insert 1 "b"
#         |> insert 1 "c"
#         |> mapWithMultiplicity \s, n ->
#             when (s, n) is
#                 ("a", _) -> { value: "b", number: 2 }
#                 (x, m) -> { value: x, number: m }
#     expectedBag = empty {} |> insert 3 "b" |> insert 1 "c"
#     bagA == expectedBag

## Map a bag-creating function over the elements of the bag, joining all the resulting bags together. The resulting multiplicities in each bag are scaled with the original multiplicity of the item the bag is created from.
## ```
## expect
##     Bag.empty {} |> Bag.insert 2 "a" |> Bag.insert 3 "b" |> Bag.joinMap \s ->
##         s |> Bag.repeat 3
##     == Bag.empty {} |> Bag.insert 6 "a" |> Bag.insert 9 "b"
## ```
joinMap : Bag a, (a -> Bag b) -> Bag b
joinMap = \bag, func ->
    bag
    |> walk (empty {}) \state, elem, number ->
        elem
        |> func
        |> scaleWith number
        |> join state

expect
    bagA =
        empty {}
        |> insert 5 "a"
        |> insert 1 "b"
        |> insert 2 "c"
        |> joinMap \s ->
            when s is
                "a" -> empty {} |> insert 1 1
                _ -> empty {} |> insert 1 2
    expectedBag = empty {} |> insert 5 1 |> insert 3 2
    bagA == expectedBag

expect
    Bag.empty {}
    |> Bag.insert 2 "a"
    |> Bag.insert 3 "b"
    |> Bag.joinMap \s ->
        s |> Bag.repeat 3
    == Bag.empty {}
    |> Bag.insert 6 "a"
    |> Bag.insert 9 "b"

# joinMapWithMultiplicity : Bag a, (a, Nat -> Bag b) -> Bag b
# joinMapWithMultiplicity = \bag, func ->
#     bag
#     |> walk (empty {}) \state, elem, number ->
#         elem
#         |> func number
#         |> join state

# expect
#     bagA =
#         empty {}
#         |> insert 5 "a"
#         |> insert 1 "b"
#         |> insert 2 "c"
#         |> joinMapWithMultiplicity \s, n ->
#             when (s, n) is
#                 ("a", m) -> empty {} |> insert m 1
#                 (_, k) -> empty {} |> insert (k + 1) 2
#     expectedBag = empty {} |> insert 5 1 |> insert 5 2
#     bagA == expectedBag

## Keep only items that fulfill the given predicative, which refers to both the item and its multiplicity.
keepIf : Bag a, ((a, Nat) -> Bool) -> Bag a
keepIf = \@Bag dict, pred ->
    dict |> Dict.keepIf pred |> @Bag

expect
    bagA =
        empty {}
        |> insert 5 "a"
        |> insert 1 "b"
        |> insert 2 "c"
        |> keepIf \(s, n) ->
            when (s, n) is
                ("a", m) if m > 4 -> Bool.true
                ("b", _) -> Bool.true
                (_, _) -> Bool.false
    expectedBag = empty {} |> insert 5 "a" |> insert 1 "b"
    bagA == expectedBag

## Drop all items that fulfill the given predicative, which refers to both the item and its multiplicity.
dropIf : Bag a, ((a, Nat) -> Bool) -> Bag a
dropIf = \@Bag dict, pred ->
    dict |> Dict.dropIf pred |> @Bag

expect
    bagA =
        empty {}
        |> insert 5 "a"
        |> insert 1 "b"
        |> insert 2 "c"
        |> dropIf \(s, n) ->
            when (s, n) is
                ("a", m) if m > 4 -> Bool.true
                ("b", _) -> Bool.true
                (_, _) -> Bool.false
    expectedBag = empty {} |> insert 2 "c"
    bagA == expectedBag

