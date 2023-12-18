##  #NonEmptyList
##
## `NonEmptyList` is a non-empty list, useful for modeling data with one or more occurence.
## `NonEmptyList` is likely to be less performant than the built-in List, but is more ergonomic for cases where existence of data is guaranteed.
## There are two ways to construct a `NonEmptyList`. It can either be constructed using `fromList`, or `single`.
interface NonEmptyList
    exposes [
        fromList,
        toList,
        addOneAndRepeat,
        get,
        replace,
        set,
        update,
        append,
        appendIfOk,
        prepend,
        prependIfOk,
        len,
        concat,
        last,
        single,
        reverse,
        join,
        contains,
        walk,
        walkBackwards,
        walkUntil,
        walkWithIndex,
        walkBackwardsUntil,
        walkFrom,
        walkFromUntil,
        sum,
        product,
        any,
        all,
        countIf,
        map,
        map2,
        map3,
        map4,
        mapWithIndex,
        sortWith,
        sortAsc,
        sortDesc,
        swap,
        first,
        takeFirstAnd,
        takeLastAnd,
        min,
        max,
        joinMap,
        findFirst,
        findLast,
        findFirstIndex,
        findLastIndex,
        intersperse,
        startsWith,
        endsWith,
        chunksOf,
        mapTry,
        walkTry,
    ]
    imports []

NonEmptyList a := { body : List a, foot : a } implements [Eq]

fromList : List a -> Result (NonEmptyList a) [ListWasEmpty]
fromList = \lst ->
    foot <- lst |> List.last |> Result.try
    body = lst |> List.dropLast 1
    Ok (@NonEmptyList { body, foot })

expect [] |> fromList == Err ListWasEmpty
expect [1, 2, 3] |> fromList == Ok (@NonEmptyList { body: [1, 2], foot: 3 })

toList : NonEmptyList a -> List a
toList = \@NonEmptyList { body, foot } ->
    body |> List.append foot

expect single "a" |> append "b" |> toList == ["a", "b"]

get : NonEmptyList a, Nat -> Result a [OutOfBounds]
get = \nonempty, n ->
    nonempty |> toList |> List.get n

expect single 1 |> get 1 == Err OutOfBounds

expect
    result =
        nonempty <- [1, 2] |> fromList |> Result.try
        nonempty |> get 1
    result == Ok 2

replace : NonEmptyList a, Nat, a -> { nonemptylist : NonEmptyList a, value : a }
replace = \@NonEmptyList { body, foot }, n, newVal ->
    if n == body |> List.len then
        {
            nonemptylist: @NonEmptyList { body, foot: newVal },
            value: foot,
        }
    else
        { list, value } = body |> List.replace n newVal
        {
            nonemptylist: @NonEmptyList { body: list, foot },
            value,
        }

set : NonEmptyList a, Nat, a -> NonEmptyList a
set = \nonempty, n, newVal ->
    (nonempty |> replace n newVal).nonemptylist

expect
    nonempty = @NonEmptyList { body: ["a", "b", "c"], foot: "d" } |> set 2 "e"
    nonempty == @NonEmptyList { body: ["a", "b", "e"], foot: "d" }

expect
    nonempty = @NonEmptyList { body: ["a", "b", "c"], foot: "d" } |> set 3 "e"
    nonempty == @NonEmptyList { body: ["a", "b", "c"], foot: "e" }

expect
    nonempty = @NonEmptyList { body: ["a", "b", "c"], foot: "d" } |> set 4 "e"
    nonempty == @NonEmptyList { body: ["a", "b", "c"], foot: "d" }

update : NonEmptyList a, Nat, (a -> a) -> NonEmptyList a
update = \nonempty, n, func ->
    when nonempty |> get n is
        Err OutOfBounds -> nonempty
        Ok value ->
            newValue = func value
            nonempty |> set n newValue

expect
    nonempty = @NonEmptyList { body: ["a", "b", "c"], foot: "d" } |> update 2 (\str -> str |> Str.concat "x")
    nonempty == @NonEmptyList { body: ["a", "b", "cx"], foot: "d" }

expect
    nonempty = @NonEmptyList { body: ["a", "b", "c"], foot: "d" } |> update 3 (\str -> str |> Str.concat "x")
    nonempty == @NonEmptyList { body: ["a", "b", "c"], foot: "dx" }

expect
    nonempty = @NonEmptyList { body: ["a", "b", "c"], foot: "d" } |> update 4 (\str -> str |> Str.concat "x")
    nonempty == @NonEmptyList { body: ["a", "b", "c"], foot: "d" }

append : NonEmptyList a, a -> NonEmptyList a
append = \nonempty, val ->
    @NonEmptyList { body: nonempty |> toList, foot: val }

expect
    @NonEmptyList { body: [1, 2], foot: 3 }
    |> append 4
    == @NonEmptyList { body: [1, 2, 3], foot: 4 }

appendIfOk : NonEmptyList a, Result a * -> NonEmptyList a
appendIfOk = \nonempty, res ->
    when res is
        Err _ -> nonempty
        Ok val -> nonempty |> append val

expect
    res = Ok 4
    @NonEmptyList { body: [1, 2], foot: 3 } |> appendIfOk res == @NonEmptyList { body: [1, 2, 3], foot: 4 }

expect
    res = Err OutOfBounds
    @NonEmptyList { body: [1, 2], foot: 3 } |> appendIfOk res == @NonEmptyList { body: [1, 2], foot: 3 }

prepend : NonEmptyList a, a -> NonEmptyList a
prepend = \@NonEmptyList { body, foot }, val ->
    @NonEmptyList { body: body |> List.prepend val, foot }

expect
    @NonEmptyList { body: [1, 2], foot: 3 } |> prepend 4 == @NonEmptyList { body: [4, 1, 2], foot: 3 }

prependIfOk : NonEmptyList a, Result a * -> NonEmptyList a
prependIfOk = \nonempty, res ->
    when res is
        Err _ -> nonempty
        Ok val -> nonempty |> prepend val

expect
    res = Ok 4
    @NonEmptyList { body: [1, 2], foot: 3 } |> prependIfOk res == @NonEmptyList { body: [4, 1, 2], foot: 3 }

expect
    res = Err OutOfBounds
    @NonEmptyList { body: [1, 2], foot: 3 } |> prependIfOk res == @NonEmptyList { body: [1, 2], foot: 3 }

## Gives the length of the non-empty list.
## ```
## expect single "a" |> NonEmptyList.len == 1
## ```
len : NonEmptyList * -> Nat
len = \nonempty ->
    nonempty |> toList |> List.len

expect len (@NonEmptyList { body: [], foot: "a" }) == 1
expect len (@NonEmptyList { body: ["b", "c"], foot: "d" }) == 3
expect single "a" |> len == 1

concat : NonEmptyList a, NonEmptyList a -> NonEmptyList a
concat = \nonempty, @NonEmptyList { body: bodyB, foot: footB } ->
    @NonEmptyList { body: nonempty |> toList |> List.concat bodyB, foot: footB }

expect
    nonemptyA = @NonEmptyList { body: ["a", "b"], foot: "c" }
    nonemptyB = @NonEmptyList { body: ["d", "e"], foot: "f" }
    expected = @NonEmptyList { body: ["a", "b", "c", "d", "e"], foot: "f" }
    actual = nonemptyA |> concat nonemptyB
    actual == expected

last : NonEmptyList a -> a
last = \@NonEmptyList { foot } -> foot

expect @NonEmptyList { body: ["a", "b"], foot: "c" } |> last == "c"

## Constructs a "singleton" `NonEmptyList` from a single element.
single : a -> NonEmptyList a
single = \x -> @NonEmptyList { body: [], foot: x }

expect single "a" == @NonEmptyList {body: [], foot: "a"}

## Add one occurence of an element `x` and then repeat it `n` times.
## ```
## expect x |> NonEmptyList.addOneAndRepeat 5 |> NonEmptyList.len == 6
## ```
addOneAndRepeat : a, Nat -> NonEmptyList a
addOneAndRepeat = \x, n ->
    @NonEmptyList { body: x |> List.repeat n, foot: x }

expect "a" |> addOneAndRepeat 10 |> len == 11
expect "a" |> addOneAndRepeat 3 == @NonEmptyList {body: ["a", "a", "a"], foot: "a"}    

reverse : NonEmptyList a -> NonEmptyList a
reverse = \nonempty ->
    (@NonEmptyList { body, foot }) = nonempty
    when body is
        [] -> nonempty
        [head, .. as rest] ->
            @NonEmptyList {
                body: rest |> List.append foot |> List.reverse,
                foot: head,
            }

expect 3 |> single |> reverse == 3 |> single            
expect @NonEmptyList {body: [1,2], foot: 3} |> reverse == @NonEmptyList {body: [3, 2], foot: 1}            

join : NonEmptyList (NonEmptyList a) -> NonEmptyList a
join = \@NonEmptyList { body, foot } ->
    body
    |> List.walkBackwards foot \state, elem ->
        elem |> concat state

contains : NonEmptyList a, a -> Bool where a implements Eq
contains = \nonempty, x ->
    nonempty |> toList |> List.contains x

walk : NonEmptyList elem, state, (state, elem -> state) -> state
walk = \nonempty, state, func ->
    nonempty |> toList |> List.walk state func

walkWithIndex : NonEmptyList elem, state, (state, elem, Nat -> state) -> state
walkWithIndex = \nonempty, state, func ->
    nonempty |> toList |> List.walkWithIndex state func

walkBackwards : NonEmptyList elem, state, (state, elem -> state) -> state
walkBackwards = \nonempty, state, func ->
    nonempty |> toList |> List.walkBackwards state func

walkUntil : NonEmptyList elem, state, (state, elem -> [Continue state, Break state]) -> state
walkUntil = \nonempty, state, func ->
    nonempty |> toList |> List.walkUntil state func

walkBackwardsUntil : NonEmptyList elem, state, (state, elem -> [Continue state, Break state]) -> state
walkBackwardsUntil = \nonempty, state, func ->
    nonempty |> toList |> List.walkBackwardsUntil state func

walkFrom : NonEmptyList elem, Nat, state, (state, elem -> state) -> state
walkFrom = \nonempty, n, state, func ->
    nonempty |> toList |> List.walkFrom n state func

walkFromUntil : NonEmptyList elem, Nat, state, (state, elem -> [Continue state, Break state]) -> state
walkFromUntil = \nonempty, n, state, func ->
    nonempty |> toList |> List.walkFromUntil n state func

sum : NonEmptyList (Num a) -> Num a
sum = \nonempty ->
    nonempty |> toList |> List.sum

product : NonEmptyList (Num a) -> Num a
product = \nonempty ->
    nonempty |> toList |> List.product

any : NonEmptyList a, (a -> Bool) -> Bool
any = \nonempty, pred ->
    nonempty |> toList |> List.any pred

all : NonEmptyList a, (a -> Bool) -> Bool
all = \nonempty, pred ->
    nonempty |> toList |> List.all pred

countIf : NonEmptyList a, (a -> Bool) -> Nat
countIf = \nonempty, pred ->
    nonempty |> toList |> List.countIf pred

map : NonEmptyList a, (a -> b) -> NonEmptyList b
map = \@NonEmptyList { body, foot }, func ->
    @NonEmptyList { body: body |> List.map func, foot: func foot }

map2 : NonEmptyList a, NonEmptyList b, (a, b -> c) -> NonEmptyList c
map2 = \nonemptyA, nonemptyB, func ->
    listA = nonemptyA |> toList
    listB = nonemptyB |> toList
    when List.map2 listA listB func |> fromList is
        Ok nonempty -> nonempty
        Err _ -> crash "this should never happen"

map3 : NonEmptyList a, NonEmptyList b, NonEmptyList c, (a, b, c -> d) -> NonEmptyList d
map3 = \nonemptyA, nonemptyB, nonemptyC, func ->
    listA = nonemptyA |> toList
    listB = nonemptyB |> toList
    listC = nonemptyC |> toList
    when List.map3 listA listB listC func |> fromList is
        Ok nonempty -> nonempty
        Err _ -> crash "this should never happen"

map4 : NonEmptyList a, NonEmptyList b, NonEmptyList c, NonEmptyList d, (a, b, c, d -> e) -> NonEmptyList e
map4 = \nonemptyA, nonemptyB, nonemptyC, nonemptyD, func ->
    listA = nonemptyA |> toList
    listB = nonemptyB |> toList
    listC = nonemptyC |> toList
    listD = nonemptyD |> toList
    when List.map4 listA listB listC listD func |> fromList is
        Ok nonempty -> nonempty
        Err _ -> crash "this should never happen"

mapWithIndex : NonEmptyList a, (a, Nat -> b) -> NonEmptyList b
mapWithIndex = \@NonEmptyList { body, foot }, func ->
    n = body |> List.len
    @NonEmptyList { body: body |> List.mapWithIndex func, foot: foot |> func n }

sortWith : NonEmptyList a, (a, a -> [LT, EQ, GT]) -> NonEmptyList a
sortWith = \nonempty, comparer ->
    when nonempty |> toList |> List.sortWith comparer |> fromList is
        Ok sorted -> sorted
        Err _ -> crash "this should never happen"

sortAsc : NonEmptyList (Num a) -> NonEmptyList (Num a)
sortAsc = \nonempty ->
    when nonempty |> toList |> List.sortAsc |> fromList is
        Ok sorted -> sorted
        Err _ -> crash "this should never happen"

sortDesc : NonEmptyList (Num a) -> NonEmptyList (Num a)
sortDesc = \nonempty ->
    when nonempty |> toList |> List.sortDesc |> fromList is
        Ok sorted -> sorted
        Err _ -> crash "this should never happen"

swap : NonEmptyList a, Nat, Nat -> NonEmptyList a
swap = \nonempty, nA, nB ->
    res =
        elemA <- nonempty |> get nA |> Result.try
        elemB <- nonempty |> get nB |> Result.try
        nonempty |> set nB elemA |> set nA elemB |> Ok
    res |> Result.withDefault nonempty

first : NonEmptyList a -> a
first = \@NonEmptyList { body, foot } ->
    when body is
        [head, ..] -> head
        [] -> foot

takeFirstAnd : NonEmptyList elem, Nat -> NonEmptyList elem
takeFirstAnd = \nonempty, n ->
    length = len nonempty
    (@NonEmptyList { body }) = nonempty
    when body is
        [] -> nonempty
        lst if (List.len lst) + 1 >= length -> nonempty
        [.. as listBody, end] ->
            @NonEmptyList { body: listBody |> List.takeFirst n, foot: end }

takeLastAnd : NonEmptyList elem, Nat -> NonEmptyList elem
takeLastAnd = \@NonEmptyList { body, foot }, n ->
    @NonEmptyList { body: body |> List.takeLast n, foot }

min : NonEmptyList (Num a) -> Num a
min = \@NonEmptyList { body, foot } ->
    body |> List.walk foot Num.min

expect @NonEmptyList { body: [2, 1], foot: 3 } |> min == 1

max : NonEmptyList (Num a) -> Num a
max = \@NonEmptyList { body, foot } ->
    body |> List.walk foot Num.max

expect @NonEmptyList { body: [2, 1], foot: 3 } |> max == 3

joinMap : NonEmptyList a, (a -> NonEmptyList b) -> NonEmptyList b
joinMap = \nonempty, func ->
    nonempty |> map func |> join

findFirst : NonEmptyList elem, (elem -> Bool) -> Result elem [NotFound]
findFirst = \nonempty, pred ->
    nonempty |> toList |> List.findFirst pred

findLast : NonEmptyList elem, (elem -> Bool) -> Result elem [NotFound]
findLast = \nonempty, pred ->
    nonempty |> toList |> List.findLast pred

findFirstIndex : NonEmptyList elem, (elem -> Bool) -> Result Nat [NotFound]
findFirstIndex = \nonempty, pred ->
    nonempty |> toList |> List.findFirstIndex pred

findLastIndex : NonEmptyList elem, (elem -> Bool) -> Result Nat [NotFound]
findLastIndex = \nonempty, pred ->
    nonempty |> toList |> List.findLastIndex pred

intersperse : NonEmptyList elem, elem -> NonEmptyList elem
intersperse = \nonempty, x ->
    (@NonEmptyList { body, foot }) = nonempty
    when body is
        [] -> nonempty
        lst -> @NonEmptyList { body: lst |> List.intersperse x |> List.append x, foot }

startsWith : NonEmptyList elem, NonEmptyList elem -> Bool where elem implements Eq
startsWith = \nonemptyA, nonemptyB ->
    nonemptyA |> toList |> List.startsWith (nonemptyB |> toList)

endsWith : NonEmptyList elem, NonEmptyList elem -> Bool where elem implements Eq
endsWith = \nonemptyA, nonemptyB ->
    nonemptyA |> toList |> List.endsWith (nonemptyB |> toList)

chunksOf : NonEmptyList a, Nat -> List (NonEmptyList a)
chunksOf = \nonempty, n ->
    nonempty
    |> toList
    |> List.chunksOf n
    |> List.map \lst ->
        when lst |> fromList is
            Ok nonemptylist -> nonemptylist
            Err _ -> crash "This should never happen"

mapTry : NonEmptyList elem, (elem -> Result ok err) -> Result (NonEmptyList ok) err
mapTry = \@NonEmptyList { body, foot }, func ->
    newBody <- body |> List.mapTry func |> Result.try
    newFoot <- foot |> func |> Result.try
    Ok (@NonEmptyList { body: newBody, foot: newFoot })

walkTry : NonEmptyList elem, state, (state, elem -> Result state err) -> Result state err
walkTry = \nonempty, state, func ->
    nonempty
    |> walkUntil (Ok state) \currentRes, elem ->
        when currentRes is
            Err error -> Break (Err error)
            Ok newState -> Continue (func newState elem)
