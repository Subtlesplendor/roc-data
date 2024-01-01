## `NonEmptyList` is a non-empty list, useful for modeling data with one or more occurence.
## Example:
## ```
## Order : { items : NonEmptyList Item,
##     customer : CustomerInfo
## }
## ```
## There are two ways to construct a `NonEmptyList`. It can either be constructed using `fromList`, or `single`.
## This API implements many of the functions in the built-in `List`. For the functions that are missing, you can drop down to a regular `List` using `toList`.
interface NonEmptyList
    exposes [
        fromList,
        toList,
        singleThenRepeat,
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

## # Construct

## Construct a non-empty list from a `List`. Returns `ListWasEmpty` if the original list was empty.
fromList : List a -> Result (NonEmptyList a) [ListWasEmpty]
fromList = \lst ->
    foot <- lst |> List.last |> Result.try
    body = lst |> List.dropLast 1
    Ok (@NonEmptyList { body, foot })

expect [] |> fromList == Err ListWasEmpty
expect [1, 2, 3] |> fromList == Ok (@NonEmptyList { body: [1, 2], foot: 3 })

## Constructs a "singleton" `NonEmptyList` from a single element.
single : a -> NonEmptyList a
single = \x -> @NonEmptyList { body: [], foot: x }

expect single "a" == @NonEmptyList { body: [], foot: "a" }

## Transform the non-empty list to a list.
## ```
## expect NonEmptyList.single "a" |> NonEmptyList.append "b" |> NonEmptyList.toList == ["a", "b"]
## ```
toList : NonEmptyList a -> List a
toList = \@NonEmptyList { body, foot } ->
    body |> List.append foot

expect single "a" |> append "b" |> toList == ["a", "b"]

## # Useful Functions
## These functions are more ergonomic versions of the same functions in the `List` API.
## Because the list is non-empty, these are always guaranteed to return a value.

# separator

last : NonEmptyList a -> a
last = \@NonEmptyList { foot } -> foot

expect @NonEmptyList { body: ["a", "b"], foot: "c" } |> last == "c"

first : NonEmptyList a -> a
first = \@NonEmptyList { body, foot } ->
    when body is
        [head, ..] -> head
        [] -> foot

min : NonEmptyList (Num a) -> Num a
min = \@NonEmptyList { body, foot } ->
    body |> List.walk foot Num.min

expect @NonEmptyList { body: [2, 1], foot: 3 } |> min == 1

max : NonEmptyList (Num a) -> Num a
max = \@NonEmptyList { body, foot } ->
    body |> List.walk foot Num.max

expect @NonEmptyList { body: [2, 1], foot: 3 } |> max == 3

# separator

## # Modified Functions
## The following functions are slightly modified from the usual `List` API.

## Add one occurence of an element and then repeat it the specified number of times.
## ```
## expect "a" |> NonEmptyList.singleThenRepeat 5 |> NonEmptyList.len == 6
## ```
singleThenRepeat : a, Nat -> NonEmptyList a
singleThenRepeat = \x, n ->
    @NonEmptyList { body: x |> List.repeat n, foot: x }

expect "a" |> singleThenRepeat 10 |> len == 11
expect "a" |> singleThenRepeat 3 == @NonEmptyList { body: ["a", "a", "a"], foot: "a" }

## Take the first element and then the following specified number of elements.
takeFirstAnd : NonEmptyList elem, Nat -> NonEmptyList elem
takeFirstAnd = \nonempty, n ->
    length = len nonempty
    (@NonEmptyList { body }) = nonempty
    when body is
        [] -> nonempty
        lst if (List.len lst) + 1 >= length -> nonempty
        [.. as listBody, end] ->
            @NonEmptyList { body: listBody |> List.takeFirst n, foot: end }

## Take the last element and then the preceeding specified number of elements
takeLastAnd : NonEmptyList elem, Nat -> NonEmptyList elem
takeLastAnd = \@NonEmptyList { body, foot }, n ->
    @NonEmptyList { body: body |> List.takeLast n, foot }

# separator

## ## Just Like `List`

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

expect
    { nonemptylist, value } = @NonEmptyList { body: ["a", "b", "c"], foot: "d" } |> replace 2 "e"
    nonemptylist == @NonEmptyList { body: ["a", "b", "e"], foot: "d" } && value == "c"

expect
    { nonemptylist, value } = @NonEmptyList { body: ["a", "b", "c"], foot: "d" } |> replace 4 "e"
    nonemptylist == @NonEmptyList { body: ["a", "b", "c"], foot: "d" } && value == "e"

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
expect @NonEmptyList { body: [1, 2], foot: 3 } |> reverse == @NonEmptyList { body: [3, 2], foot: 1 }

join : NonEmptyList (NonEmptyList a) -> NonEmptyList a
join = \@NonEmptyList { body, foot } ->
    body
    |> List.walkBackwards foot \state, elem ->
        elem |> concat state

expect
    res =
        nonemptyA <- [1, 2, 3] |> fromList |> Result.try
        nonemptyB <- [4, 5, 6] |> fromList |> Result.try
        nonemptyC <- [7, 8, 9] |> fromList |> Result.try
        nonemptyA |> single |> append nonemptyB |> append nonemptyC |> join |> Ok
    res
    |> Result.map \nonempty ->
        nonempty == @NonEmptyList { body: [1, 2, 3, 4, 5, 6, 7, 8], foot: 9 }
    |> Result.withDefault Bool.false

contains : NonEmptyList a, a -> Bool where a implements Eq
contains = \nonempty, x ->
    nonempty |> toList |> List.contains x

expect 1 |> single |> contains 1
expect 1 |> single |> contains 2 |> Bool.not
expect @NonEmptyList { body: [1, 2], foot: 3 } |> contains 3
expect
    nonempty = @NonEmptyList { body: [1, 2], foot: 3 }
    (nonempty |> contains 1)
    && (nonempty |> contains 2)
    && (nonempty |> contains 3)
    && !(nonempty |> contains 4)

walk : NonEmptyList elem, state, (state, elem -> state) -> state
walk = \nonempty, state, func ->
    nonempty |> toList |> List.walk state func

expect
    @NonEmptyList { body: ["a", "b"], foot: "c" }
    |> walk "" \state, elem ->
        state |> Str.concat elem
    == "abc"

walkWithIndex : NonEmptyList elem, state, (state, elem, Nat -> state) -> state
walkWithIndex = \nonempty, state, func ->
    nonempty |> toList |> List.walkWithIndex state func

expect
    @NonEmptyList { body: ["a", "b"], foot: "c" }
    |> walkWithIndex "" \state, elem, index ->
        state |> Str.concat elem |> Str.concat (Num.toStr index)
    == "a0b1c2"

walkBackwards : NonEmptyList elem, state, (state, elem -> state) -> state
walkBackwards = \nonempty, state, func ->
    nonempty |> toList |> List.walkBackwards state func

expect
    @NonEmptyList { body: ["a", "b"], foot: "c" }
    |> walkBackwards "" \state, elem ->
        state |> Str.concat elem
    == "cba"

walkUntil : NonEmptyList elem, state, (state, elem -> [Continue state, Break state]) -> state
walkUntil = \nonempty, state, func ->
    nonempty |> toList |> List.walkUntil state func

expect
    nonemptylist = @NonEmptyList { body: ["a", "b", ""], foot: "c" }
    nonemptylist
    |> walkUntil "" \state, elem ->
        when elem is
            "" -> Break state
            s -> Continue (state |> Str.concat s)
    == "ab"

expect
    nonemptylist = @NonEmptyList { body: ["a", "b"], foot: "c" }
    nonemptylist
    |> walkUntil "" \state, elem ->
        when elem is
            "" -> Break state
            s -> Continue (state |> Str.concat s)
    == "abc"

walkBackwardsUntil : NonEmptyList elem, state, (state, elem -> [Continue state, Break state]) -> state
walkBackwardsUntil = \nonempty, state, func ->
    nonempty |> toList |> List.walkBackwardsUntil state func

expect
    nonemptylist = @NonEmptyList { body: ["a", "", "b"], foot: "c" }
    nonemptylist
    |> walkBackwardsUntil "" \state, elem ->
        when elem is
            "" -> Break state
            s -> Continue (state |> Str.concat s)
    == "cb"

expect
    nonemptylist = @NonEmptyList { body: ["a", "b"], foot: "c" }
    nonemptylist
    |> walkBackwardsUntil "" \state, elem ->
        when elem is
            "" -> Break state
            s -> Continue (state |> Str.concat s)
    == "cba"

walkFrom : NonEmptyList elem, Nat, state, (state, elem -> state) -> state
walkFrom = \nonempty, n, state, func ->
    nonempty |> toList |> List.walkFrom n state func

expect
    nonemptylist = @NonEmptyList { body: ["a", "b", "c"], foot: "d" }
    nonemptylist
    |> walkFrom 2 "" \state, elem ->
        state |> Str.concat elem
    == "cd"

walkFromUntil : NonEmptyList elem, Nat, state, (state, elem -> [Continue state, Break state]) -> state
walkFromUntil = \nonempty, n, state, func ->
    nonempty |> toList |> List.walkFromUntil n state func

expect
    nonemptylist = @NonEmptyList { body: ["", "b", "c", ""], foot: "d" }
    nonemptylist
    |> walkFromUntil 1 "" \state, elem ->
        when elem is
            "" -> Break state
            s -> Continue (state |> Str.concat s)
    == "bc"

expect
    nonemptylist = @NonEmptyList { body: ["", "b", "c"], foot: "d" }
    nonemptylist
    |> walkFromUntil 1 "" \state, elem ->
        when elem is
            "" -> Break state
            s -> Continue (state |> Str.concat s)
    == "bcd"

sum : NonEmptyList (Num a) -> Num a
sum = \nonempty ->
    nonempty |> toList |> List.sum

expect
    nonemptylist = @NonEmptyList { body: [1, 0, -2], foot: 3 }
    nonemptylist |> sum == 2

product : NonEmptyList (Num a) -> Num a
product = \nonempty ->
    nonempty |> toList |> List.product

expect
    nonemptylist = @NonEmptyList { body: [1, 2, -2], foot: 3 }
    nonemptylist |> product == -12

any : NonEmptyList a, (a -> Bool) -> Bool
any = \nonempty, pred ->
    nonempty |> toList |> List.any pred

expect
    nonemptylist = @NonEmptyList { body: ["a", "", "c"], foot: "d" }
    nonemptylist |> any Str.isEmpty

expect
    nonemptylist = @NonEmptyList { body: ["a", "b", "c"], foot: "d" }
    nonemptylist |> any Str.isEmpty |> Bool.not

all : NonEmptyList a, (a -> Bool) -> Bool
all = \nonempty, pred ->
    nonempty |> toList |> List.all pred

expect
    nonemptylist = @NonEmptyList { body: ["a", "", "c"], foot: "d" }
    nonemptylist |> all Str.isEmpty |> Bool.not

expect
    nonemptylist = @NonEmptyList { body: [1, 2, 3], foot: 4 }
    nonemptylist |> all (\x -> x > 0)

countIf : NonEmptyList a, (a -> Bool) -> Nat
countIf = \nonempty, pred ->
    nonempty |> toList |> List.countIf pred

expect
    nonemptylist = @NonEmptyList { body: ["a", "", "c"], foot: "d" }
    nonemptylist |> countIf Str.isEmpty == 1

expect
    nonemptylist = @NonEmptyList { body: [1, 0, 3], foot: 0 }
    nonemptylist |> countIf (\x -> x > 0) == 2

map : NonEmptyList a, (a -> b) -> NonEmptyList b
map = \@NonEmptyList { body, foot }, func ->
    @NonEmptyList { body: body |> List.map func, foot: func foot }

expect @NonEmptyList { body: [1, 0, 3], foot: 0 } |> map (\x -> x + 1) == @NonEmptyList { body: [2, 1, 4], foot: 1 }

map2 : NonEmptyList a, NonEmptyList b, (a, b -> c) -> NonEmptyList c
map2 = \nonemptyA, nonemptyB, func ->
    listA = nonemptyA |> toList
    listB = nonemptyB |> toList
    when List.map2 listA listB func |> fromList is
        Ok nonempty -> nonempty
        Err _ -> crash "this should never happen"

expect
    nonemptyA = @NonEmptyList { body: [1, 2, 3], foot: 4 }
    nonemptyB = @NonEmptyList { body: ["a", "b"], foot: "c" }
    nonemptyA |> map2 nonemptyB (\n, s -> (n, s)) == @NonEmptyList { body: [(1, "a"), (2, "b")], foot: (3, "c") }

map3 : NonEmptyList a, NonEmptyList b, NonEmptyList c, (a, b, c -> d) -> NonEmptyList d
map3 = \nonemptyA, nonemptyB, nonemptyC, func ->
    listA = nonemptyA |> toList
    listB = nonemptyB |> toList
    listC = nonemptyC |> toList
    when List.map3 listA listB listC func |> fromList is
        Ok nonempty -> nonempty
        Err _ -> crash "this should never happen"

expect
    nonemptyA = @NonEmptyList { body: [1, 2, 3], foot: 4 }
    nonemptyB = @NonEmptyList { body: ["a", "b"], foot: "c" }
    nonemptyC = @NonEmptyList { body: [10], foot: 11 }
    nonemptyA |> map3 nonemptyB nonemptyC (\n1, s, n2 -> (n1, s, n2)) == @NonEmptyList { body: [(1, "a", 10)], foot: (2, "b", 11) }

map4 : NonEmptyList a, NonEmptyList b, NonEmptyList c, NonEmptyList d, (a, b, c, d -> e) -> NonEmptyList e
map4 = \nonemptyA, nonemptyB, nonemptyC, nonemptyD, func ->
    listA = nonemptyA |> toList
    listB = nonemptyB |> toList
    listC = nonemptyC |> toList
    listD = nonemptyD |> toList
    when List.map4 listA listB listC listD func |> fromList is
        Ok nonempty -> nonempty
        Err _ -> crash "this should never happen"

expect
    nonemptyA = @NonEmptyList { body: [1, 2, 3], foot: 4 }
    nonemptyB = @NonEmptyList { body: ["a", "b"], foot: "c" }
    nonemptyC = @NonEmptyList { body: [10], foot: 11 }
    nonemptyD = @NonEmptyList { body: [], foot: "d" }
    nonemptyA |> map4 nonemptyB nonemptyC nonemptyD (\n, s1, m, s2 -> (n, s1, m, s2)) == @NonEmptyList { body: [], foot: (1, "a", 10, "d") }

mapWithIndex : NonEmptyList a, (a, Nat -> b) -> NonEmptyList b
mapWithIndex = \@NonEmptyList { body, foot }, func ->
    n = body |> List.len
    @NonEmptyList { body: body |> List.mapWithIndex func, foot: foot |> func n }

expect
    nonempty = @NonEmptyList { body: ["a", "b"], foot: "c" }
    actual =
        nonempty
        |> mapWithIndex \s, n ->
            s |> Str.concat (Num.toStr n)
    actual == @NonEmptyList { body: ["a0", "b1"], foot: "c2" }

sortWith : NonEmptyList a, (a, a -> [LT, EQ, GT]) -> NonEmptyList a
sortWith = \nonempty, comparer ->
    when nonempty |> toList |> List.sortWith comparer |> fromList is
        Ok sorted -> sorted
        Err _ -> crash "this should never happen"

expect
    nonempty = @NonEmptyList { body: [1, 3], foot: 2 }
    actual =
        nonempty
        |> sortWith \x1, x2 ->
            if x1 > x2 then
                GT
            else if x1 == x2 then
                EQ
            else
                LT
    actual == @NonEmptyList { body: [1, 2], foot: 3 }

sortAsc : NonEmptyList (Num a) -> NonEmptyList (Num a)
sortAsc = \nonempty ->
    when nonempty |> toList |> List.sortAsc |> fromList is
        Ok sorted -> sorted
        Err _ -> crash "this should never happen"

expect
    nonempty = @NonEmptyList { body: [1, 3], foot: 2 }
    actual = nonempty |> sortAsc
    actual == @NonEmptyList { body: [1, 2], foot: 3 }

sortDesc : NonEmptyList (Num a) -> NonEmptyList (Num a)
sortDesc = \nonempty ->
    when nonempty |> toList |> List.sortDesc |> fromList is
        Ok sorted -> sorted
        Err _ -> crash "this should never happen"

expect
    nonempty = @NonEmptyList { body: [1, 3], foot: 2 }
    actual = nonempty |> sortDesc
    actual == @NonEmptyList { body: [3, 2], foot: 1 }

swap : NonEmptyList a, Nat, Nat -> NonEmptyList a
swap = \nonempty, nA, nB ->
    res =
        elemA <- nonempty |> get nA |> Result.try
        elemB <- nonempty |> get nB |> Result.try
        nonempty |> set nB elemA |> set nA elemB |> Ok
    res |> Result.withDefault nonempty

expect
    nonempty = @NonEmptyList { body: [1, 3], foot: 2 }
    actual = nonempty |> swap 0 1
    actual == @NonEmptyList { body: [3, 1], foot: 2 }

expect
    nonempty = @NonEmptyList { body: [1, 3], foot: 2 }
    actual = nonempty |> swap 0 4
    actual == nonempty

joinMap : NonEmptyList a, (a -> NonEmptyList b) -> NonEmptyList b
joinMap = \nonempty, func ->
    nonempty |> map func |> join

expect
    nonempty = @NonEmptyList { body: [1, 3], foot: 2 }
    actual = nonempty |> joinMap \x -> x |> singleThenRepeat 2
    actual == @NonEmptyList { body: [1, 1, 1, 3, 3, 3, 2, 2], foot: 2 }

findFirst : NonEmptyList elem, (elem -> Bool) -> Result elem [NotFound]
findFirst = \nonempty, pred ->
    nonempty |> toList |> List.findFirst pred

expect
    nonempty = @NonEmptyList { body: [1, 3], foot: 2 }
    res =
        n <- nonempty |> findFirst (\x -> x > 2) |> Result.try
        Ok (n == 3)
    res |> Result.withDefault Bool.false

expect
    nonempty = @NonEmptyList { body: [1, 3], foot: 2 }
    res =
        error <- nonempty |> findFirst (\x -> x > 4) |> Result.onErr
        Err (error == NotFound)
    when res is
        Ok _ -> Bool.false
        Err b -> b

findLast : NonEmptyList elem, (elem -> Bool) -> Result elem [NotFound]
findLast = \nonempty, pred ->
    nonempty |> toList |> List.findLast pred

expect
    nonempty = @NonEmptyList { body: [1, 3], foot: 2 }
    res =
        n <- nonempty |> findLast (\x -> x > 1) |> Result.try
        Ok (n == 2)
    res |> Result.withDefault Bool.false

expect
    nonempty = @NonEmptyList { body: [1, 3], foot: 2 }
    res =
        error <- nonempty |> findLast (\x -> x > 4) |> Result.onErr
        Err (error == NotFound)
    when res is
        Ok _ -> Bool.false
        Err b -> b

findFirstIndex : NonEmptyList elem, (elem -> Bool) -> Result Nat [NotFound]
findFirstIndex = \nonempty, pred ->
    nonempty |> toList |> List.findFirstIndex pred

expect
    nonempty = @NonEmptyList { body: [1, 3], foot: 2 }
    res =
        n <- nonempty |> findFirstIndex (\x -> x > 2) |> Result.try
        Ok (n == 1)
    res |> Result.withDefault Bool.false

expect
    nonempty = @NonEmptyList { body: [1, 3], foot: 2 }
    res =
        error <- nonempty |> findFirstIndex (\x -> x > 4) |> Result.onErr
        Err (error == NotFound)
    when res is
        Ok _ -> Bool.false
        Err b -> b

findLastIndex : NonEmptyList elem, (elem -> Bool) -> Result Nat [NotFound]
findLastIndex = \nonempty, pred ->
    nonempty |> toList |> List.findLastIndex pred

expect
    nonempty = @NonEmptyList { body: [1, 3], foot: 2 }
    res =
        n <- nonempty |> findLastIndex (\x -> x > 1) |> Result.try
        Ok (n == 2)
    res |> Result.withDefault Bool.false

expect
    nonempty = @NonEmptyList { body: [1, 3], foot: 2 }
    res =
        error <- nonempty |> findLastIndex (\x -> x > 4) |> Result.onErr
        Err (error == NotFound)
    when res is
        Ok _ -> Bool.false
        Err b -> b

intersperse : NonEmptyList elem, elem -> NonEmptyList elem
intersperse = \nonempty, x ->
    (@NonEmptyList { body, foot }) = nonempty
    when body is
        [] -> nonempty
        lst -> @NonEmptyList { body: lst |> List.intersperse x |> List.append x, foot }

expect
    nonempty = @NonEmptyList { body: [1, 3], foot: 2 }
    nonempty
    |> intersperse 0
    == @NonEmptyList { body: [1, 0, 3, 0], foot: 2 }

expect
    single 1
    |> intersperse 0
    == @NonEmptyList { body: [], foot: 1 }

startsWith : NonEmptyList elem, NonEmptyList elem -> Bool where elem implements Eq
startsWith = \nonemptyA, nonemptyB ->
    nonemptyA |> toList |> List.startsWith (nonemptyB |> toList)

expect
    nonemptyA = single 1 |> append 0 |> append 2
    nonemptyB = single 1 |> append 0
    nonemptyA |> startsWith nonemptyB

expect
    nonemptyA = single 1 |> append 0 |> append 2
    nonemptyB = single 1 |> append 2
    nonemptyA |> startsWith nonemptyB |> Bool.not

endsWith : NonEmptyList elem, NonEmptyList elem -> Bool where elem implements Eq
endsWith = \nonemptyA, nonemptyB ->
    nonemptyA |> toList |> List.endsWith (nonemptyB |> toList)

expect
    nonemptyA = single 1 |> append 0 |> append 2
    nonemptyB = single 0 |> append 2
    nonemptyA |> endsWith nonemptyB

expect
    nonemptyA = single 1 |> append 0 |> append 2
    nonemptyB = single 1 |> append 2
    nonemptyA |> endsWith nonemptyB |> Bool.not

chunksOf : NonEmptyList a, Nat -> List (NonEmptyList a)
chunksOf = \nonempty, n ->
    nonempty
    |> toList
    |> List.chunksOf n
    |> List.map \lst ->
        when lst |> fromList is
            Ok nonemptylist -> nonemptylist
            Err _ -> crash "This should never happen"

expect
    nonempty = single 1 |> append 0 |> append 2
    nonempty |> chunksOf 0 == []

expect
    nonemptyA = single 1 |> append 0 |> append 2
    actual1 = @NonEmptyList { body: [1], foot: 0 }
    actual2 = @NonEmptyList { body: [], foot: 2 }
    nonemptyA |> chunksOf 2 == [actual1, actual2]

mapTry : NonEmptyList elem, (elem -> Result ok err) -> Result (NonEmptyList ok) err
mapTry = \@NonEmptyList { body, foot }, func ->
    newBody <- body |> List.mapTry func |> Result.try
    newFoot <- foot |> func |> Result.try
    Ok (@NonEmptyList { body: newBody, foot: newFoot })

expect
    nonemptyA = single "1" |> append "0" |> append "2"
    res =
        nonempty <- nonemptyA |> mapTry Str.toDec |> Result.try
        Ok (nonempty == @NonEmptyList { body: [1, 0], foot: 2 })
    res |> Result.withDefault Bool.false

expect
    nonemptyA = single "1" |> append "a" |> append "2"
    res =
        nonempty <- nonemptyA |> mapTry Str.toDec |> Result.try
        Ok (nonempty == @NonEmptyList { body: [1, 0], foot: 2 })
    res |> Result.withDefault Bool.false |> Bool.not

walkTry : NonEmptyList elem, state, (state, elem -> Result state err) -> Result state err
walkTry = \nonempty, state, func ->
    nonempty
    |> walkUntil (Ok state) \currentRes, elem ->
        when currentRes is
            Err error -> Break (Err error)
            Ok newState -> Continue (func newState elem)

expect
    nonemptyA = single "1" |> append "0" |> append "2"
    res =
        nonemptyA
        |> walkTry 0 \state, elem ->
            n <- elem |> Str.toDec |> Result.try
            state |> Num.add n |> Ok
    res |> Result.map (\x -> x == 3) |> Result.withDefault Bool.false

expect
    nonemptyA = single "1" |> append "a" |> append "2"
    res =
        nonemptyA
        |> walkTry 0 \state, elem ->
            n <- elem |> Str.toDec |> Result.try
            state |> Num.add n |> Ok
    res == Err InvalidNumStr
