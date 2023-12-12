##  #NonEmptyList 
##
## `NonEmptyList` is a non-empty list, useful for modeling data with one or more occurence.
## `NonEmptyList` is likely to be less performant than the built-in List, but is more ergonomic for cases where existence of data is guaranteed.
## There are two ways to construct a `NonEmptyList`. It can either be constructed using `fromList`, or `single`.
## `NonEmptyList` defines many of the same functions as `List`. Certain functions like `drop` and `keep` are more suited to regular `List`s, and are not implemented for `NonEmptyList`. To access them it is best to cast back to a list using `toList`.
interface NonEmptyList 
    exposes [
        toList, fromList, single, len, first, last, addOneAndRepeat, min, max,
        join, walkFrom, countIf, map, walkFromUntil, walkBackwardsUntil, all, walkWithIndex, any, walk, walkUntil, walkBackwards, contains, product, sum, reverse
    ]
    imports []


NonEmptyList a := {body: List a, tail: a} implements [Eq]

## Construct a `NonEmptyList` from a list. Returns `ListWasEmpty` if the input list is empty.
## ```
## [1, 2, 3] |> NonEmptyList.fromList 
##
## expect [] |> NonEmptyList.fromList == Err ListWasEmpty
## ```
fromList: List a -> Result (NonEmptyList a) [ListWasEmpty]
fromList = \lst ->
    tail <- lst |> List.last |> Result.try
    body = lst |> List.dropLast 1
    Ok (@NonEmptyList {body, tail})

expect [] |> fromList == Err ListWasEmpty

expect ["a", "b", "c"] |> fromList == Ok (@NonEmptyList {body: ["a","b"], tail: "c"})

## Constructs a "singleton" `NonEmptyList` from a single element.
single: a -> NonEmptyList a
single = \x -> @NonEmptyList {body: [], tail: x}

## Gives the length of the non-empty list.
## ```
## expect single "a" |> NonEmptyList.len == 1
## ```
len: NonEmptyList * -> Nat
len = \nonempty ->
        nonempty |> toList |> List.len

expect len (@NonEmptyList {body: [], tail: "a"}) == 1
expect len (@NonEmptyList {body: ["b","c"], tail: "d"}) == 3 
expect single "a" |> toList |> List.len == 1


## Convert a `NonEmptyList` to a list.
toList: NonEmptyList a -> List a
toList = \@NonEmptyList {body, tail} ->
    body |> List.append tail

expect 
    @NonEmptyList {body: ["a","b"], tail:"c"}
    |> toList
    == ["a","b","c"]

expect
    lst = [1, 2, 3]
    res = lst |> fromList
    when res is
        Ok nonempty -> nonempty |> toList == lst
        Err _ -> Bool.false
      
last: NonEmptyList a -> a
last = \(@NonEmptyList {tail}) -> tail

expect @NonEmptyList {body: ["a","b"], tail: "c"} |> last == "c"

first: NonEmptyList a -> a
first = \(@NonEmptyList {body, tail}) ->
    when body is
        [head, ..] -> head
        [] -> tail

expect @NonEmptyList {body: [], tail: "c"} |> first == "c"
expect @NonEmptyList {body: ["a","b"], tail: "c"} |> first == "a"

min: NonEmptyList (Num a) -> Num a
min = \(@NonEmptyList {body, tail}) ->
    body |> List.walk tail Num.min

expect @NonEmptyList {body: [2,1], tail: 3} |> min == 1

max: NonEmptyList (Num a) -> Num a
max = \(@NonEmptyList {body, tail}) ->
    body |> List.walk tail Num.max  

expect @NonEmptyList {body: [2,1], tail: 3} |> max == 3 

## Add one occurence of an element `x` and then repeat it `n` times.
## ```
## expect x |> NonEmptyList.addOneAndRepeat 5 |> NonEmptyList.len == 6
## ```
addOneAndRepeat: a, Nat -> NonEmptyList a
addOneAndRepeat = \x, n ->
    @NonEmptyList {body: x |> List.repeat n, tail: x}


sum: NonEmptyList (Num a) -> Num a   
sum = \nonempty ->
    nonempty |> toList |> List.sum

product: NonEmptyList (Num a) -> Num a   
product = \nonempty ->
    nonempty |> toList |> List.product    


any: NonEmptyList a, (a -> Bool) -> Bool
any = \nonempty, pred ->
    nonempty |> toList |> List.any pred


all: NonEmptyList a, (a -> Bool) -> Bool
all = \nonempty, pred ->
        nonempty |> toList |> List.all pred 

        
get: NonEmptyList a, Nat -> Result a [OutOfBounds]
get = \nonempty, n ->
    nonempty |> toList |> List.get n

expect single 1 |> get 1 == Err OutOfBounds   
 
expect
    result =
        nonempty <- [1, 2] |> fromList |> Result.try
        nonempty |> get 1
    result == Ok 2


replace: NonEmptyList a, Nat, a -> {nonempty: NonEmptyList a, value: a}
replace = \(@NonEmptyList {body, tail}), n, newVal -> 
    if n == body |> List.len then 
        {
            nonempty: @NonEmptyList {body, tail: newVal},
            value: tail
        }
    else 
        {list, value} = body |> List.replace n newVal
        {
            nonempty: @NonEmptyList {body:list, tail},
            value
        }

expect
    {nonempty, value} = @NonEmptyList {body: ["a", "b", "c"], tail: "d"} |> replace 2 "e"
    (nonempty == @NonEmptyList {body: ["a", "b", "e"], tail: "d"})
        && (value == "c")

expect
    {nonempty, value} = @NonEmptyList {body: ["a", "b", "c"], tail: "d"} |> replace 3 "e"
    (nonempty == @NonEmptyList {body: ["a", "b", "c"], tail: "e"})
        && (value == "d")        

expect
    {nonempty, value} = @NonEmptyList {body: ["a", "b", "c"], tail: "d"} |> replace 4 "e"
    (nonempty == @NonEmptyList {body: ["a", "b", "c"], tail: "d"})
        && (value == "e")        

set: NonEmptyList a, Nat, a -> NonEmptyList a
set = \nonempty, n, newVal -> 
    (nonempty |> replace n newVal).nonempty

expect
    nonempty = @NonEmptyList {body: ["a", "b", "c"], tail: "d"} |> set 2 "e"
    nonempty == @NonEmptyList {body: ["a", "b", "e"], tail: "d"}

expect
    nonempty = @NonEmptyList {body: ["a", "b", "c"], tail: "d"} |> set 3 "e"
    nonempty == @NonEmptyList {body: ["a", "b", "c"], tail: "e"}       

expect
    nonempty = @NonEmptyList {body: ["a", "b", "c"], tail: "d"} |> set 4 "e"
    nonempty == @NonEmptyList {body: ["a", "b", "c"], tail: "d"}
    

update: NonEmptyList a, Nat, (a -> a) -> NonEmptyList a 
update = \nonempty, n, func ->
        when nonempty |> get n is
        Err OutOfBounds -> nonempty
        Ok value -> 
            newValue = func value
            nonempty |> set n newValue

expect
    nonempty = @NonEmptyList {body: ["a", "b", "c"], tail: "d"} |> update 2 (\str -> str |> Str.concat "x")
    nonempty == @NonEmptyList {body: ["a", "b", "cx"], tail: "d"}

expect
    nonempty = @NonEmptyList {body: ["a", "b", "c"], tail: "d"} |> update 3 (\str -> str |> Str.concat "x")
    nonempty == @NonEmptyList {body: ["a", "b", "c"], tail: "dx"}     

expect
    nonempty = @NonEmptyList {body: ["a", "b", "c"], tail: "d"} |> update 4 (\str -> str |> Str.concat "x")
    nonempty == @NonEmptyList {body: ["a", "b", "c"], tail: "d"}        
    
    
append: NonEmptyList a, a -> NonEmptyList a
append = \nonempty, val ->
    @NonEmptyList {body: nonempty |> toList, tail: val}

expect @NonEmptyList {body: [1,2], tail: 3} |> append 4 == @NonEmptyList {body: [1,2,3], tail: 4}   
    
appendIfOk: NonEmptyList a, Result a * -> NonEmptyList a
appendIfOk = \nonempty, res ->
    when res is
        Err _ -> nonempty
        Ok val -> nonempty |> append val
        
expect 
    res = Ok 4
    @NonEmptyList {body: [1,2], tail: 3} |> appendIfOk res == @NonEmptyList {body: [1,2,3], tail: 4}       

expect 
    res = Err OutOfBounds
    @NonEmptyList {body: [1,2], tail: 3} |> appendIfOk res == @NonEmptyList {body: [1,2], tail: 3}  

prepend: NonEmptyList a, a -> NonEmptyList a
prepend = \@NonEmptyList {body, tail}, val ->
    @NonEmptyList {body: body |> List.prepend val, tail}    

expect 
    @NonEmptyList {body: [1,2], tail: 3} |> prepend 4 == @NonEmptyList {body: [4,1,2], tail: 3}
    
prependIfOk: NonEmptyList a, Result a * -> NonEmptyList a
prependIfOk = \nonempty, res ->
        when res is
            Err _ -> nonempty
            Ok val -> nonempty |> prepend val    

expect 
    res = Ok 4
    @NonEmptyList {body: [1,2], tail: 3} |> prependIfOk res == @NonEmptyList {body: [4,1,2], tail: 3}       
        
expect 
    res = Err OutOfBounds
    @NonEmptyList {body: [1,2], tail: 3} |> prependIfOk res == @NonEmptyList {body: [1,2], tail: 3}             


concat: NonEmptyList a, NonEmptyList a -> NonEmptyList a
concat = \nonempty, (@NonEmptyList {body: bodyB, tail: tailB}) ->
        @NonEmptyList {body: nonempty |> toList |> List.concat bodyB, tail: tailB}

expect
    nonemptyA = @NonEmptyList {body: ["a","b"], tail: "c"}
    nonemptyB = @NonEmptyList {body: ["d","e"], tail: "f"}
    expected =  @NonEmptyList {body: ["a","b","c","d","e"], tail: "f"}  
    actual = nonemptyA |> concat nonemptyB
    actual == expected


reverse: NonEmptyList a -> NonEmptyList a
reverse = \nonempty ->
        (@NonEmptyList {body, tail}) = nonempty
        when body is
            [] -> nonempty
            [head, .. as rest] -> 
                @NonEmptyList {
                    body: rest |> List.append tail |> List.reverse,
                    tail: head
                }

join: NonEmptyList (NonEmptyList a) -> NonEmptyList a
join = \(@NonEmptyList {body, tail}) ->
    body |> List.walkBackwards tail \state, elem ->
                elem |> concat state


contains: NonEmptyList a, a -> Bool where a implements Eq
contains = \nonempty, x ->
    nonempty |> toList |> List.contains x
    
walk: NonEmptyList elem, state, (state, elem -> state) -> state
walk = \nonempty, state, func ->
    nonempty |> toList |> List.walk state func

walkBackwards: NonEmptyList elem, state, (state, elem -> state) -> state
walkBackwards = \nonempty, state, func ->
    nonempty |> toList |> List.walkBackwards state func    

walkWithIndex : NonEmptyList elem, state, (state, elem, Nat -> state) -> state    
walkWithIndex = \nonempty, state, func ->
    nonempty |> toList |> List.walkWithIndex state func

walkUntil : NonEmptyList elem, state, (state, elem -> [Continue state, Break state]) -> state
walkUntil = \nonempty, state, func ->
    nonempty |> toList |> List.walkUntil state func 

walkBackwardsUntil : NonEmptyList elem, state, (state, elem -> [Continue state, Break state]) -> state
walkBackwardsUntil = \nonempty, state, func ->
    nonempty |> toList |> List.walkBackwardsUntil state func    
    
walkFrom: NonEmptyList elem, Nat, state, (state, elem -> state) -> state
walkFrom = \nonempty, n, state, func ->
        nonempty |> toList |> List.walkFrom n state func    

walkFromUntil: NonEmptyList elem, Nat, state, (state, elem -> [Continue state, Break state]) -> state
walkFromUntil = \nonempty, n, state, func ->
            nonempty |> toList |> List.walkFromUntil n state func         

        

countIf : NonEmptyList a, (a -> Bool) -> Nat 
countIf = \nonempty, pred ->  
    nonempty |> toList |> List.countIf pred

map: NonEmptyList a, (a -> b) -> NonEmptyList b
map = \(@NonEmptyList {body, tail}), func ->
    @NonEmptyList {body: body |> List.map func, tail: (func tail)}



