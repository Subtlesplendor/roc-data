##  #NonEmpty 
##
## `NonEmpty` is a non-empty list, useful for modeling data with one or more occurence.
## `NonEmpty` is likely to be less performant than the built-in List, but is more ergonomic for cases where existence of data is guaranteed.
## There are two ways to construct a `NonEmpty`. It can either be constructed using `fromList`, or `single`.
## `NonEmpty` defines many of the same functions as `List`. Certain functions like `drop` and `keep` are more suited to regular `List`s, and are not implemented for `NonEmpty`. To access them it is best to cast back to a list using `toList`.
interface Data.NonEmpty 
    exposes [toList, fromList, single, len, first, last, addOneAndRepeat, min, max]
    imports []


## The type
NonEmpty a := {body: List a, tail: a} implements [Eq { isEq: isEq }]

isEq : NonEmpty a, NonEmpty a -> Bool 
    where a implements Eq
isEq = \nonemptyA, nonEmptyB ->
    nonemptyA |> toList == nonEmptyB |> toList

## Construct a `NonEmpty` from a list. Returns `ListWasEmpty` if the input list is empty.
## ```
## NonEmpty.fromList [1, 2, 3]
##
## expect [] |> NonEmpty.fromList == Err ListWasEmpty
## ```
fromList: List a -> Result (NonEmpty a) [ListWasEmpty]
fromList = \lst ->
    tail <- lst |> List.last |> Result.try
    body = lst |> List.dropLast 1
    Ok (@NonEmpty {body, tail})

expect [] |> fromList == Err ListWasEmpty

expect ["a", "b", "c"] |> fromList == Ok (@NonEmpty {body: ["a","b"], tail: "c"})

## Constructs a "singleton" `NonEmpty` from a single element.
single: a -> NonEmpty a
single = \x -> @NonEmpty {body: [], tail: x}

## Gives the length of the non-empty list.
## ```
## expect single "a" |> NonEmpty.len == 1
## ```
len: NonEmpty * -> Nat
len = \nonempty ->
        nonempty |> toList |> List.len

expect len (@NonEmpty {body: [], tail: "a"}) == 1
expect len (@NonEmpty {body: ["b","c"], tail: "d"}) == 3 
expect single "a" |> toList |> List.len == 1


## Convert a `NonEmpty` to a list.
toList: NonEmpty a -> List a
toList = \@NonEmpty {body, tail} ->
    body |> List.append tail

expect 
    @NonEmpty {body: ["a","b"], tail:"c"}
    |> toList
    == ["a","b","c"]

expect
    lst = [1, 2, 3]
    res = lst |> fromList
    when res is
        Ok nonempty -> nonempty |> toList == lst
        Err _ -> Bool.false
      
last: NonEmpty a -> a
last = \(@NonEmpty {tail}) -> tail

expect @NonEmpty {body: ["a","b"], tail: "c"} |> last == "c"

first: NonEmpty a -> a
first = \(@NonEmpty {body, tail}) ->
    when body is
        [head, ..] -> head
        [] -> tail

expect @NonEmpty {body: [], tail: "c"} |> first == "c"
expect @NonEmpty {body: ["a","b"], tail: "c"} |> first == "a"

min: NonEmpty (Num a) -> Num a
min = \(@NonEmpty {body, tail}) ->
    body |> List.walk tail Num.min

expect @NonEmpty {body: [2,1], tail: 3} |> min == 1

max: NonEmpty (Num a) -> Num a
max = \(@NonEmpty {body, tail}) ->
    body |> List.walk tail Num.max  

expect @NonEmpty {body: [2,1], tail: 3} |> max == 3 

## Add one occurence of an element `x` and then repeat it `n` times.
## ```
## expect x |> NonEmpty.addOneAndRepeat 5 |> NonEmpty.len == 6
## ```
addOneAndRepeat: a, Nat -> NonEmpty a
addOneAndRepeat = \x, n ->
    @NonEmpty {body: x |> List.repeat n, tail: x}


sum: NonEmpty (Num a) -> Num a   
sum = \nonempty ->
    nonempty |> toList |> List.sum

product: NonEmpty (Num a) -> Num a   
product = \nonempty ->
    nonempty |> toList |> List.product    


any: NonEmpty a, (a -> Bool) -> Bool
any = \nonempty, pred ->
    nonempty |> toList |> List.any pred


all: NonEmpty a, (a -> Bool) -> Bool
all = \nonempty, pred ->
        nonempty |> toList |> List.all pred 

        
get: NonEmpty a, Nat -> Result a [OutOfBounds]
get = \nonempty, n ->
    nonempty |> toList |> List.get n

expect single 1 |> get 1 == Err OutOfBounds   
 
expect
    result =
        nonempty <- [1, 2] |> fromList |> Result.try
        nonempty |> get 1
    result == Ok 2


replace: NonEmpty a, Nat, a -> {nonempty: NonEmpty a, value: a}
replace = \(@NonEmpty {body, tail}), n, newVal -> 
    if n == body |> List.len then 
        {
            nonempty: @NonEmpty {body, tail: newVal},
            value: tail
        }
    else 
        {list, value} = body |> List.replace n newVal
        {
            nonempty: @NonEmpty {body:list, tail},
            value
        }

expect
    {nonempty, value} = @NonEmpty {body: ["a", "b", "c"], tail: "d"} |> replace 2 "e"
    (nonempty == @NonEmpty {body: ["a", "b", "e"], tail: "d"})
        && (value == "c")

expect
    {nonempty, value} = @NonEmpty {body: ["a", "b", "c"], tail: "d"} |> replace 3 "e"
    (nonempty == @NonEmpty {body: ["a", "b", "c"], tail: "e"})
        && (value == "d")        

expect
    {nonempty, value} = @NonEmpty {body: ["a", "b", "c"], tail: "d"} |> replace 4 "e"
    (nonempty == @NonEmpty {body: ["a", "b", "c"], tail: "d"})
        && (value == "e")        

set: NonEmpty a, Nat, a -> NonEmpty a
set = \nonempty, n, newVal -> 
    (nonempty |> replace n newVal).nonempty

expect
    nonempty = @NonEmpty {body: ["a", "b", "c"], tail: "d"} |> set 2 "e"
    nonempty == @NonEmpty {body: ["a", "b", "e"], tail: "d"}

expect
    nonempty = @NonEmpty {body: ["a", "b", "c"], tail: "d"} |> set 3 "e"
    nonempty == @NonEmpty {body: ["a", "b", "c"], tail: "e"}       

expect
    nonempty = @NonEmpty {body: ["a", "b", "c"], tail: "d"} |> set 4 "e"
    nonempty == @NonEmpty {body: ["a", "b", "c"], tail: "d"}
    

update: NonEmpty a, Nat, (a -> a) -> NonEmpty a 
update = \nonempty, n, func ->
        when nonempty |> get n is
        Err OutOfBounds -> nonempty
        Ok value -> 
            newValue = func value
            nonempty |> set n newValue

expect
    nonempty = @NonEmpty {body: ["a", "b", "c"], tail: "d"} |> update 2 (\str -> str |> Str.concat "x")
    nonempty == @NonEmpty {body: ["a", "b", "cx"], tail: "d"}

expect
    nonempty = @NonEmpty {body: ["a", "b", "c"], tail: "d"} |> update 3 (\str -> str |> Str.concat "x")
    nonempty == @NonEmpty {body: ["a", "b", "c"], tail: "dx"}     

expect
    nonempty = @NonEmpty {body: ["a", "b", "c"], tail: "d"} |> update 4 (\str -> str |> Str.concat "x")
    nonempty == @NonEmpty {body: ["a", "b", "c"], tail: "d"}        
    
    
append: NonEmpty a, a -> NonEmpty a
append = \nonempty, val ->
    @NonEmpty {body: nonempty |> toList, tail: val}

expect @NonEmpty {body: [1,2], tail: 3} |> append 4 == @NonEmpty {body: [1,2,3], tail: 4}   
    
appendIfOk: NonEmpty a, Result a * -> NonEmpty a
appendIfOk = \nonempty, res ->
    when res is
        Err _ -> nonempty
        Ok val -> nonempty |> append val
        
expect 
    res = Ok 4
    @NonEmpty {body: [1,2], tail: 3} |> appendIfOk res == @NonEmpty {body: [1,2,3], tail: 4}       

expect 
    res = Err OutOfBounds
    @NonEmpty {body: [1,2], tail: 3} |> appendIfOk res == @NonEmpty {body: [1,2], tail: 3}  

prepend: NonEmpty a, a -> NonEmpty a
prepend = \@NonEmpty {body, tail}, val ->
    @NonEmpty {body: body |> List.prepend val, tail}    

expect 
    @NonEmpty {body: [1,2], tail: 3} |> prepend 4 == @NonEmpty {body: [4,1,2], tail: 3}
    
prependIfOk: NonEmpty a, Result a * -> NonEmpty a
prependIfOk = \nonempty, res ->
        when res is
            Err _ -> nonempty
            Ok val -> nonempty |> prepend val    

expect 
    res = Ok 4
    @NonEmpty {body: [1,2], tail: 3} |> prependIfOk res == @NonEmpty {body: [4,1,2], tail: 3}       
        
expect 
    res = Err OutOfBounds
    @NonEmpty {body: [1,2], tail: 3} |> prependIfOk res == @NonEmpty {body: [1,2], tail: 3}             


concat: NonEmpty a, NonEmpty a -> NonEmpty a
concat = \nonempty, (@NonEmpty {body: bodyB, tail: tailB}) ->
        @NonEmpty {body: nonempty |> toList |> List.concat bodyB, tail: tailB}

expect
    nonemptyA = @NonEmpty {body: ["a","b"], tail: "c"}
    nonemptyB = @NonEmpty {body: ["d","e"], tail: "f"}
    expected =  @NonEmpty {body: ["a","b","c","d","e"], tail: "f"}  
    actual = nonemptyA |> concat nonemptyB
    actual == expected


reverse: NonEmpty a -> NonEmpty a
reverse = \nonempty ->
        (@NonEmpty {body, tail}) = nonempty
        when body is
            [] -> nonempty
            [head, .. as rest] -> 
                @NonEmpty {
                    body: rest |> List.append tail |> List.reverse,
                    tail: head
                }

join: NonEmpty (NonEmpty a) -> NonEmpty a
join = \(@NonEmpty {body, tail}) ->
    body |> List.walkBackwards tail \state, elem ->
                elem |> concat state


contains: NonEmpty a, a -> Bool where a implements Eq
contains = \nonempty, x ->
    nonempty |> toList |> List.contains x
    
walk: NonEmpty elem, state, (state, elem -> state) -> state
walk = \nonempty, state, func ->
    nonempty |> toList |> List.walk state func

walkBackwards: NonEmpty elem, state, (state, elem -> state) -> state
walkBackwards = \nonempty, state, func ->
    nonempty |> toList |> List.walkBackwards state func    

walkWithIndex : NonEmpty elem, state, (state, elem, Nat -> state) -> state    
walkWithIndex = \nonempty, state, func ->
    nonempty |> toList |> List.walkWithIndex state func

walkUntil : NonEmpty elem, state, (state, elem -> [Continue state, Break state]) -> state
walkUntil = \nonempty, state, func ->
    nonempty |> toList |> List.walkUntil state func 

walkBackwardsUntil : NonEmpty elem, state, (state, elem -> [Continue state, Break state]) -> state
walkBackwardsUntil = \nonempty, state, func ->
    nonempty |> toList |> List.walkBackwardsUntil state func    
    
walkFrom: NonEmpty elem, Nat, state, (state, elem -> state) -> state
walkFrom = \nonempty, n, state, func ->
        nonempty |> toList |> List.walkFrom n state func    

walkFromUntil: NonEmpty elem, Nat, state, (state, elem -> [Continue state, Break state]) -> state
walkFromUntil = \nonempty, n, state, func ->
            nonempty |> toList |> List.walkFromUntil n state func         

        

countIf : NonEmpty a, (a -> Bool) -> Nat 
countIf = \nonempty, pred ->  
    nonempty |> toList |> List.countIf pred

map: NonEmpty a, (a -> b) -> NonEmpty b
map = \(@NonEmpty {body, tail}), func ->
    @NonEmpty {body: body |> List.map func, tail: (func tail)}



