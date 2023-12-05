interface Data.NonEmpty 
    exposes [toList, fromList, get]
    imports []


NonEmpty a := {body: List a, tail: a}

single: a -> NonEmpty a
single = \x -> @NonEmpty {body: [], tail: x}

toList: NonEmpty a -> List a
toList = \@NonEmpty {body, tail} ->
    when body is
        [] -> [tail]
        lst -> lst |> List.append tail

expect single "a" |> toList |> List.len == 1        

fromList: List a -> Result (NonEmpty a) [ListWasEmpty]
fromList = \lst ->
    tail <- lst |> List.last |> Result.try
    body = lst |> List.dropLast 1
    Ok (@NonEmpty {body, tail})

expect
    lst = [1, 2, 3]
    res = lst |> fromList
    when res is
        Ok nonempty -> nonempty |> toList == lst
        Err _ -> Bool.false

get: NonEmpty a, Nat -> Result a [OutOfBounds]
get = \nonempty, n ->
    nonempty |> toList |> List.get n

expect single 1 |> get 1 == Err OutOfBounds

expect 
    when [1, 2] |> fromList is 
        Err _ -> Bool.false
        Ok nonempty ->
            when nonempty |> get 1 is
                Err _ -> Bool.false
                Ok value -> value == 2 

expect 
    nonempty <- [1, 2] |> fromList |> test
    value <- nonempty |> get 1 |> test
    value == 2             
    
test: Result a *, (a -> Bool) -> Bool
test = \res, pred ->
    when res is
        Err _ -> Bool.false
        Ok x -> x |> pred