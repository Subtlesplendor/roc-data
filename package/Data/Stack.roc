interface Data.Stack 
    exposes [Stack, new, size, peek, push, pop, #Principals
             fromList, toList, isEmpty, onTopOf, descend #Ergonomics
             ]
    imports []


Stack a := List a

# -- Core -------

new : {} -> Stack *
new = \{} ->
    @Stack []

size: Stack * -> Nat
size = \@Stack lst ->
    List.len lst    

expect new {} |> size == 0

peek: Stack a -> Result a [StackWasEmpty]
peek = \@Stack lst ->
    _ <- Result.onErr (List.last lst)
    Err StackWasEmpty    

expect 
    new {} 
        |> peek 
        == Err StackWasEmpty       



push: Stack a, a -> Stack a
push = \@Stack lst, a ->
    @Stack (lst |> List.append a)

expect 
    new {} 
        |> push 1
        |> push 2
        |> peek 
        == Ok 2

pop: Stack a -> Result {stack: Stack a, elem: a} [StackWasEmpty]
pop = \@Stack lst ->
    when List.last lst is
        Ok elem ->
            Ok {stack: @Stack (lst |> List.dropLast 1), elem}
        Err _ ->
            Err StackWasEmpty

expect
    err = new {} |> pop
    when err is
        Ok _ -> Bool.false
        Err StackWasEmpty -> Bool.true

expect 
    res = 
        new {} 
        |> push 1 
        |> push 2 
        |> pop 
    when res is
        Ok {stack} ->
                peek stack == Ok 1
        Err _ -> Bool.false         


# -- Ergonomics --------

isEmpty: Stack * -> Bool
isEmpty = \@Stack lst ->
    lst |> List.isEmpty

expect new {} |> isEmpty

expect new {} |> push 1 |> isEmpty |> Bool.not

onTopOf : Stack a, Stack a -> Stack a
onTopOf = \@Stack l1, @Stack l2 ->
    @Stack (l2 |> List.concat l1)

expect 
    stack1 = new {} |> push 1
    stack2 = new {} |> push 2
    stack1 
        |> onTopOf stack2
        |> peek
        == Ok 1

fromList: List a -> Stack a
fromList = \lst ->
    @Stack (lst |> List.reverse)

expect [1,2,3] |> fromList |> peek == Ok 1    

toList: Stack a -> List a
toList = \@Stack lst ->
    lst |> List.reverse

expect 
    new {} |> push 3 |> push 2 |> push 1 |> toList == [1,2,3]

descend: Stack a, state, (state, a -> state) -> state
descend = \@Stack lst, s, f ->
    lst |> List.walkBackwards s f
    
expect 
    stack = ["a","b","c"] |> fromList
    stack |> descend "" Str.concat == "abc"
  