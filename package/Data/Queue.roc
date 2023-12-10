interface Data.Queue 
    exposes [Queue, new, size, peek, enque, deque, #Principals
             fromList, toList, isEmpty, before, process #Ergonomics
             ]
    imports []


Queue a := List a

# -- Core -------

new : {} -> Queue *
new = \{} ->
    @Queue []

size: Queue * -> Nat
size = \@Queue lst ->
    List.len lst    

expect new {} |> size == 0

peek: Queue a -> Result a [QueueWasEmpty]
peek = \@Queue lst ->
    _ <- Result.onErr (List.first lst)
    Err QueueWasEmpty    

expect 
    new {} 
        |> peek 
        == Err QueueWasEmpty       



enque: Queue a, a -> Queue a
enque = \@Queue lst, a ->
    @Queue (lst |> List.append a)

expect 
    new {} 
        |> enque 1
        |> enque 2
        |> peek 
        == Ok 1

deque: Queue a -> Result {queue: Queue a, elem: a} [QueueWasEmpty]
deque = \@Queue lst ->
    when List.first lst is
        Ok elem ->
            Ok {queue: @Queue (lst |> List.dropFirst 1), elem}
        Err _ ->
            Err QueueWasEmpty

expect
    err = new {} |> deque
    when err is
        Ok _ -> Bool.false
        Err QueueWasEmpty -> Bool.true

expect 
    res = 
        new {} 
        |> enque 1 
        |> enque 2 
        |> deque 
    when res is
        Ok {queue} ->
                peek queue == Ok 2
        Err _ -> Bool.false         


# -- Ergonomics --------

isEmpty: Queue * -> Bool
isEmpty = \@Queue lst ->
    lst |> List.isEmpty

expect new {} |> isEmpty

expect new {} |> enque 1 |> isEmpty |> Bool.not

before : Queue a, Queue a -> Queue a
before = \@Queue l1, @Queue l2 ->
    @Queue (l1 |> List.concat l2)

expect 
    queue1 = new {} |> enque 1
    queue2 = new {} |> enque 2
    queue1 
        |> before queue2
        |> peek
        == Ok 1

fromList: List a -> Queue a
fromList = \lst ->
    @Queue lst

expect [1,2,3] |> fromList |> peek == Ok 1    

toList: Queue a -> List a
toList = \@Queue lst -> lst

expect 
    new {} |> enque 3 |> enque 2 |> enque 1 |> toList == [3,2,1]

process: Queue a, state, (state, a -> state) -> state
process = \@Queue lst, s, f ->
    lst |> List.walk s f
    
expect 
    queue = ["a","b","c"] |> fromList
    queue |> process "" Str.concat == "abc"
  