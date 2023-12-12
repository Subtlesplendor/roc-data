## #Queue
## A `Queue` is like a `List` but with a reduced interface. Items can only be accessed at the start of the `Queue`, and only added at the end. Construct a `Queue` either by calling `fromList` or by creating an `empty` `Queue` and `enque` elements at the end. Access the first element using `deque`.
## Example:
## ```
## Jobs = Queue PrintJob
## 
## addPrintJob: Queue -> Queue, 
## addPrintJob = \queue ->
##     queue |> Queue.enque
## 
## nextPrintJob: Queue -> Result {remaining: Queue PrintJob, job: PrintJob} NoJobsAvailable
## nextPrintJob = \jobQueue ->
##     when jobQueue |> Queue.deque is 
##         Err QueueWasEmpty -> Err NoJobsAvailable
##         Ok {queue, elem} -> Ok {remaining: queue, job: elem }
## ```
interface Queue 
    exposes [Queue, empty, size, peek, enque, deque, #Principals
             fromList, isEmpty, inFrontOf, process #Ergonomics
             ]
    imports []


Queue a := List a implements [Eq]

# separator

## ##Basic Primitives

# separator

## Create an empty Queue.
## ```
## emptyQueue = Queue.empty {} 
## ```
empty : {} -> Queue *
empty = \{} ->
    @Queue []

expect empty {} == @Queue []      

## Add ("enque") an element at the end of the queue.    
## ```
## Queue.empty {} |> Queue.enque "a" |> Queue.enque "b"== Queue.fromList ["a", "b"]
## ```
enque: Queue a, a -> Queue a
enque = \@Queue lst, a ->
    @Queue (lst |> List.append a)    

expect empty {} |> enque "a" == fromList ["a"] 
expect empty {} |> enque "a" |> enque "b" == fromList ["a", "b"]
    


## Deque the first element in the queue. Returns the remaining queue and the retreived element if succesful, and `Err QueueWasEmpty` if the queue was empty.   
## ```
## expect Queue.empty {} |> Queue.deque == Err QueueWasEmpty
## expect 
##     Queue.empty {} |> Queue.enque "a" |> Queue.enque "b" |> Queue.deque 
##     == Ok {queue: Queue.empty {} |> Queue.enque "b", elem: "a"} 
## ```      
deque: Queue a -> Result {queue: Queue a, elem: a} [QueueWasEmpty]
deque = \@Queue lst ->
    when List.first lst is
        Ok elem ->
            Ok {queue: @Queue (lst |> List.dropFirst 1), elem}
        Err _ ->
            Err QueueWasEmpty

expect empty {} |> deque == Err QueueWasEmpty            
expect empty {} |> enque "a" |> enque "b" |> deque == Ok {queue: empty {} |> enque "b", elem: "a"}             


## Create a `Queue` from a `List`. The first element of the list will be the first element of the queue.  
## ```
## expect Queue.fromList [] == Queue.empty {}
## expect Queue.fromList ["a","b"] == Queue.empty {} |> Queue.enque "a" |> Queue.enque "b"
## ```   
fromList: List a -> Queue a
fromList = \lst ->
    @Queue lst

expect fromList [] == empty {}    
expect [1,2,3] |> fromList == empty {} |> enque 1 |> enque 2 |> enque 3


#separator

## Ergonomics
## The following functions are not part of the traditional definition of a queue.


## Determine the number of items in the queue.
## ```
## expect Queue.empty {} |> Queue.size == 0
## expect Queue.empty {} |> Queue.enque "a" |> Queue.enque "b" |> Queue.size == 2
## ```
size: Queue * -> Nat
size = \@Queue lst ->
    List.len lst    

expect empty {} |> size == 0
expect ["a", "b", "c"] |> fromList |> size == 3


## Peek at the first item in the queue.
## ```
## expect Queue.empty {} |> Queue.peek == Err QueueWasEmpty 
## expect 
##      Queue.empty {} 
##         |> Queue.enque 1
##         |> Queue.enque 2
##         |> Queue.peek 
##         == Ok 1
## ```
peek: Queue a -> Result a [QueueWasEmpty]
peek = \@Queue lst ->
    _ <- Result.onErr (List.first lst)
    Err QueueWasEmpty    

expect empty {} |> peek == Err QueueWasEmpty       

expect 
    empty {} 
        |> enque 1
        |> enque 2
        |> peek 
        == Ok 1

expect
    err = empty {} |> deque
    when err is
        Ok _ -> Bool.false
        Err QueueWasEmpty -> Bool.true

expect 
    res = 
        empty {} 
        |> enque 1 
        |> enque 2 
        |> deque 
    when res is
        Ok {queue} ->
                peek queue == Ok 2
        Err _ -> Bool.false         


## Determine if the queue is empty.
## ```
## expect Queue.empty {} |> Queue.isEmpty
## expect Queue.empty {} |> Queue.enque 1 |> Queue.isEmpty |> Bool.not
## ```
isEmpty: Queue * -> Bool
isEmpty = \@Queue lst ->
    lst |> List.isEmpty

expect empty {} |> isEmpty
expect empty {} |> enque 1 |> isEmpty |> Bool.not

## Place the first queue in front of the second queue.
## ```
## expect 
##     queue1 = Queue.empty {} |> Queue.enque 1 |> Queue.enque 2
##     queue2 = Queue.empty {} |> Queue.enque 3 |> Queue.enque 4
##     queue1 
##         |> Queue.inFrontOf queue2
##         == Queue.fromList [1, 2, 3 ,4]
## ```
inFrontOf : Queue a, Queue a -> Queue a
inFrontOf = \@Queue l1, @Queue l2 ->
    @Queue (l1 |> List.concat l2)

expect 
    queue1 = empty {} |> enque 1 |> enque 2
    queue2 = empty {} |> enque 3 |> enque 4
    queue1 
        |> inFrontOf queue2
        == fromList [1, 2, 3 ,4]  

# toList: Queue a -> List a
# toList = \@Queue lst -> lst

# expect 
#     empty {} |> enque 3 |> enque 2 |> enque 1 |> toList == [3,2,1]


## Process the queue, performing an action and accumulating a state. Analogous to `walk`ing a `List`.
## ```
## expect Queue.empty {} |> Queue.process 0 Num.add == 0
## expect 
##     Queue.empty {} |> Queue.enque "a" |> Queue.enque "b" |> Queue.enque "c"
##     |> Queue.process "" Str.concat
##     == "abc"
## ```
process: Queue a, state, (state, a -> state) -> state
process = \@Queue lst, s, f ->
    lst |> List.walk s f
    
expect empty {} |> process 0 Num.add == 0    
expect 
    empty {} |> enque "a" |> enque "b" |> enque "c"
        |> process "" Str.concat 
        == "abc"
  