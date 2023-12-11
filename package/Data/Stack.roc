## #Stack
## A simple (Stack)[https://en.wikipedia.org/wiki/Stack_(abstract_data_type)] data type. A `Stack` is like a `List` but where only the top of the `Stack` accessible. Construct one either by calling `fromList` or by creating an `empty` `Stack` and `push`ing elements onto the stack. Access the top element using `pop`.
## Example:
##```
##Page = Nat
##State = {current: Page, back: Stack Page, forward: Stack Page }
##goBack: State -> State
##goBack = \state ->
##    {current, back, forward} = state
##    when Stack.pop back is
##        Err StackWasEmpty -> state
##        Ok {stack, elem} ->
##            {current: elem, back: stack, forward: forward |> Stack.push current}
##```

interface Data.Stack 
    exposes [Stack, empty, size, peek, push, pop, #Principals
             fromList, isEmpty, onTopOf, descend #Ergonomics
             ]
    imports []


Stack a := List a implements [Eq]

## ##Basic Primitives

# separator

##Create an empty Stack.
##```
##emptyStack = Stack.empty {} 
##```
empty : {} -> Stack *
empty = \{} ->
    @Stack []

expect empty {} == @Stack []    

##Create a `Stack` from a `List`. The first element of the list will be the top element of the stack.  
##```
##expect Stack.fromList [] == Stack.empty {}
## expect Stack.fromList ["a","b"] == empty {} |> Stack.push "b" |> Stack.push "a"
##```  
fromList: List a -> Stack a
fromList = \lst ->
    @Stack (lst |> List.reverse)    

##Push an element onto the stack.    
##```
##empty {} |> push "a"
##```
push: Stack a, a -> Stack a
push = \@Stack lst, a ->
    @Stack (lst |> List.append a)  

##Pop the top element off of the stack. Returns the remaining stack and the retreived element if succesful, and `Err StackWasEmpty` if the stack was empty.   
##```
##empty {} |> push "a" |>
##```   
pop: Stack a -> Result {stack: Stack a, elem: a} [StackWasEmpty]
pop = \@Stack lst ->
    when List.last lst is
        Ok elem ->
            Ok {stack: @Stack (lst |> List.dropLast 1), elem}
        Err _ ->
            Err StackWasEmpty    

## ##Ergonomics
## These are extra ergonomics that may be useful with stacks.

## Determine the number of items on the `Stack`.
##```
## expect empty {} |> size == 0
## expect empty {} |> push "a" |> push "b" |> size == 2
##```
size: Stack * -> Nat
size = \@Stack lst ->
    List.len lst    

expect empty {} |> size == 0
expect empty {} |> push "a" |> push "b" |> size == 2

## Peek at the top of the stack.
##```
## expect empty {} |> peek == Err StackWasEmpty 
## expect 
##     empty {} 
##        |> push 1
##        |> push 2
##        |> peek 
##        == Ok 2
##```
peek: Stack a -> Result a [StackWasEmpty]
peek = \@Stack lst ->
    _ <- Result.onErr (List.last lst)
    Err StackWasEmpty    

expect empty {} |> peek == Err StackWasEmpty       

expect 
    empty {} 
        |> push 1
        |> push 2
        |> peek 
        == Ok 2

expect
    err = empty {} |> pop
    when err is
        Ok _ -> Bool.false
        Err StackWasEmpty -> Bool.true

expect 
    res = 
        empty {} 
        |> push 1 
        |> push 2 
        |> pop 
    when res is
        Ok {stack} ->
                peek stack == Ok 1
        Err _ -> Bool.false         


# -- Ergonomics --------
##Determine if the stack is empty.
##```
##expect empty {} |> isEmpty
##expect empty {} |> push 1 |> isEmpty |> Bool.not
##```
isEmpty: Stack * -> Bool
isEmpty = \@Stack lst ->
    lst |> List.isEmpty

expect empty {} |> isEmpty

expect empty {} |> push 1 |> isEmpty |> Bool.not


# -- Ergonomics --------
##Place the first stack on top of the second stack.
##```
##expect 
##stack1 = empty {} |> push 1
##stack2 = empty {} |> push 2
##stack1 
##    |> onTopOf stack2
##    |> peek
##    == Ok 1
##```
onTopOf : Stack a, Stack a -> Stack a
onTopOf = \@Stack l1, @Stack l2 ->
    @Stack (l2 |> List.concat l1)

expect 
    stack1 = empty {} |> push 1
    stack2 = empty {} |> push 2
    stack1 
        |> onTopOf stack2
        |> peek
        == Ok 1

expect [1,2,3] |> fromList |> peek == Ok 1    

# toList: Stack a -> List a
# toList = \@Stack lst ->
#     lst |> List.reverse

# expect 
#     empty {} |> push 3 |> push 2 |> push 1 |> toList == [1,2,3]

##Descend the `Stack`, performing an action and accumulating a state. Analogous to `walk`ing a `List`.
##```
##expect 
##    ["a","b","c"] 
##    |> fromList
##    |> descend "" Str.concat
##    == "abc"
##```
descend: Stack a, state, (state, a -> state) -> state
descend = \@Stack lst, s, f ->
    lst |> List.walkBackwards s f
    
expect 
    ["a","b","c"] 
        |> fromList
        |> descend "" Str.concat
         == "abc"
  