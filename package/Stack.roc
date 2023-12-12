## #Stack
## A simple (Stack)[https://en.wikipedia.org/wiki/Stack_(abstract_data_type)] data type. A `Stack` is like a `List` but where only the top of the `Stack` accessible. Construct one either by calling `fromList` or by creating an `empty` `Stack` and `push`ing elements onto the stack. Access the top element using `pop`.
## Example:
##```
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

##Create a `Stack` from a `List`. The last element of the list will be the top element of the stack.  
##```
##expect Stack.fromList [] == Stack.empty {}
##expect Stack.fromList ["a","b"] == Stack.empty {} |> Stack.push "a" |> Stack.push "b"
##```  
fromList: List a -> Stack a
fromList = \lst -> @Stack lst    

expect fromList [] == empty {}
expect fromList ["a","b"] == empty {} |> push "a" |> push "b"

##Push an element onto the stack.    
##```
##Stack.empty {} |> Stack.push "a"
##```
push: Stack a, a -> Stack a
push = \@Stack lst, a ->
    @Stack (lst |> List.append a)  

expect empty {} |> push "a" == fromList ["a"] 
expect empty {} |> push "a" |> push "b" == fromList ["a", "b"]


##Pop the top element off of the stack. Returns the remaining stack and the retreived element if succesful, and `Err StackWasEmpty` if the stack was empty.   
##```
##expect Stack.empty {} |> Stack.pop == Err StackWasEmpty
##expect Stack.empty {} |> Stack.push "a" |> Stack.push "b" |> Stack.pop == Ok {stack: Stack.fromList ["a"], elem: "b"} 
##```   
pop: Stack a -> Result {stack: Stack a, elem: a} [StackWasEmpty]
pop = \@Stack lst ->
    when List.last lst is
        Ok elem ->
            Ok {stack: @Stack (lst |> List.dropLast 1), elem}
        Err _ ->
            Err StackWasEmpty    

expect empty {} |> push "a" |> push "b" |> pop == Ok {stack: fromList ["a"], elem: "b"}
expect empty {} |> pop == Err StackWasEmpty 

## ##Ergonomics
## The following functions are not part of the traditional definition of a stack.

## Determine the number of items on the stack.
##```
## expect Stack.empty {} |> Stack.size == 0
## expect Stack.empty {} |> Stack.push "a" |> Stack.push "b" |> Stack.size == 2
##```
size: Stack * -> Nat
size = \@Stack lst ->
    List.len lst    

expect empty {} |> size == 0
expect empty {} |> push "a" |> push "b" |> size == 2

## Peek at the top item of the stack.
##```
## expect Stack.empty {} |> Stack.peek == Err StackWasEmpty 
## expect 
##     Stack.empty {} 
##        |> Stack.push 1
##        |> Stack.push 2
##        |> Stack.peek 
##        == Ok 2
##```
peek: Stack a -> Result a [StackWasEmpty]
peek = \@Stack lst ->
    _ <- Result.onErr (List.last lst)
    Err StackWasEmpty    

expect empty {} |> peek == Err StackWasEmpty
expect [1,2,3] |> fromList |> peek == Ok 3         

expect 
    empty {} |> push 1 |> push 2
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

##Determine if the stack is empty.
##```
##expect Stack.empty {} |> Stack.isEmpty
##expect Stack.empty {} |> Stack.push 1 |> Stack.isEmpty |> Bool.not
##```
isEmpty: Stack * -> Bool
isEmpty = \@Stack lst ->
    lst |> List.isEmpty

expect empty {} |> isEmpty

expect empty {} |> push 1 |> isEmpty |> Bool.not


##Place the first stack on top of the second stack.
##```
##expect 
##    stack1 = Stack.empty {} |> Stack.push 1
##    stack2 = Stack.empty {} |> pStack.ush 2
##    stack1 
##        |> Stack.onTopOf stack2
##        |> Stack.peek
##        == Ok 1
##```
onTopOf : Stack a, Stack a -> Stack a
onTopOf = \@Stack l1, @Stack l2 ->
    @Stack (l2 |> List.concat l1)

expect 
    stack1 = empty {} |> push 1 |> push 2
    stack2 = empty {} |> push 3 |> push 4
    stack1 |> onTopOf stack2 == fromList [3, 4, 1, 2]

# toList: Stack a -> List a
# toList = \@Stack lst -> lst

# expect 
#     Stack.empty {} |> Stack.push 3 |> Stack.push 2 |> Stack.push 1 |> toList == [3,2,31]

##Descend the stack, performing an action and accumulating a state. Analogous to `walk`ing a `List`.
##```
##expect Stack.empty {} |> Stack.descend 0 Num.add == 0
##expect 
##    Stack.empty {} |> Stack.push "a" |> Stack.push "b" |> Stack.push "c"
##    |> Stack.descend "" Str.concat
##    == "cba"
##```
descend: Stack a, state, (state, a -> state) -> state
descend = \@Stack lst, s, f ->
    lst |> List.walkBackwards s f
    

expect empty {} |> descend 0 Num.add == 0

expect 
    empty {} |> push "a" |> push "b" |> push "c"
        |> descend "" Str.concat
        == "cba"
  