##  #NonEmptyList 
##
## `NonEmptyList` is a non-empty list, useful for modeling data with one or more occurence.
## `NonEmptyList` is likely to be less performant than the built-in List, but is more ergonomic for cases where existence of data is guaranteed.
## There are two ways to construct a `NonEmptyList`. It can either be constructed using `fromList`, or `single`.
## `NonEmptyList` defines many of the same functions as `List`. Certain functions like `drop` and `keep` are more suited to regular `List`s, and are not implemented for `NonEmptyList`. To access them it is best to cast back to a list using `toList`.
interface NonEmptyList2 
    exposes [
        fromList, toList,
        isEmpty, get, replace, set, update, append, appendIfOk, len, withCapacity,
        #reserve, releaseExcessCapacity, concat, last, single, repeat, reverse, join, contains,
        #walk, walkBackwards, walkUntil, walkBackwardsUntil, walkFrom, walkFromUntil, sum, product,
        #any, all, keepIf, dropIf, countIf, keepOks, keepErrs, map, map2, map3, map4, mapWithIndex, range,
        #sortWith, sortAsc, sortDesc, swap, first, takeFirst, takeLast, dropFirst, dropLast, dropAt,
        #min, max, joinMap, findFirst, findLast, findFirstIndex, findLastIndex, sublist, intersperse,
        #startsWith, endsWith, split, splitFirst, splitLast, chunksOf, mapTry, walkTry
    ]
    imports []


NonEmptyList a := {head: a, tail: List a} implements [Eq]

fromList: List a -> Result (NonEmptyList a) [ListWasEmpty]

expect [] |> fromList == Err ListWasEmpty


toList: NonEmptyList a -> List a

isEmpty: NonEmptyList a -> Bool

get: NonEmptyList a, Nat -> Result a [OutOfBounds]

replace: NonEmptyList a, Nat, a -> {nonemptylist: NonEmptyList a, value: a}