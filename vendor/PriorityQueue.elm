module PriorityQueue exposing
    ( PriorityQueue
    , empty, insert, fromList
    , head, tail, pop, take, drop, isEmpty
    , toList
    )

{-| A priority queue for Elm.

A [_priority queue_](https://en.wikipedia.org/wiki/Priority_queue) is an

> abstract data type which is like a regular queue or stack data structure, but where additionally each element has a "priority" associated with it. In a priority queue, an element with high priority is served before an element with low priority.

The implementation we provide here is based on Okasaki's _leftist heaps_. See [Purely Functional Data Structures](https://www.cs.cmu.edu/~rwh/theses/okasaki.pdf) for more information.


# Priority

Throughout this package `priority` will mean a function `a -> Int` that assigns a integer value to an element. Lower values indicate higher priority and vice versa.


# Types

@docs PriorityQueue


# Building

@docs empty, insert, fromList


# Query

@docs head, tail, pop, take, drop, isEmpty


# Conversion

@docs toList

-}

import PriorityQueue.Kernel as Kernel


{-| The abstract datatype of this package.
-}
type alias PriorityQueue a =
    Kernel.PriorityQueue a


{-| Create an empty `PriorityQueue`.

Must be given a `priority` function, i.e. a function that assigns the priority to elements.

Say we want to process rectangles, making sure to process rectangles with bigger area first

    type alias Rectangle =
        { width : Int
        , height : Int
        }

    area : Rectangle -> Int
    area { width, height } =
        width * height

If we wanted to create a queue of rectangles, where higher priority is awarded to rectangles with a bigger area the following code would create the queue.

    queue : PriorityQueue Rectangle
    queue =
        let
            priority =
                area >> negate
        in
        empty priority

Note that the area is negated to give higher priority to rectangles with bigger area.

-}
empty : (a -> Int) -> PriorityQueue a
empty priority =
    Kernel.empty priority


{-| Insert an element into the `PriorityQueue`

    let
        -- a nice rectangle
        rectangle =
            { width = 5, height = 3 }
    in
    queue
        |> insert rectangle

-}
insert : a -> PriorityQueue a -> PriorityQueue a
insert element queue =
    Kernel.insert element queue


{-| Creates a `PriorityQueue` from a given list of elements.

Must be given a `priority` function, i.e. a function that assigns the priority to elements.

Assume again that rectangles with bigger area have higher priority, the following code creates a queue from a list of rectangles.

    rectangles : List Rectangle
    rectangles =
        [ { width = 1, height = 1 }
        , { width = 5, height = 3 }
        , { width = 2, height = 4 }
        ]

    queue : PriorityQueue Rectangle
    queue =
        let
            priority =
                area >> negate
        in
        fromList priority rectangles

Note that the area is negated to give higher priority to rectangles with bigger area.

-}
fromList : (a -> Int) -> List a -> PriorityQueue a
fromList priority elements =
    let
        emptyQueue =
            Kernel.empty priority
    in
    List.foldl Kernel.insert emptyQueue elements


{-| Return the element of the `PriorityQueue` with the highest priority.

    empty (area >> negate)
        |> insert { width = 1, height = 1 }
        |> insert { width = 5, height = 3 }
        |> insert { width = 2, height = 2 }
        |> head     -- Just { width = 5, height = 3 }

Returns `Nothing` when the queue is empty.

    empty (area >> negate)
        |> head     -- Nothing

-}
head : PriorityQueue a -> Maybe a
head queue =
    Kernel.head queue


{-| Return the `PriorityQueue` that remains when the head is removed.

    empty (area >> negate)
        |> insert { width = 1, height = 1 }
        |> insert { width = 5, height = 3 }
        |> insert { width = 2, height = 2 }
        |> tail

would leave a priority queue that contains the rectangles `{ width = 1, height = 1 }` and `{ width = 2, height 2 }`.

-}
tail : PriorityQueue a -> PriorityQueue a
tail queue =
    Kernel.tail queue


{-| Returns a tuple with the head of a queue and its tail.

    -- For any queue
    pop queue == ( head queue, tail queue )

-}
pop : PriorityQueue a -> ( Maybe a, PriorityQueue a )
pop queue =
    ( head queue, tail queue )


{-| Return the first `n` elements of the `PriorityQueue` with the highest priority

    empty (area >> negate)
        |> insert { width = 1, height = 1 }
        |> insert { width = 5, height = 3 }
        |> insert { width = 2, height = 2 }
        |> take 2   -- [ { width = 5, height = 3 }, { width = 2, heigh = 2 }]

-}
take : Int -> PriorityQueue a -> List a
take n queue =
    tailCallTake [] n queue


{-| tail call variant of take
-}
tailCallTake : List a -> Int -> PriorityQueue a -> List a
tailCallTake accumulator n queue =
    if n <= 0 then
        List.reverse accumulator

    else
        case head queue of
            Just value ->
                tailCallTake (value :: accumulator) (n - 1) (tail queue)

            Nothing ->
                List.reverse accumulator


{-| Returns a new `PriorityQueue` with the first `n` elements dropped.

    empty (area >> negate)
        |> insert { width = 1, height = 1 }
        |> insert { width = 5, height = 3 }
        |> insert { width = 2, height = 2 }
        |> drop 2

would leave a priority queue that only contains the rectangle `{ width = 1, height = 1 }`.

-}
drop : Int -> PriorityQueue a -> PriorityQueue a
drop n queue =
    if n <= 0 || isEmpty queue then
        queue

    else
        drop (n - 1) (tail queue)


{-| Determine if the `PriorityQueue` is empty.
-}
isEmpty : PriorityQueue a -> Bool
isEmpty queue =
    Kernel.isEmpty queue


{-| Returns all the elements in a `PriorityQueue` as a `List`.

The order will be determined by the priority, higher priority elements before lower priority elements.

    empty (area >> negate)
        |> insert { width = 1, height = 1 }
        |> insert { width = 5, height = 3 }
        |> insert { width = 2, height = 2 }
        |> toList   -- [ { width = 5, height = 3 }, { width = 2, height = 2 }, { width = 1, height = 1 } ]

-}
toList : PriorityQueue a -> List a
toList queue =
    tailRecursiveToList [] queue


{-| [Tail-recursive][https://en.wikipedia.org/wiki/Tail_call] version of `toList`.
-}
tailRecursiveToList : List a -> PriorityQueue a -> List a
tailRecursiveToList accumulator queue =
    case Kernel.head queue of
        Nothing ->
            List.reverse accumulator

        Just element ->
            tailRecursiveToList (element :: accumulator) (Kernel.tail queue)
