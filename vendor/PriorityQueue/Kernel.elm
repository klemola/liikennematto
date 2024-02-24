module PriorityQueue.Kernel exposing
    ( PriorityQueue, Priority
    , empty, insert
    , isEmpty, head, tail
    )

{-| The kernel of this package.

The implementation we provide here is based on Okasaki's _leftist heaps_. See [Purely Functional Data Structures](https://www.cs.cmu.edu/~rwh/theses/okasaki.pdf) for more information.


# Types

@docs PriorityQueue, Priority


# Building

@docs empty, insert


# Query

@docs isEmpty, head, tail

-}


{-| The core type of this module.
-}
type PriorityQueue a
    = PriorityQueue
        { priority : Priority a
        , tree : Tree a
        }


{-| Determines the priority of an element.

Lower values correspond to higher priority and vice versa.

-}
type alias Priority a =
    a -> Int


{-| Create an empty `PriorityQueue`.
-}
empty : Priority a -> PriorityQueue a
empty priority =
    PriorityQueue { priority = priority, tree = emptyTree }


{-| Insert an element into the `PriorityQueue`
-}
insert : a -> PriorityQueue a -> PriorityQueue a
insert element (PriorityQueue { priority, tree }) =
    let
        a =
            singleton element

        merged =
            merge priority a tree
    in
    PriorityQueue { priority = priority, tree = merged }


{-| Determine if a `PriorityQueue` is empty.
-}
isEmpty : PriorityQueue a -> Bool
isEmpty (PriorityQueue { tree }) =
    case tree of
        Empty ->
            True

        _ ->
            False


{-| Return the element of the `PriorityQueue` with the highest priority.

Returns `Nothing` when the queue is empty.

-}
head : PriorityQueue a -> Maybe a
head (PriorityQueue { tree }) =
    case tree of
        Empty ->
            Nothing

        Node _ element _ _ ->
            Just element


{-| Return the `PriorityQueue` that remains when the head is removed.
-}
tail : PriorityQueue a -> PriorityQueue a
tail ((PriorityQueue { priority, tree }) as queue) =
    case tree of
        Empty ->
            queue

        Node _ _ a b ->
            let
                merged =
                    merge priority a b
            in
            PriorityQueue { priority = priority, tree = merged }


{-| heap-ordered binary tree with the _leftist property_

The leftist property is that the _rank_ of any left child is at least as large as that of its right sibling. The rank of a node is defined as the length of its _right spine_, i.e. the right-most path of the node in question to an empty node.

-}
type Tree a
    = Empty
    | Node Int a (Tree a) (Tree a)


{-| An `emptyTree`.
-}
emptyTree : Tree a
emptyTree =
    Empty


{-| Create a `Tree` with a single element.
-}
singleton : a -> Tree a
singleton element =
    Node 1 element Empty Empty


{-| Merge two trees while maintaining the _leftist property_
-}
merge : Priority a -> Tree a -> Tree a -> Tree a
merge priority left right =
    case ( left, right ) of
        ( Empty, _ ) ->
            right

        ( _, Empty ) ->
            left

        ( Node _ x a b, Node _ y u v ) ->
            if priority x <= priority y then
                make x a (merge priority b right)

            else
                make y u (merge priority left v)


{-| Create a non empty tree from an element and two sub-trees, keeping the _leftist property_ intact.
-}
make : a -> Tree a -> Tree a -> Tree a
make element a b =
    if rank a >= rank b then
        Node (1 + rank b) element a b

    else
        Node (1 + rank a) element b a


{-| Determine the _rank_ of a tree.
-}
rank : Tree a -> Int
rank tree =
    case tree of
        Empty ->
            0

        Node r _ _ _ ->
            r
