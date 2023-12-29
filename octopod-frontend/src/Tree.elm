module Tree exposing (..)

import List.Extra as List


type Tree a
    = Node String (List (Tree a))
    | Leaf a


getLabel : Tree a -> Maybe String
getLabel t =
    case t of
        Node l _ ->
            Just l

        _ ->
            Nothing


getChildren : Tree a -> List (Tree a)
getChildren t =
    case t of
        Node _ cs ->
            cs

        _ ->
            []


getValue : Tree a -> Maybe a
getValue ts =
    case ts of
        Leaf d ->
            Just d

        _ ->
            Nothing


pathToTree : List String -> a -> Tree a
pathToTree a v =
    case a of
        l :: ls ->
            Node l [ pathToTree ls v ]

        [] ->
            Leaf v


mergeTrees : List (Tree a) -> List (Tree a)
mergeTrees ts =
    let
        defaultCompare a b =
            case ( a, b ) of
                ( Node x _, Node y _ ) ->
                    compare x y

                ( Leaf _, _ ) ->
                    LT

                ( _, Leaf _ ) ->
                    GT
    in
    mergeTreesWithSort defaultCompare ts


mergeTreesWithSort : (Tree a -> Tree a -> Order) -> List (Tree a) -> List (Tree a)
mergeTreesWithSort =
    mergeTreesByWithSort getLabel


mergeTreesByWithSort : (Tree a -> b) -> (Tree a -> Tree a -> Order) -> List (Tree a) -> List (Tree a)
mergeTreesByWithSort mrg cmp ts =
    let
        f x =
            case x of
                ( Node l cs, ns ) ->
                    cs
                        :: List.map getChildren ns
                        |> List.concat
                        |> mergeTreesByWithSort mrg cmp
                        |> List.sortWith cmp
                        |> Node l

                ( Leaf a, _ ) ->
                    Leaf a
    in
    List.gatherEqualsBy mrg ts
        |> List.map f
        |> List.sortWith cmp


getByPath : List (Tree a) -> List String -> Maybe a
getByPath ts p =
    case ( p, ts ) of
        ( l :: ls, _ ) ->
            List.find (\x -> getLabel x == Just l) ts
                |> Maybe.andThen (\x -> getByPath (getChildren x) ls)

        ( [], [ Leaf v ] ) ->
            Just v

        _ ->
            Nothing


map : (a -> b) -> Tree a -> Tree b
map f t =
    case t of
        Node l cs ->
            Node l (List.map (map f) cs)

        Leaf x ->
            Leaf (f x)


values : Tree a -> List a
values t =
    case t of
        Node _ cs ->
            List.concat (List.map values cs)

        Leaf x ->
            [ x ]
