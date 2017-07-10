module Extra.Graph exposing (fromEdges)

import Graph exposing (Edge, Graph, Node, NodeId)


indexOf : a -> List a -> Int
indexOf item list =
    let
        help l i =
            case l of
                [] ->
                    -1

                head :: tail ->
                    if head == item then
                        i
                    else
                        help tail (i + 1)
    in
    help list 0


fromEdges : (node -> node -> Order) -> List ( node, List node ) -> Graph node ()
fromEdges compareFn edges =
    let
        sorted =
            List.sortWith (\( k1, _ ) ( k2, _ ) -> compareFn k1 k2) edges

        orderedKeys =
            List.map (\( k, _ ) -> k) sorted

        edgePairs =
            List.concatMap
                (\( k1, ks ) ->
                    let
                        left =
                            indexOf k1 orderedKeys
                    in
                    List.map (\k2 -> ( left, indexOf k2 orderedKeys )) ks
                )
                sorted
    in
    Graph.fromNodeLabelsAndEdgePairs orderedKeys edgePairs
