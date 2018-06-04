module BridgeStructure where

list_nodes :: [(Int, (Double, Double))]
list_nodes = [(1, (0, 0)),
              (2, (10, 15)),
              (3, (20, 00)),
              (4, (30, 15)),
              (5, (40, 0)),
              (6, (50, 15)),
              (7, (60, 0)),
              (8, (70, 15)),
              (9, (90, 0))]


list_node_fixtures :: [(Int, (Bool, Bool))]
list_node_fixtures = [(1, (True, True)),
                     (9, (False, True))]

list_edges :: [(Int, (Int, Int))]
list_edges = [(1, (1, 2)),
              (2, (1, 3)),
              (3, (2, 3)),
              (4, (2, 4)),
              (5, (3, 4)),
              (6, (3, 5)),
              (7, (4, 5)),
              (8, (4, 6)),
              (9, (5, 6)),
              (10, (5, 7)),
              (11, (6, 7)),
              (12, (6, 8)),
              (13, (7, 8)),
              (14, (7, 9)),
              (15, (8, 9))]