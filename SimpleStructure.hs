module SimpleStructure where

list_nodes :: [(Int, (Double, Double))]
list_nodes = [(1, (0, 0)),
              (2, (1, 0)),
              (3, (0, 1)),
              (4, (1, 1))]

list_node_fixtures :: [(Int, (Bool, Bool))]
list_node_fixtures = [(1, (True, True)),
                    (2, (False, True))]

list_edges :: [(Int, (Int, Int))]
list_edges = [(1, (1, 3)),
            (2, (2, 4)),
            (3, (3, 4)),
            (4, (1, 4)),
            (5, (2, 3))]

elastic_mod = 400000000.0 :: Double
rho = 7850.0 :: Double
cross_sec_area = 0.04361282 :: Double