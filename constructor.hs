module Constructor where

import Numeric.LinearAlgebra
import Data.List
import Data.Set
import Data.HashMap.Strict as M
import BridgeStructure as Structure


nodes = M.fromList Structure.list_nodes
edges = M.fromList Structure.list_edges
fixtures = M.fromList Structure.list_node_fixtures

elastic_mod = 400000000.0 :: Double
rho = 7850.0 :: Double
cross_sec_area = 0.04361282 :: Double


-- better error handling for imporper lookups?

calc_length :: Int -> Double
calc_length edge_id = sqrt(delta_x * delta_x + delta_y * delta_y)
    where (node_1_id, node_2_id) = M.lookupDefault (0, 0) edge_id edges
          (x1, y1) = M.lookupDefault (0, 0) node_1_id nodes
          (x2, y2) = M.lookupDefault (1, 1) node_2_id nodes
          delta_x = x2 - x1
          delta_y = y2 - y1
          
superposition_matrices :: Foldable t => t (Matrix Double) -> Matrix Double
superposition_matrices = Prelude.foldr (+) (((2 * M.size nodes)><(2 * M.size nodes)) (repeat 0))

index_x1 :: (Int, Int) -> Int
index_x1 (first, second) = (first * 2) - 2

index_x2 :: (Int, Int) -> Int
index_x2 (first, second) = (second * 2) - 2

index_y1 :: (Int, Int) -> Int
index_y1 (first, second) = (first * 2) - 1

index_y2 :: (Int, Int) -> Int
index_y2 (first, second) = (second * 2) - 1

x1_x1 :: Int -> Int
x1_x1 edge_id = index_x1 connections * 2 * M.size nodes + index_x1 connections
    where connections = M.lookupDefault (0, 0) edge_id edges

x1_x2 :: Int -> Int
x1_x2 edge_id = index_x1 connections * 2 * M.size nodes + index_x2 connections
    where connections = M.lookupDefault (0, 0) edge_id edges

x2_x1 :: Int -> Int
x2_x1 edge_id = index_x2 connections * 2 * M.size nodes + index_x1 connections
    where connections = M.lookupDefault (0, 0) edge_id edges

x2_x2 :: Int -> Int
x2_x2 edge_id = index_x2 connections * 2 * M.size nodes + index_x2 connections
    where connections = M.lookupDefault (0, 0) edge_id edges

y1_y1 :: Int -> Int
y1_y1 edge_id = index_y1 connections * 2 * M.size nodes + index_y1 connections
    where connections = M.lookupDefault (0, 0) edge_id edges

y1_y2 :: Int -> Int
y1_y2 edge_id = index_y1 connections * 2 * M.size nodes + index_y2 connections
    where connections = M.lookupDefault (0, 0) edge_id edges

y2_y1 :: Int -> Int
y2_y1 edge_id = index_y2 connections * 2 * M.size nodes + index_y1 connections
    where connections = M.lookupDefault (0, 0) edge_id edges

y2_y2 :: Int -> Int
y2_y2 edge_id = index_y2 connections * 2 * M.size nodes + index_y2 connections
    where connections = M.lookupDefault (0, 0) edge_id edges

base_list :: [Double]
base_list = Data.List.take (4 * M.size nodes * M.size nodes) (repeat 0)

gen_mass_list :: Int -> Int -> Double -> Double
gen_mass_list index edge_id value
    | index == x1_x1 edge_id = 2 * m + value
    | index == x1_x2 edge_id = m + value
    | index == x2_x1 edge_id = m + value
    | index == x2_x2 edge_id = 2 * m + value
    | index == y1_y1 edge_id = 2 * m + value
    | index == y1_y2 edge_id = m + value
    | index == y2_y1 edge_id = m + value
    | index == y2_y2 edge_id = 2 * m + value
    | otherwise              = value
    where
        m = rho * cross_sec_area * truss_length / 6
        truss_length = calc_length edge_id

gen_stiffness_list :: Int -> Int -> Double -> Double
gen_stiffness_list index edge_id value
    | index == x1_x1 edge_id = value + k
    | index == x1_x2 edge_id = value - k
    | index == x2_x1 edge_id = value - k
    | index == x2_x2 edge_id = value + k
    | otherwise              = value
    where
        k = elastic_mod * cross_sec_area / (calc_length edge_id)

elemental_mass_list :: Int -> Matrix Double
elemental_mass_list edge_id = ((2 * M.size nodes)><(2 * M.size nodes))[gen_mass_list index edge_id value | (index, edge_id, value) <- zip3 [0..] (repeat edge_id) base_list]

elemental_stiffness_list :: Int -> Matrix Double
elemental_stiffness_list edge_id = ((2 * M.size nodes)><(2 * M.size nodes))[gen_stiffness_list index edge_id value | (index, edge_id, value) <- zip3 [0..] (repeat edge_id) base_list]

all_mass_lists :: [Matrix Double]
all_mass_lists = [elemental_mass_list edge_id | edge_id <- M.keys edges]

all_stiffness_lists :: [Matrix Double]
all_stiffness_lists = [elemental_stiffness_list edge_id | edge_id <- M.keys edges]

mass_matrix :: Matrix Double
mass_matrix = superposition_matrices all_mass_lists

stiffness_matrix :: Matrix Double
stiffness_matrix = superposition_matrices all_stiffness_lists

remove_row :: Element a => Int -> Matrix a -> Matrix a
remove_row row matrix = fromLists (h ++ (tail t))
        where (h, t) = Data.List.splitAt row (toLists matrix)

remove_col :: Element a => Int -> Matrix a -> Matrix a
remove_col col matrix = fromLists [h ++ (tail t)| row <- toLists matrix, let (h, t) = Data.List.splitAt col row]

remove_row_col :: Element a => Int -> Matrix a -> Matrix a
remove_row_col index matrix = remove_col index (remove_row index matrix)

indexes_to_remove :: [(Int, (Bool, Bool))] -> [Int]
indexes_to_remove [] = []
indexes_to_remove (x:xs)
        | x_fixed && y_fixed = 2 * id - 2 : 2 * id - 1 : indexes_to_remove xs
        | x_fixed && not y_fixed = 2 * id - 2 : indexes_to_remove xs
        | not x_fixed && y_fixed = 2 * id - 1 : indexes_to_remove xs
        where
            id = fst x
            x_fixed = fst (snd x)
            y_fixed = snd (snd x)

ordered_indexes_to_remove :: [Int]
ordered_indexes_to_remove = reverse(sort(indexes_to_remove list_node_fixtures))

slim_global_matrices :: Element a => [Int] -> Matrix a -> Matrix a
slim_global_matrices [] matrix = matrix
slim_global_matrices (x:xs) matrix = slim_global_matrices xs (remove_row_col x matrix)

slimmed_mass_matrix :: Matrix Double
slimmed_mass_matrix = slim_global_matrices ordered_indexes_to_remove mass_matrix

slimmed_stiffness_matrix :: Matrix Double
slimmed_stiffness_matrix = slim_global_matrices ordered_indexes_to_remove stiffness_matrix

l :: Vector Double
v :: Matrix Double
(l, v) = geigSH (trustSym slimmed_stiffness_matrix) (trustSym slimmed_mass_matrix)

-- need to reinsert 0's into v

zero_row :: [Double]
zero_row = Data.List.take (cols v) (repeat 0)

inserted_indexes :: Set Int
inserted_indexes = Data.Set.fromList ordered_indexes_to_remove

merge_v :: [[Double]] -> [[Double]]
merge_v [] = []
merge_v xs = merge_v' xs 0
    where
        merge_v' xs index
                | Data.Set.member index inserted_indexes = zero_row : merge_v' xs (index + 1)
                | xs == []                               = []
                | otherwise                              = head xs : merge_v' (tail xs) (index + 1)


padded_v :: Matrix Double
padded_v = fromLists (merge_v (toLists v))

eigen_pairs :: [(Double, [Double])]
eigen_pairs = sort [(sqrt (maximum [0, eigenvalue] / (2 * pi)), eigenvector) | (eigenvalue, eigenvector) <- zip (Numeric.LinearAlgebra.toList l) (toLists (tr padded_v)), eigenvalue > 1]

to_animate :: [Double]
freq :: Double
(freq, to_animate) = head eigen_pairs


-- V = [ 0., 0., -0.03174424, 0., -0.02270762, 0., -0.02524252, 0.]
