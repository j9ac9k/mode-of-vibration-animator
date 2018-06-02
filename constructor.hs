module Project where

import Numeric.LinearAlgebra
import Data.List
import Data.HashMap.Strict as M

list_nodes :: [(Int, (Double, Double))]
list_nodes = [(1, (0, 0)),
              (2, (1, 0)),
              (3, (0, 1)),
              (4, (1, 1))]

list_edges :: [(Int, (Int, Int))]
list_edges = [(1, (1, 3)),
              (2, (2, 4)),
              (3, (3, 4)),
              (4, (1, 4)),
              (5, (2, 3))]

nodes = M.fromList list_nodes
edges = M.fromList list_edges

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
          
superposition_matrices :: Foldable t => t (Matrix R) -> Matrix R
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
base_list = take (4 * M.size nodes * M.size nodes) (repeat 0)

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

mass_matrix :: Matrix R
mass_matrix = superposition_matrices all_mass_lists

stiffness_matrix :: Matrix R
stiffness_matrix = superposition_matrices all_stiffness_lists



-- V = [ 0., 0., -0.03174424, 0., -0.02270762, 0., -0.02524252, 0.]
