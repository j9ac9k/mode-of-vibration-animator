# Truss Structure Natural Frequency Vibration Viewer

## Summary

The Mode of Vibration Animator takes in a file containing structure geometry, calculates the natural frequencies, and animates the mode of vibration. This project is largely a learning exercise for me to learn Haskell, and my submission for CS 557 Functional Languages at Portland State University.

## Installation

Ubuntu linux

```bash
# Install Haskell stack dependencies
sudo apt-get install g++ gcc libc6-dev libffi-dev libgmp-dev make xz-utils zlib1g-dev git gnupg

# Install the Haskell Stack on Linux
wget -qO- https://get.haskellstack.org/ | sh

# LAPACK and BLAS needed for hmatrix dependency
sudo apt-get install libblas-dev liblapack-dev

git clone git@github.com:j9ac9k/mode-of-vibration-animator.git
cd mode-of-vibration-animator
stack build
```

MacOS

```bash
# need homebrew installed prior to
brew install haskell-stack

git clone git@github.com:j9ac9k/mode-of-vibration-animator.git
cd mode-of-vibration-animator
stack build
```

## Running

To run the compiled stack built variant from the terminal run:

```bash
stack exec vibration-animator
```

If you are editing `Constructor.hs`, with the goal of viewing the different modes of vibration, you can either run the script from `ghci`

```bash
cd src
ghci -fno-ghci-sanbox
Prelude> :l Animator.hs
*Animator> start_animation
```

Or you can rebuild with stack

```bash
stack build
stack exec vibration-animator
```

## Tunable Parameters

### Structure Geometry

`Constructor.hs` incorporates some parameters that can be adjusted.

```haskell
-- Change the file imported as Structure for other Examples

import PowerlineStructure as Structure
```

Here, I am importing `PowerlineStructure.hs` file which contains the structure geometry relevant to a power-line tower.  There are a few other structures included, including `BridgeStructure.hs` and `SimpleStructure.hs`.  Simply change which file is imported as `Structure`.

### Mode of Vibration

Structures have more than one mode of vibration.  The first mode of vibration is generally not that interesting, but the higher mode of vibrations can look quite strange.  To view another mode of vibration alter the following parameter in `Constructor.hs`.

```haskell
-- The mode represents which natural frequency you want to animate

mode = 3 :: Int
```

There are no safety checks here, so you should choose a number => 1, and the mode of vibration should exist for the structure, for any complicated structure, up to the 10 would be appropriate.

### Creating Your Own Structures

This program does allow for the creation of your own structures.  There are some considerations to keep in mind when creating your structures.

There are 6 variables stored in a structure file that you must specify, `list_nodes`, `list_node_fixtures`, `list_edges`, `elastic_mod`, `rho` and `cross_sec_area`.  

`list_nodes` is of type `list_nodes :: [(Int, (Double, Double))]` is a list containing a nested pair of the form `(id, (x_position, y_position))`.  The `id` component serves as a unique identifier for that node, the first node must begin at `1` and following nodes must increase by increments of `1`.  The `x_position` and `y_position` are variables of type `Double` that indicate the position of the node.

`list_node_fixtures` is of type `list_node_fixtures :: [(Int, (Bool, Bool))]`.  This variable indicates which nodes are constrained in a way they are not allowed to move.  You can imagine this as which node is effectively acting as a mounting point of the structure to the ground.  This program allows you to constrain the x-direction of travel or the y-direction of travel independently.  A structure requires a minimum of one constraint in the x-direction, and one constraint in the y-direction.  Structures can have more than one constraint, but it is advised that someone creating their own structures be careful with imposing too many fixtures.  If a node is fixed, it is not free to vibrate.  Too many fixtures not only do not represent a structure well, but also over-constrain the numerical solution.

`list_edges` is of type `list_edges :: [(Int, (Int, Int))]`.  The nested pairs represent `(id, (first_node_id, second_node_id))`.  This list stores all the truss members, and identify what two nodes the truss member connects to.

`elastic_mod` represents the [Elastic Modulus](https://en.wikipedia.org/wiki/Elastic_modulus) of the material of the structure. The Elastic Modulus can be thought of as how resistant to deformation via tension or compression.  The higher the value, the more force it takes to deform the material.

`rho` is the density of the material.

`cross_sec_area` is the cross sectional area of the structural elements.

All the values I used assume metric units of meters (m) for x, y positioning, kg/m<sup>3</sup> for density, and N/m<sup>2</sup> for elastic modulus.  

## Dependencies

This package has two primary dependencies, `hmatrix` and `Gloss`.  `hmatrix` handles the computation of the eigenvalues and eigenvectors, and assists with the matrix composition.  `Gloss` is used to animate the resulting modes of vibration.
