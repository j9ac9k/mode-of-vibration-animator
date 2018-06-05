# Truss Structure Natural Frequency Vibration Viewer

The Mode of Vibration Animator takes in a file containing structure geometry, calculates the natural frequencies, and using Gloss, animates them. This project is largely a learning exercise for me to learn Haskell, and my submission for CS 557 Functional Languages at PSU.


# Installation

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
stack exec vibration-animator
```

MacOS 

```bash
# need homebrew installed prior to
brew install haskell-stack

git clone git@github.com:j9ac9k/mode-of-vibration-animator.git
cd mode-of-vibration-animator
stack build
stack exec vibration-animator
```
