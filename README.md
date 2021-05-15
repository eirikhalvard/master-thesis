# Three-Way Semantic Merge for Feature Model Evolution Plans

A master thesis by Eirik Halvard Sæther

## Overview

This repository contains source code relevant to my master thesis. This includes the following:

- [The thesis (PDF)](./thesis/src/thesis.pdf)
- [A three-way merge tool written in Haskell](./backend)
- [A visualization tool written in Elm for exploring different merge scenarios](./frontend)

## Installation and Usage of the Tools

### Prerequisites

Note that several tools are necessary in order to get this project working. This includes at least the following:

- The Haskell language as well as additional build and development tools such as stack and cabal. Easiest solution is to install [Haskell with batteries included](https://www.haskell.org/platform/), which installs all necessary tools.
- (Optional) If you want to use the visualization tool as well, an installation of [Elm](https://guide.elm-lang.org/install/elm.html) and [elm-live](https://www.elm-live.com/) are necessary.

Note that the tools are tested on MacOS, and other operating systems might require additional tools to get things to work.

### Using the Tools

The main contribution is [the three-way merge tool implemented as a command-line interface in Haskell](./backend). This tool can be used independently of the [visualization tool](./frontend). However, we have included a simple script that showcases both tools.

#### The `./run` Script

Running `./run` will compile and run the haskell backend, which will generate the json representation of all the predefined merge examples and the results of their three-way merge. Then an elm server will be started at `localhost:8000`, which will display the result of the merge
