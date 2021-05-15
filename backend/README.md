# Master Thesis Backend

## Introduction

The backend for the evolution plan three way merge. The program is implemented in the strongly typed, purely functional programming language Haskell.

The evolution plan merger backend defines a command line interface, `epmerge`, which will do three way merges on evolution plans. This means that you have a common base evolution plan, and two evolution plans which are derived from the common base, and the merger will merge the two derived versions. The base is used as a reference to know what changed in each version. The result of the merge is either an error, or a sound evolution plan, which consists of all the changes from both versions.

## Usage

### Compilation

There are a couple of ways to compile and run the project. Depending on your personal preference, you can choose either of the following list of steps. As an example, we will simply show the `help` page. Note that with the `exec` and `run` commands, we will have to write `--` before the flags to pass the flags correctly.

#### Installing on path

- `stack build` will compile the project and create the `epmerge` executable
- `stack install` will install the `epmerge` executable on the PATH
- `epmerge --help` will use the newly installed executable and show the help page

#### Without installing on path

- `stack build` to compile the project
- `stack exec epmerge -- --help` to execute the executable with the `--help` flag

#### All in one command

- `stack run -- --help` will compile and run epmerge executable with the `--help` flag.

### Development tools

- `stack repl` Opens up a repl with all the modules available
- `ghcid -c "stack repl"` Compiles project on save

## Example usage

- `epmerge --generateAll --generateElm` Performs a merge on all examples, and writes everything to the frontend
- `epmerge --generateOne "ConflictMultipleAdd" --print` Performs a merge on a predefined, conflicting example, and prints the result
- `epmerge --fromFile="./data/sound_flatmodification.json" --fromType FlatModification --print --toFile "./output_example.json"` Reads an examle from file, with the file containing an example in the FlatModification evolution plan type. The result of the merge is printed and written to the `output_example.json` file

## Explanation

### Help page

Running the merger with `--help` flag will yield the following user guide

```
A three way merge tool for feature model evolution plans

Usage: epmerge (--generateOne EXAMPLENAME | --generateAll | --fromFile FILENAME)
               [-F|--fromType FROMTYPE] [-T|--toType TOTYPE] [-p|--print]
               [-g|--generateElm] [-o|--toFile FILEPATH]
  Merges evolution plans into a single merged plan, which respects the formal
  semantics of evolution plans

Available options:
  --generateOne EXAMPLENAME
                           Generates one of the examples
  --generateAll            Generates all examples
  --fromFile FILENAME      Read a three way merge plan from file
  -F,--fromType FROMTYPE   The type to convert from (useful only when reading
                           from file)(choices: TreeUser | FlatUser |
                           FlatModification) (default: TreeUser)
  -T,--toType TOTYPE       The type to convert to (useful when printing and
                           writing to file)(choices: TreeUser | FlatUser |
                           FlatModification) (default: FlatModification)
  -p,--print               Whether to print the merge result
  -g,--generateElm         Whether to pass generated results to the elm frontend
  -o,--toFile FILEPATH     Outputed file to write the merge result as JSON
  -h,--help                Show this help text
```

### Modes

The command line interface has three basic `Modes`. `GenerateAll`, `GenerateOne` and `FromFile`.

`GenerateAll` will simply run all the examples in the code. This includes some sound examples and some erroneous examples. The erroneous examples consists of `Merge` conflicts, `Local` conflicts and `Global` conflicts. This mode can be run using `epmerge --generateAll`.

`GenerateOne` takes one argument, `Example Name`, which is the string associated with one of the examples in the code. The merger will run the merger on the specified example. This mode can be run using `epmerge --generateOne EXAMPLENAME`. If you provide an `EXAMPLENAME` that does not exist, the merger will give you all the available example names.

`FromFile` also takes an argument, `File Name`, which is the name of json file to read from. The merger will read the file, and run the merger on the input. This mode can be run using `epmerge --fromFile FILEPATH`. To see some different examples, see the `./data` folder, which includes all three different types of evolution plans.

### Flags

In order to produce some useful information, the user has to set some flags. Full specification is listed in the `help` page, but some common examples are as follows.

---

When using the `FromFile` mode, you will have to specify what evolution plan representation you are using. this can be done with the `--fromType` flag, which takes either `TreeUser`, `FlatUser` or `FlatModification` as a parameter. As an example, running the following will read a sound example from file, merge the input, and print the result.

`epmerge --fromFile="./data/sound_flatuser.json" --fromType=FlatUser --print`

---

In able to view the results of the merge, we could do one or more of the following.

- Print the result of the merge using the `--print` flag
- Write the result to a json file using the `--toFile FILEPATH` flag.
- Write the example(s) to the Elm frontend using the `--generateElm` flag. This will write the result to a file in `../frontend/data`. The next time the frontend is loaded, the user can see an actual visual, tree representation of the _base_, _version 1_, _version 2_ evolution plans, as well as the expected and actual merge output.

To specify the output format of the print or the file to write the output, you can use the `--toType`, with either `TreeUser`, `FlatUser` or `FlatModification` as an argument.
