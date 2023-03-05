# DDGRAM

## Overview

DDGRAM is a small command line interface (CLI) tool that provides a unified interface for the conversion of diagrams-as-text files. It also comes with a small template library to get started with different diagrams. In the future it would be nice to have simple unified style library as well. Finally, `ddgram` comes with a pandoc script with which you can embed digrams directly into your Markdown document. This is shown in this [template](https://github.com/MMesch/flakeTemplates/tree/main/pandocmax).

## Usage

This project relies heavily on the package manager [Nix](https://nixos.org) to solve the challenge of safely pulling together dependencies across ecosystem boundaries. You can run the CLI directly from the repo with:

```
nix run github:mmesch/ddgram -- <subcommands/flags>
```

Conversion is handled by the `ddgram convert` subcommand:

```
$ ddgram convert --help

convert diagrams to images

Usage: ddgram convert [-i|--informat INFORMAT] [-f|--outformat OUTFORMAT] INPATH
                      [-o|--outpath OUTPATH] [--extraOptions OptionsString]
  Convert diagram files to images

Available options:
  -i,--informat INFORMAT   The informat, specifying the program that is used for
                           conversion. One of: VegaLite, Vega, GraphViz,
                           Mermaid, Svgbob, Plantuml
  -f,--outformat OUTFORMAT The format of the output file. One of: SVG, PDF, PNG
  INPATH                   The file path of the input file
  -o,--outpath OUTPATH     The file path of the output file
  --extraOptions OptionsString
                           extraoptions that will be passed to the executable.
                           E.g. "-s" (default: [])
  -h,--help                Show this help text
```

## Design decisions

- rely on Nix only for dependency management
- support all combinations of input/output formats
- support all included flags on all commands
