## Overview

A general purpose library.

## Installation

`devtools::install_github("gfleetwood/sansor")`

## CLI Cricket

This is a cli game masquerading as a function.

Growing up in the West Indies we played a bootleg version of cricket using a pencil. 
You marked the middle of each side: 0, 1, 2, 4, 6, W, and bowled by rolling the pencil and 
stopping it with a finger. Whatever was written on the upturned side determined the result - no 
runs, some number of runs, or a wicket.

This implementation mirrors this game on the command line. Run using:

`Rscript -e "library(sansor); sansor::cli_cricket()"`

Press any button to bowl. The defaults are set for five overs, four wickets, and the user bowling both innings (one per team). The computer randomly picks a bowling outcome.
