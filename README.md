
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SURGE

<!-- badges: start -->

<!-- badges: end -->

The SURGE package streamlines importing of 3D landmark data from
Stratovan Checkpoint into R for analysis.

## Installation

You can install the development version of SURGE from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("rnfelice/SURGE")
```

## Basic Usage

You can import your .pts files from Checkpoint into a 3D array. You need
a list of .pts files and a csv file containing information about the
start and end of each semilandmark curve and how many points to retain
from each

``` r
#import table defining curves
curve_table <- read_csv("croc_curves.csv")
#identify the folder where your pts files are
ptsfolder <- "~/Crocs/pts"
#import the pts file names
ptslist <- dir(ptsfolder, pattern='.pts', recursive=F)
```

Then use the function create\_curve\_info to define your curves in a
Morpho-friendly format

``` r
my_curves <- create_curve_info(curve_table, n_fixed = 102)
```

And then use that curve info to import the data and make all the curves
have the same number of points

``` r
setwd(ptsfolder)
subsampled.lm <- import_chkpt_data(ptslist, my_curves, subsampl = TRUE)
```
