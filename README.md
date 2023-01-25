
<!-- README.md is generated from README.Rmd. Please edit that file -->

# corVis

<!-- badges: start -->
<!-- badges: end -->

The goal of corVis is to visualise association and conditional
association. The conventional correlation matrix displays are extended
to include every variable pair, in order to visualise all pairwise
associations in the dataset. Display with multiple association measures
is introduced for a comparison of the measures. A conditional
association display is also introduced to explore the difference in
association at different levels of conditioning variable.

## Installation

You can install the development version of corVis from
[GitHub](https://github.com/) with:

``` r
# still in progress
# install.packages("devtools")
# devtools::install_github("chinwan16/corVis")
```

## Example 1: Pairwise display for every variable pair in a dataset

This is an example of an association measures display for every variable
pair in the dataset. We will use `penguins` dataset from
`palmerpenguins` package in R.

``` r
library(corVis)
# install.packages("palmerpenguins")
library(palmerpenguins)

penguins_df <- palmerpenguins::penguins
names(penguins_df)
#> [1] "species"           "island"            "bill_length_mm"   
#> [4] "bill_depth_mm"     "flipper_length_mm" "body_mass_g"      
#> [7] "sex"               "year"
names(penguins_df) <- c("species", "island", "bill_l",
                 "bill_d", "flip_l", "mass",
                 "sex", "year")
penguins_assoc <- calc_assoc(penguins_df)
plot_assoc_matrix(penguins_assoc)
```

<img src="man/figures/README-example1-1.png" width="100%" />

## Example 2: Multiple measures display for comparison of association measures

This example shows a display of multiple association measures for all
the variable pairs. This display is useful in comparing the multiple
measures in order to reveal variable pairs with high difference among
the measures.

``` r
penguins_pearson <- tbl_cor(penguins_df)
penguin_distance <- tbl_dcor(penguins_df)
penguins_mic <- tbl_mine(penguins_df)
penguins_cancor <- tbl_cancor(penguins_df)
penguins_nmi <- tbl_nmi(penguins_df)

penguins_compare <- rbind(penguins_pearson, 
                          penguin_distance, 
                          penguins_mic, 
                          penguins_cancor,
                          penguins_nmi)
penguins_compare$measure_type <- factor(penguins_compare$measure_type,
                                        levels = c("cancor","nmi","pearson",
                                                   "mic","dcor"))

plot_assoc_linear(penguins_compare,
                  var_order = "max_diff",
                  limits = c(0,1))
```

<img src="man/figures/README-example2-1.png" width="100%" />

## Example 3: Conditional association display

This example shows a conditional association display at different levels
of a conditioning variable (`species` in this case). This display is
useful in for exploring variable pairs with high difference in the
measure value at different levels of a conditioning variable.

``` r
penguins_cond <- calc_assoc(penguins_df, by="species")
plot_assoc_matrix(penguins_cond)
```

<img src="man/figures/README-example3-1.png" width="100%" />
