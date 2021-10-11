
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dxr: Diagnostic Product Evaluation in R

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/dxr)](https://CRAN.R-project.org/package=dxr)
[![R-CMD-check](https://github.com/bjoleary/dxr/workflows/R-CMD-check/badge.svg)](https://github.com/bjoleary/dxr/actions?query=workflow%3AR-CMD-check)
[![lint](https://github.com/bjoleary/dxr/workflows/lint/badge.svg)](https://github.com/bjoleary/dxr/actions?query=workflow%3Alint)
<!-- badges: end -->

This package provides functions to support evaluations of diagnostic
products using R.

## Installation

You can install the development version of dxr from
[GitHub](https://github.com/bjoleary/dxr) with:

``` r
# install.packages("devtools")
devtools::install_github("bjoleary/dxr")
```

## Example

This library includes many basic statistics functions that are
consistent with CLSI standards for confidence interval estimation using
Wilson Scores. Results are output in a convenient list format.

``` r
library(dxr)
sensitivity(
  true_positives = 29,
  false_negatives = 1,
  digits = 0.1, 
  interval = 0.95
  )
#> $estimate
#> [1] 0.9666667
#> 
#> $estimate_percent
#> [1] "96.7%"
#> 
#> $ratio
#> [1] "29/30"
#> 
#> $lower
#> [1] 0.8332961
#> 
#> $lower_percent
#> [1] "83.3%"
#> 
#> $upper
#> [1] 0.9940914
#> 
#> $upper_percent
#> [1] "99.4%"
#> 
#> $confidence_interval
#> [1] "(95% CI: 83.3%; 99.4%)"
#> 
#> $string_two_line
#> [1] "96.7% (29/30) \n(95% CI: 83.3%; 99.4%)"
#> 
#> $string
#> [1] "29/30 = 96.7% (95% CI: 83.3%; 99.4%)"
```

## Build a panel of samples

Create an excel data entry sheet for information about a
well-characterized sample panel to be used to evaluate diagnostic
assays.

``` r
sample_panel <-
  build_panel_sheet(
    panel_name = "Example Panel",
    panel_description = NA_character_,
    n_samples = 110L,
    sample_groups = c("Positive", "Negative"),
    sample_matrices = c("Serum", "Plasma"),
    analytes = c("IgM", "IgG", "Pan-Ig"),
    targets = "Spike",
    qualitative_outcomes = c("Positive", "Negative"),
    qualitative_comparators =
      c(
        "Comparator Serology Assay with PCR confirmed infection",
        "Collected Pre-2020"
      ),
    semiquantitative_outcomes = c("0", "100", "400", "1600", "6400"),
    semiquantitative_comparators = "Comparator Serology Assay",
    quantitative_units = NA_character_,
    quantitative_comparators = NA_character_
  )

# To save it: 
# write_panel_sheet(
#   panel_sheet_data = sample_panel,
#   filepath = "sample_panel.xlsx"
# )

print(sample_panel$panel_metadata)
#> # A tibble: 13 × 2
#>    name                         value    
#>    <chr>                        <list>   
#>  1 panel_name                   <chr [1]>
#>  2 panel_description            <chr [1]>
#>  3 n_samples                    <int [1]>
#>  4 sample_groups                <chr [2]>
#>  5 sample_matrices              <chr [2]>
#>  6 analytes                     <chr [3]>
#>  7 targets                      <chr [1]>
#>  8 qualitative_outcomes         <chr [2]>
#>  9 qualitative_comparators      <chr [2]>
#> 10 semiquantitative_outcomes    <chr [5]>
#> 11 semiquantitative_comparators <chr [1]>
#> 12 quantitative_units           <chr [1]>
#> 13 quantitative_comparators     <chr [1]>
# This will be written to excel to facilitate data entry:
print(sample_panel$panel_table)
#> # A tibble: 330 × 9
#>    sample            analyte target group matrix qualitative_res… qualitative_com…
#>    <chr>             <chr>   <chr>  <chr> <chr>  <chr>            <chr>           
#>  1 example_panel_001 IgM     Spike  <NA>  <NA>   <NA>             <NA>            
#>  2 example_panel_001 IgG     Spike  <NA>  <NA>   <NA>             <NA>            
#>  3 example_panel_001 Pan-Ig  Spike  <NA>  <NA>   <NA>             <NA>            
#>  4 example_panel_002 IgM     Spike  <NA>  <NA>   <NA>             <NA>            
#>  5 example_panel_002 IgG     Spike  <NA>  <NA>   <NA>             <NA>            
#>  6 example_panel_002 Pan-Ig  Spike  <NA>  <NA>   <NA>             <NA>            
#>  7 example_panel_003 IgM     Spike  <NA>  <NA>   <NA>             <NA>            
#>  8 example_panel_003 IgG     Spike  <NA>  <NA>   <NA>             <NA>            
#>  9 example_panel_003 Pan-Ig  Spike  <NA>  <NA>   <NA>             <NA>            
#> 10 example_panel_004 IgM     Spike  <NA>  <NA>   <NA>             <NA>            
#> # … with 320 more rows, and 2 more variables: semiquantitative_result <chr>,
#> #   semiquantitative_comparator <chr>
```

## Set up a data entry sheet for an evaluation

Evaluations should be associated with an existing panel. The code that
sets up an evaluation sheet will perform some sanity checks to make sure
the evaluation is compatible with the panel.

``` r
evaluation_one <- 
  build_evaluation_sheet(
    evaluation_name = "Example Evaluation",
    evaluation_description = NA_character_,
    developer = "ACME Test Corp.",
    assay = "Test Assay #1",
    lot_numbers = "20200101",
    panel_data = sample_panel, # See previous code chunk
    analytes = c("IgM", "IgG", "Pan-Ig"),
    targets = "Spike",
    # We'll include an additional qualitative outcome. The function will 
    # identify the inconsistency with the panel. This will generate a warning, 
    # but the evaluation can still proceed in this case. Equivocal results 
    # will just be called as false positives or false negatives since 
    # "Equivocal" is not a valid outcome in the sample_panel generated above.
    qualitative_outcomes = c("Positive", "Equivocal", "Negative"),
    semiquantitative_outcomes = NA_character_,
    quantitative_units = NA_character_,
    randomize = FALSE,
    blind = FALSE
  )
#> Warning in build_evaluation_sheet(evaluation_name = "Example Evaluation", :
#> At least one of the qualitative outcomes you identified for this evaluation
#> (Positive, Equivocal, Negative) is not a valid qualitative outcome for the
#> panel: "Example Panel". Valid qualitative outcomes for this panel include:
#> Positive, Negative. The evaluation outcome(s) that do not match those that are
#> valid for this panel (Equivocal) will be called as false results.

# To save it: 
# write_evaluation_sheet(
#   evaluation_sheet_data = evaluation_one,
#   filepath = "evaluation_one.xlsx"
# )

# Evaluation metadata:
print(evaluation_one$evaluation_metadata)
#> # A tibble: 11 × 2
#>    name                      value    
#>    <chr>                     <list>   
#>  1 evaluation_name           <chr [1]>
#>  2 evaluation_description    <chr [1]>
#>  3 developer                 <chr [1]>
#>  4 assay                     <chr [1]>
#>  5 lot_numbers               <chr [1]>
#>  6 analytes                  <chr [3]>
#>  7 targets                   <chr [1]>
#>  8 qualitative_outcomes      <chr [3]>
#>  9 semiquantitative_outcomes <chr [1]>
#> 10 quantitative_units        <chr [1]>
#> 11 blinded                   <lgl [1]>
# This one is not randomized or blinded, so the IDs match:
print(evaluation_one$sample_blinding)
#> # A tibble: 110 × 2
#>    evaluation_sample_id panel_sample_id  
#>    <chr>                <chr>            
#>  1 example_panel_001    example_panel_001
#>  2 example_panel_002    example_panel_002
#>  3 example_panel_003    example_panel_003
#>  4 example_panel_004    example_panel_004
#>  5 example_panel_005    example_panel_005
#>  6 example_panel_006    example_panel_006
#>  7 example_panel_007    example_panel_007
#>  8 example_panel_008    example_panel_008
#>  9 example_panel_009    example_panel_009
#> 10 example_panel_010    example_panel_010
#> # … with 100 more rows
# This will be written to excel to facilitate data entry:
print(evaluation_one$evaluation_table)
#> # A tibble: 330 × 7
#>    sample            analyte target lot_number datetime_observati… qualitative_res…
#>    <chr>             <chr>   <chr>  <chr>      <dttm>              <chr>           
#>  1 example_panel_001 IgM     Spike  20200101   NA                  <NA>            
#>  2 example_panel_001 IgG     Spike  20200101   NA                  <NA>            
#>  3 example_panel_001 Pan-Ig  Spike  20200101   NA                  <NA>            
#>  4 example_panel_002 IgM     Spike  20200101   NA                  <NA>            
#>  5 example_panel_002 IgG     Spike  20200101   NA                  <NA>            
#>  6 example_panel_002 Pan-Ig  Spike  20200101   NA                  <NA>            
#>  7 example_panel_003 IgM     Spike  20200101   NA                  <NA>            
#>  8 example_panel_003 IgG     Spike  20200101   NA                  <NA>            
#>  9 example_panel_003 Pan-Ig  Spike  20200101   NA                  <NA>            
#> 10 example_panel_004 IgM     Spike  20200101   NA                  <NA>            
#> # … with 320 more rows, and 1 more variable: notes_and_anomalies <chr>
```
