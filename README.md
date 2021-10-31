
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dxr: Diagnostic Product Evaluation in R

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/dxr)](https://CRAN.R-project.org/package=dxr)
[![R-CMD-check](https://github.com/bjoleary/dxr/workflows/R-CMD-check/badge.svg)](https://github.com/bjoleary/dxr/actions?query=workflow%3AR-CMD-check)
[![lint](https://github.com/bjoleary/dxr/workflows/lint/badge.svg)](https://github.com/bjoleary/dxr/actions?query=workflow%3Alint)
[![Codecov test
coverage](https://codecov.io/gh/bjoleary/dxr/branch/main/graph/badge.svg)](https://codecov.io/gh/bjoleary/dxr?branch=main)
<!-- badges: end -->

This package provides functions to support evaluations of diagnostic
products using R.

It’s under development and should not be used “in production.” Feedback
is welcome.

## Installation

You can install the development version of dxr from
[GitHub](https://github.com/bjoleary/dxr) with:

``` r
# install.packages("devtools")
devtools::install_github("bjoleary/dxr")
```

## Example

This library includes many basic statistics functions You can find
references to the sources of the methods used in the documentation for
each function. Generally, they are based on [Statistics with
Confidence](https://www.wiley.com/en-us/Statistics+with+Confidence%3A+Confidence+Intervals+and+Statistical+Guidelines%2C+2nd+Edition-p-9780727913753),
edited by Altman, Machin, Bryant, and Gardner. References are to the
Kindle edition of that book.

Results are output in a list format for convenience. There are probably
better versions of these functions available in `stats::` and elsewhere
if they are all you are looking for.

``` r
library(dxr)
sensitivity(
  true_positives = 29,
  false_negatives = 1,
  digits = 0.1, # Round to here
  interval = 0.95 # Give a 95% confidence interval
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

ppv_with_confidence(
  sensitivity = 
    sensitivity_fraction(
      numerator = 29,
      denominator = 30,
      digits = 0.1, # This only affects rounding for the formatted output.
                    # The PPV function will use the full-precision estimate. 
      interval = 0.95 # Give a 95% confidence interval
    ),
  specificity_fraction(
    numerator = 79,
    denominator = 80,
    digits = 0.1,
    interval = 0.95 # Give a 95% confidence interval
  ),
  prevalence = 0.2, # 20%,
  interval = 0.95 # Give a 95% confidence interval
)
#> $estimate
#> [1] 0.9508197
#> 
#> $estimate_percent
#> [1] "95.1%"
#> 
#> $lower
#> [1] 0.7553809
#> 
#> $lower_percent
#> [1] "75.5%"
#> 
#> $upper
#> [1] 0.9911859
#> 
#> $upper_percent
#> [1] "99.1%"
#> 
#> $confidence_interval
#> [1] "(95% CI: 75.5%; 99.1%)"
#> 
#> $string_two_line
#> [1] "95.1%\n(95% CI: 75.5%; 99.1%)"
#> 
#> $string
#> [1] "95.1% (95% CI: 75.5%; 99.1%)"
```

## Build a panel of samples

The first step in the types of experiments this is intended for is to
build a panel of well-characterized samples that will be used to
evaluate all of the assays under consideration. This package provides
`build_panel_sheet()` and `write_panel_sheet()` to generate an excel
template where you can record ground truth for those samples, as well as
additional information.

At the moment, it really expects the qualitative truth to be “Positive”
or “Negative”. There should not be “Equivocal” or similar outcomes for
samples included in the panel.

It provides some facility for recording semi-quantitative (e.g. titers)
and quantitative truths for the sample panel, but there is no scoring or
assessment based on this at this time. The current focus is on
assessment of qualitative performance.

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

sample_panel$panel_metadata %>% 
  tidyr::pivot_wider() %>%
  purrr::map(unlist) %>%
  as.list()
#> $panel_name
#> [1] "Example Panel"
#> 
#> $panel_description
#> [1] NA
#> 
#> $n_samples
#> [1] 110
#> 
#> $sample_groups
#> [1] "Positive" "Negative"
#> 
#> $sample_matrices
#> [1] "Serum"  "Plasma"
#> 
#> $analytes
#> [1] "IgM"    "IgG"    "Pan-Ig"
#> 
#> $targets
#> [1] "Spike"
#> 
#> $qualitative_outcomes
#> [1] "Positive" "Negative"
#> 
#> $qualitative_comparators
#> [1] "Comparator Serology Assay with PCR confirmed infection"
#> [2] "Collected Pre-2020"                                    
#> 
#> $semiquantitative_outcomes
#> [1] "0"    "100"  "400"  "1600" "6400"
#> 
#> $semiquantitative_comparators
#> [1] "Comparator Serology Assay"
#> 
#> $quantitative_units
#> [1] NA
#> 
#> $quantitative_comparators
#> [1] NA

# This will be written to excel to facilitate data entry:
sample_panel$panel_table
#> # A tibble: 330 × 9
#>    sample       analyte target group matrix qualitative_tru… qualitative_compar…
#>    <chr>        <chr>   <chr>  <chr> <chr>  <chr>            <chr>              
#>  1 example_pan… IgM     Spike  <NA>  <NA>   <NA>             <NA>               
#>  2 example_pan… IgG     Spike  <NA>  <NA>   <NA>             <NA>               
#>  3 example_pan… Pan-Ig  Spike  <NA>  <NA>   <NA>             <NA>               
#>  4 example_pan… IgM     Spike  <NA>  <NA>   <NA>             <NA>               
#>  5 example_pan… IgG     Spike  <NA>  <NA>   <NA>             <NA>               
#>  6 example_pan… Pan-Ig  Spike  <NA>  <NA>   <NA>             <NA>               
#>  7 example_pan… IgM     Spike  <NA>  <NA>   <NA>             <NA>               
#>  8 example_pan… IgG     Spike  <NA>  <NA>   <NA>             <NA>               
#>  9 example_pan… Pan-Ig  Spike  <NA>  <NA>   <NA>             <NA>               
#> 10 example_pan… IgM     Spike  <NA>  <NA>   <NA>             <NA>               
#> # … with 320 more rows, and 2 more variables: semiquantitative_truth <chr>,
#> #   semiquantitative_comparator <chr>
```

Excel’s input validation features are used as data is entered. The valid
values are stored on a hidden sheet, `input_validation`, which should
have information equivalent to that in the metadata sheet.

## Set up a data entry sheet for an evaluation

Once you have your panel set up, it’s time to evaluate your first assay.

Evaluations should be associated with an existing panel. The code that
sets up an evaluation sheet will perform some sanity checks to make sure
the evaluation is compatible with the panel.

Two functions, `build_evaluation_sheet()` and `write_evaluation_sheet()`
work similarly to the functions for building and writing panels above
and create a template excel file you can use to record the data from
your first assay evaluation.

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
evaluation_one$evaluation_metadata %>% 
    tidyr::pivot_wider() %>%
    purrr::map(unlist) %>%
    as.list()
#> $evaluation_name
#> [1] "Example Evaluation"
#> 
#> $evaluation_description
#> [1] NA
#> 
#> $developer
#> [1] "ACME Test Corp."
#> 
#> $assay
#> [1] "Test Assay #1"
#> 
#> $lot_numbers
#> [1] "20200101"
#> 
#> $analytes
#> [1] "IgM"    "IgG"    "Pan-Ig"
#> 
#> $targets
#> [1] "Spike"
#> 
#> $qualitative_outcomes
#> [1] "Positive"  "Equivocal" "Negative" 
#> 
#> $semiquantitative_outcomes
#> [1] NA
#> 
#> $quantitative_units
#> [1] NA
#> 
#> $blinded
#> [1] FALSE

# This one is not randomized or blinded, so the IDs match:
evaluation_one$sample_blinding
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
evaluation_one$evaluation_table
#> # A tibble: 330 × 7
#>    sample        analyte target lot_number datetime_observati… qualitative_resu…
#>    <chr>         <chr>   <chr>  <chr>      <dttm>              <chr>            
#>  1 example_pane… IgM     Spike  20200101   NA                  <NA>             
#>  2 example_pane… IgG     Spike  20200101   NA                  <NA>             
#>  3 example_pane… Pan-Ig  Spike  20200101   NA                  <NA>             
#>  4 example_pane… IgM     Spike  20200101   NA                  <NA>             
#>  5 example_pane… IgG     Spike  20200101   NA                  <NA>             
#>  6 example_pane… Pan-Ig  Spike  20200101   NA                  <NA>             
#>  7 example_pane… IgM     Spike  20200101   NA                  <NA>             
#>  8 example_pane… IgG     Spike  20200101   NA                  <NA>             
#>  9 example_pane… Pan-Ig  Spike  20200101   NA                  <NA>             
#> 10 example_pane… IgM     Spike  20200101   NA                  <NA>             
#> # … with 320 more rows, and 1 more variable: notes_and_anomalies <chr>
```

If you set `randomize` and/or `blind` to `TRUE`, the software will
scramble the order of the samples in the panel and/or rename them. Be
warned: it’s a pretty weak blind at the moment. The link back to the
panel sample IDs is stored in a “hidden” excel worksheet in the
evaluation workbook without any significant protection. Simply right
click any of the worksheet tabs and select “unhide” to see it. This is
on my list of things to improve…

Excel’s input validation features are used as data is entered. The valid
values are stored on a hidden sheet, `input_validation`, which should
have information equivalent to that in the metadata sheet.

## Read in data

Once you’ve conducted an evaluation, you can read in the data you’ve
entered in the template excel workbooks using `read_panel()` and
`read_evaluation()`.

## Score the evaluation

Once you’ve read in the data from the panel and the evaluation, you can
score the evaluation (qualitatively, future work needed for
semi-quantitative and quantitative) using `score_evaluation()`. This
generates the un-blinded line data table.

``` r
score_evaluation(
  panel_data = dxr::example_panel_1,
  evaluation_data = dxr::example_evaluation_1
) %>% print()
#> # A tibble: 220 × 17
#>    panel_sample_id evaluation_sample_id lot_number matrix group     analyte
#>    <chr>           <chr>                <chr>      <chr>  <chr>     <chr>  
#>  1 C0001           C0001                E200330DT  Serum  Negatives IgM    
#>  2 C0001           C0001                E200330DT  Serum  Negatives IgG    
#>  3 C0002           C0002                E200330DT  Serum  Negatives IgM    
#>  4 C0002           C0002                E200330DT  Serum  Negatives IgG    
#>  5 C0004           C0004                E200330DT  Plasma Negatives IgM    
#>  6 C0004           C0004                E200330DT  Plasma Negatives IgG    
#>  7 C0005           C0005                E200330DT  Plasma Negatives IgM    
#>  8 C0005           C0005                E200330DT  Plasma Negatives IgG    
#>  9 C0008           C0008                E200330DT  Plasma Negatives IgM    
#> 10 C0008           C0008                E200330DT  Plasma Negatives IgG    
#> # … with 210 more rows, and 11 more variables: evaluation_target <chr>,
#> #   datetime_observation <date>, qualitative_result <chr>,
#> #   qualitative_truth <chr>, qualitative_match <lgl>,
#> #   qualitative_outcome_strict <chr>, semiquantitative_truth <chr>,
#> #   notes_and_anomalies <chr>, comparator_target <chr>,
#> #   qualitative_comparator <chr>, semiquantitative_comparator <chr>
```

You can also generate a standard “two-by-two” style table to understand
how the evaluation outcomes compared to the truth established in the
panel.

``` r
two_by_two(
  panel_data = dxr::example_panel_1,
  evaluation_data = dxr::example_evaluation_1
) %>% 
  dplyr::mutate_all(
    .funs = ~ tidyr::replace_na(., replace = "")
  ) %>% 
  knitr::kable() 
```

| SARS-COV-2 ELISA (IgG)         | IgM+, IgG+ | IgM-, IgG+ | IgM+, IgG- | IgM-, IgG- | Total |
|:-------------------------------|:-----------|:-----------|:-----------|:-----------|:------|
| IgM+, IgG+                     | 27         |            |            |            | 27    |
| IgM Borderline, IgG+           |            |            |            |            | 0     |
| IgM-, IgG+                     |            |            |            |            | 0     |
| IgM+, IgG Borderline           |            |            |            |            | 0     |
| IgM Borderline, IgG Borderline | 2          |            |            |            | 2     |
| IgM-, IgG Borderline           |            |            |            |            | 0     |
| IgM+, IgG-                     |            |            |            |            | 0     |
| IgM Borderline, IgG-           |            |            |            |            | 0     |
| IgM-, IgG-                     | 1          |            |            | 80         | 81    |
| Total                          | 30         | 0          | 0          | 80         | 110   |

And, finally, you can calculate summary statistics using
`calculate_performance()`. This will return a list of three tables: one
for PPA (sensitivity), one for NPA (specificity), and a summary table
with better formatting. There are still some enhancements to make here,
such as adding prevalence estimates to PPV and NPV can be calculated and
better configuring the sort order of the analytes.

``` r
calculate_performance(dxr::example_panel_1, dxr::example_evaluation_1) %>% 
  magrittr::extract2("summary") %>% 
  dplyr::rename_all(snakecase::to_title_case) %>% 
  knitr::kable() 
```

| Performance Measure | Estimate                             |
|:--------------------|:-------------------------------------|
| IgG PPA             | 27/30 = 90.0% (95% CI: 74.4%; 96.5%) |
| IgG NPA             | 80/80 = 100% (95% CI: 95.4%; 100%)   |
| IgM PPA             | 27/30 = 90.0% (95% CI: 74.4%; 96.5%) |
| IgM NPA             | 80/80 = 100% (95% CI: 95.4%; 100%)   |

## Roadmap and other notes

There is much to do:

-   Improving the approach to blinding

-   Checking the data-entry for obvious errors

-   Enabling double-entry and checking for consistency between entries

-   Providing template reports

-   Making a nice R/Shiny frontend (I think I will call it “Dexter”.
    This package sounds like “dexer” in my head already…)

This and more to come. As my weekends permit.

**Hey–that reminds me**: This is a hobby project and is built on my own
time. It is not from my employer and it is *definitely* not endorsed by
my employer. For that matter, it’s not even endorsed by me. (Also, see
the license for some important disclaimers.) The government ethics
experts I checked in with told me that open sourcing a hobby project
isn’t a problem though, so here we go.

They also said it was important that it is not intended for therapeutic
use: So, don’t try to use this to make any health decisions or anything,
okay? I’m not sure how you could, but I’m pretty sure anything you could
come up with along those lines would be a pretty bad idea…

This is currently in the “experimental” stage. I don’t mean this as
“it’s fit for use in experiments.” It’s not: It’s under development, it
has significant testing and verification gaps, and it has not been
validated.

I got the itch to build this after doing [some
work](https://open.fda.gov/apis/device/covid19serology/) to evaluate the
performance of serology assays. I incorporated some of the public data
from that for tests and examples here, but this software is independent
from that effort, and anything you see here that looks similar to that
is not meaningful. What’s the *Law and Order* disclaimer? “Although
inspired in part by a true incident, the following story is fictional
and does not depict any actual person or event.” Think of the examples
you see here like that.

Anyway, in the course of that effort, I learned a lot from some really
smart people, and I was able to automate some tasks that otherwise would
have had to be done manually. You know how it is though: You build
something and *then* you see all the ways you could have designed it
differently or more flexibly. I’m starting from scratch here and have
tried to make things a bit more general-purpose and configurable for a
wider variety of diagnostic assay evaluations.

I’ll be happy if this never does more than solidify some of what I
learned from that effort. And, of course, I would like to take it a bit
further than that if I can. Someday, I hope to get this to a point where
you might be able to validate it and use it to improve the reliability
of some of the analyses you do in your lab. Go ahead and give it a test
run, be sure to let me know how it can be improved, and thanks for your
interest! :-)
