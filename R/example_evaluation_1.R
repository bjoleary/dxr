# Do not hand edit this file. Edit data-raw/example_evaluation_1.R instead.
#' Example Evaluation 1
#'
#' A made up example evaluation.
#'
#' @format A list of tibbles.
#'
#' \describe{
#' \item{evaluation_metadata}{: tibble 11 × 2 (S3: tbl_df/tbl/data.frame)}
#' \item{name}{: chr 1:11 "evaluation_name" "evaluation_description"
#' "developer" "assay" ...}
#' \item{value}{:List of 11}
#' \item{sample_blinding}{: tibble 110 × 2 (S3: tbl_df/tbl/data.frame)}
#' \item{evaluation_sample_id}{: chr 1:110 "C0001" "C0002" "C0004" "C0005" ...}
#' \item{panel_sample_id}{: chr 1:110 "C0001" "C0002" "C0004" "C0005" ...}
#' \item{evaluation_table}{: tibble 220 × 7 (S3: tbl_df/tbl/data.frame)}
#' \item{sample}{: chr 1:220 "C0001" "C0002" "C0004" "C0005" ...}
#' \item{analyte}{: chr 1:220 "IgM" "IgM" "IgM" "IgM" ...}
#' \item{target}{: chr 1:220 "Spike" "Spike" "Spike" "Spike" ...}
#' \item{lot_number}{: chr 1:220 "E200330DT" "E200330DT" "E200330DT"
#' "E200330DT" ...}
#' \item{datetime_observation}{: Date1:220, format: "2020-04-21"
#' "2020-04-21" ...}
#' \item{qualitative_result}{: chr 1:220 "Negative" "Negative" "Negative"
#' "Negative" ...}
#' \item{notes_and_anomalies}{: chr 1:220 NA NA NA NA ...}
#' }
#'
#' @source
#' Contrived based on the data from
#' [maf3246-a001.csv](https://www.accessdata.fda.gov/cdrh_docs/presentations/maf/maf3246-a001.csv)
#' accessed 2021-10-17.
#'
"example_evaluation_1"
