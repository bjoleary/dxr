## code to prepare `example_evaluation_1` dataset goes here

# Download and read the data ---------------------------------------------------
library(magrittr)
url_root <- "https://www.accessdata.fda.gov/cdrh_docs/presentations/maf/"
maf <-  "maf3246-a001.csv"
# A Panel 1 evaluation:
ex_eval <-
  readr::read_csv(
    file = paste0(url_root, maf),
    col_types =
      readr::cols(
        date_performed = readr::col_date(format = ""),
        days_from_symptom = readr::col_double(),
        .default = readr::col_character()
      )
  )

# Build the evaluation sheet ---------------------------------------------------
suppressWarnings(
  example_evaluation_1 <-
    build_evaluation_sheet(
      evaluation_name = "Test of NCI Panel 1 Evaluation",
      evaluation_description = "Nothing",
      developer = "ACME Assay Development, Inc.",
      assay = ex_eval$device %>% unique(),
      lot_numbers = ex_eval$lot_number %>% unique(),
      panel_data = dxr::example_panel_1,
      panel_data_filepath = NA_character_,
      # We'll fudge IgM by repeating the IgG results
      analytes = c("IgM", "IgG"),
      targets = "Spike",
      # Suppressing warnings because we have different outcomes than the panel
      qualitative_outcomes = c("Positive", "Borderline", "Negative"),
      semiquantitative_outcomes = NA_character_,
      quantitative_units = NA_character_,
      randomize = FALSE,
      blind = FALSE
    )
)

example_evaluation_1$evaluation_table$sample <-
  c(ex_eval$sample_id, ex_eval$sample_id)
example_evaluation_1$evaluation_table$datetime_observation <-
  c(ex_eval$date_performed, ex_eval$date_performed)
example_evaluation_1$evaluation_table$qualitative_result <-
  c(ex_eval$igg_result, ex_eval$igg_result)
example_evaluation_1$evaluation_table$analyte <-
  c(rep("IgM", 110), rep("IgG", 110))

usethis::use_data(example_evaluation_1, overwrite = TRUE)

# Document the dataset ---------------------------------------------------------

documentation_text <-
  c(
    "Example Evaluation 1",
    "",
    "A made up example evaluation. ",
    "",
    "@format A list of tibbles.",
    "",
    "\\describe{",
    dplyr::glimpse(example_evaluation_1, width = 76) %>%
      utils::capture.output(type = c("output")) %>%
      magrittr::extract(-c(1)) %>%
      stringr::str_replace(
        string = .,
        pattern = "(^\\s{0,2}\\$\\s\\w*\\s*)", # the column name
        replacement =
          paste0(
            "  \\\\item{",
            stringr::str_extract(
              string = .,
              pattern = "(?<=^\\s{0,2}\\$\\s)\\b\\w*\\b"
            ),
            "}{"
          )
      ) %>%
      stringr::str_replace(
        string = .,
        pattern = "(^\\s{0,2}\\.{2}\\$\\s\\w*\\s*)", # the column name
        replacement =
          paste0(
            "  \\\\item{",
            stringr::str_extract(
              string = .,
              pattern = "(?<=^\\s{0,2}\\.{2}\\$\\s)\\b\\w*\\b"
            ),
            "}{"
          )
      ) %>%
      paste0(., "}") %>%
      # Square brackets are a link in Roxygen. Replace:
      stringr::str_remove_all(
        string = .,
        pattern = "\\[|\\]|\\<|\\>"
      ) %>%
      # Remove formatting strings
      stringr::str_remove_all(
        string = .,
        pattern = stringr::fixed("\0333m\03338;5;246m")
      ) %>%
      stringr::str_remove_all(
        string = .,
        pattern = stringr::fixed("\03339m\03323m")
      ) %>%
      stringr::str_wrap(
        string = .,
        width = 76
      ) %>%
      stringr::str_split(pattern = "\\n") %>%
      unlist() ,
    "}",
    "",
    "@source ",
    "Contrived based on the data from ",
    paste0("[", maf, "](", url_root, maf, ")"),
    paste0("accessed ", lubridate::today(), ".")
  ) %>%
  paste0("#' ", .) %>%
  c(
    paste0(
      "# Do not hand edit this file. Edit data-raw/example_evaluation_1.R ",
      "instead."
    ),
    .,
    "\"example_evaluation_1\""
  ) %>%
  stringr::str_squish() %T>%
  readr::write_lines(
    x = .,
    file = "R/example_evaluation_1.R",
    append = FALSE
  )

devtools::document()
