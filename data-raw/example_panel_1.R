## code to prepare `example_panel_1` dataset goes here
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

example_panel_1_details <-
  ex_eval %>%
  dplyr::select(
    sample = .data$sample_id,
    .data$group,
    matrix = .data$type,
    truth_IgM = .data$igm_truth,
    truth_IgG = .data$igg_truth,
    `truth_Pan-Ig` = .data$antibody_truth
  ) %>%
  tidyr::pivot_longer(
    cols = tidyselect::starts_with("truth_"),
    names_to = "analyte",
    names_prefix = "^truth_",
    values_to = "qualitative_result"
  ) %>%
  dplyr::left_join(
    x = .,
    y =
      ex_eval %>%
      dplyr::select(
        sample = .data$sample_id,
        titer_IgM = .data$igm_titer,
        titer_IgG = .data$igg_titer,
        `titer_Pan-Ig` = .data$pan_titer
      ) %>%
      tidyr::pivot_longer(
        cols = tidyselect::starts_with("titer_"),
        names_to = "analyte",
        names_prefix = "^titer_",
        values_to = "semiquantitative_result"
      ),
    by =
      c(
        "sample" = "sample",
        "analyte" = "analyte"
      )
  )

# Build the panel sheet --------------------------------------------------------
example_panel_1 <-
  build_panel_sheet(
    panel_name = "Example Panel 1",
    panel_description =
      paste0(
        "A panel used in Spring - Summer 2020 to evaluate serology assays for ",
        "SARS-CoV-2. Use in this package is for testing  and illustration ",
        "purposes only. See https://open.fda.gov/apis/device/covid19serology/ ",
        "for more information."
      ),
    n_samples = 110L,
    sample_groups = c("Positive", "Negative", "HIV+"),
    sample_matrices = c("Serum", "Plasma"),
    analytes = c("IgM", "IgG", "Pan-Ig"),
    targets = "Spike",
    qualitative_outcomes = c("Positive", "Negative"),
    qualitative_comparators =
      c(
        paste0(
          "PCR-confirmed and positive on CDC Spike Antigen Assays and ",
          "Krammer RBD assay at NCI"
        ),
        "Collected prior to 2020"
      ),
    semiquantitative_outcomes = c("0", "100", "400", "1600", "6400"),
    semiquantitative_comparators = "CDC Spike Antigen Assay",
    quantitative_units = NA_character_,
    quantitative_comparators = NA_character_
  )
example_panel_1$panel_table$sample <- example_panel_1_details$sample
example_panel_1$panel_table$analyte <- example_panel_1_details$analyte
example_panel_1$panel_table$matrix <- example_panel_1_details$matrix
example_panel_1$panel_table$group <- example_panel_1_details$group
example_panel_1$panel_table$qualitative_truth <-
  example_panel_1_details$qualitative_result
example_panel_1$panel_table$semiquantitative_truth <-
  example_panel_1_details$semiquantitative_result
example_panel_1$panel_table <-
  example_panel_1$panel_table %>%
  dplyr::mutate(
    qualitative_comparator =
      dplyr::case_when(
        .data$qualitative_truth == "Positive" ~
          paste0(
            "PCR-confirmed and positive on CDC Spike Antigen Assays and ",
            "Krammer RBD assay at NCI"
          ),
        .data$qualitative_truth != "Positive" ~
          "Collected prior to 2020"
      )
  )

usethis::use_data(example_panel_1, overwrite = TRUE)

# Document the dataset ---------------------------------------------------------

documentation_text <-
  c(
    "Example Panel 1",
    "",
    "A panel used in Spring - Summer 2020 to evaluate serology assays for ",
    "SARS-CoV-2. Use in this package is for testing  and illustration ",
    "purposes only. See https://open.fda.gov/apis/device/covid19serology/ ",
    "for more information.",
    "",
    "@format A list of tibbles.",
    "",
    "\\describe{",
    dplyr::glimpse(example_panel_1, width = 76) %>%
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
    paste0("accessed ", lubridate::today(), "."),
    " "
  ) %>%
  paste0("#' ", .) %>%
  c(
    paste0(
      "# Do not hand edit this file. Edit data-raw/example_panel_1.R ",
      "instead."
    ),
    .,
    "\"example_panel_1\""
  ) %>%
  stringr::str_squish() %T>%
  readr::write_lines(
    x = .,
    file = "R/example_panel_1.R",
    append = FALSE
  )

devtools::document()
