# Download and read the data ---------------------------------------------------
url_root <- "https://www.accessdata.fda.gov/cdrh_docs/presentations/maf/"
# A Panel 1 evaluation:
ex_eval <-
  readr::read_csv(
    file = paste0(url_root, "maf3246-a001.csv"),
    col_types =
      readr::cols(
        date_performed = readr::col_date(format = ""),
        days_from_symptom = readr::col_double(),
        .default = readr::col_character()
      )
    )

nci_sero_panel_1_details <-
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
nci_1 <-
  build_panel_sheet(
    panel_name = "NCI Panel 1",
    panel_description =
      paste0(
        "A panel used in Spring - Summer 2020 to evaluate serology assays for ",
        "SARS-CoV-2. Use in this package for testing purposes only. ",
        "See https://open.fda.gov/apis/device/covid19serology/ for more ",
        "information."
      ),
    n_samples = 110L,
    sample_groups = c("Positive", "Negative", "HIV+"),
    sample_matrices = c("Serum", "Plasma"),
    analytes = c("IgM", "IgG", "Pan-Ig"),
    targets = "Antibodies to SARS-CoV-2 Spike Protein Antigen",
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
nci_1$panel_table$sample <- nci_sero_panel_1_details$sample
nci_1$panel_table$analyte <- nci_sero_panel_1_details$analyte
nci_1$panel_table$matrix <- nci_sero_panel_1_details$matrix
nci_1$panel_table$group <- nci_sero_panel_1_details$group
nci_1$panel_table$qualitative_truth <-
  nci_sero_panel_1_details$qualitative_result
nci_1$panel_table$semiquantitative_truth <-
  nci_sero_panel_1_details$semiquantitative_result %>%
  as.character()
nci_1$panel_table <-
  nci_1$panel_table %>%
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

# Build the evaluation sheet ---------------------------------------------------
suppressWarnings(
  test_eval <-
    build_evaluation_sheet(
      evaluation_name = "Test of NCI Panel 1 Evaluation",
      evaluation_description = "Nothing",
      developer = ex_eval$manufacturer %>% unique(),
      assay = ex_eval$device %>% unique(),
      lot_numbers = ex_eval$lot_number %>% unique(),
      panel_data = nci_1,
      panel_data_filepath = NA_character_,
      analytes = "IgG",
      targets = "Spike",
      qualitative_outcomes = c("Positive", "Borderline", "Negative"),
      semiquantitative_outcomes = NA_character_,
      quantitative_units = NA_character_,
      randomize = FALSE,
      blind = FALSE
    )
)

test_eval$evaluation_table$sample <- ex_eval$sample_id
test_eval$evaluation_table$datetime_observation <- ex_eval$date_performed
test_eval$evaluation_table$qualitative_result <- ex_eval$igg_result
