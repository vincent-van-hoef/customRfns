globalVariables(c("uniprot_id", "UniProt", "Marker", "marker_id"))

#' Add UniProt IDs to panel
#'
#' @param panel_path Path to panel file
#' @return None
#' @export
add_uniprot_to_panel <- function(panel_path) {
    # Split input panel file into header and marker data
    split_panel <- list(
        header = grep(
            "^#",
            readr::read_lines(
                panel_path
            ),
            value = TRUE
        ),
        markers_old = readr::read_csv(
            panel_path,
            col_names = TRUE,
            comment = "#",
            show_col_types = FALSE
        )
    )

    # Verify expected UniProt ID for KLRG1 as quick sanity check for the master data
    assertthat::are_equal(
        uniprot_master |>
            dplyr::filter(Marker == "KLRG1") |>
            dplyr::select(UniProt) |>
            dplyr::pull(UniProt),
        "Q96E93"
    )

    # Join UniProt IDs to panel data:
    # - Match on marker_id
    # - Place UniProt column before sequence_1
    # - Convert all columns to character
    # - Replace NA with empty string
    split_panel$markers_with_uniprot <- split_panel$markers_old %>%
        dplyr::left_join(uniprot_master, by = c("marker_id" = "Marker")) |>
        dplyr::rename(uniprot_id = UniProt) |>
        dplyr::relocate(uniprot_id, .before = "sequence_1") |>
        dplyr::mutate(across(everything(), as.character)) |>
        dplyr::mutate(across(everything(), ~ tidyr::replace_na(., "")))

    # Test that old and new data are identical except for UniProt column
    # Get column names excluding UniProt
    old_cols <- c(
        "marker_id",
        "control",
        "nuclear",
        "full_name",
        "alt_id",
        "sequence_1",
        "conj_id",
        "sequence_2"
    )
    new_cols <- setdiff(
        colnames(split_panel$markers_with_uniprot),
        "uniprot_id"
    )
    assertthat::are_equal(old_cols, new_cols)

    # Test data values match for all columns except UniProt
    for (col in old_cols) {
        assertthat::are_equal(
            split_panel$markers_old[[col]],
            split_panel$markers_with_uniprot[[col]]
        )
        message("Data check passed: Values match for column '", col, "'")
    }

    # Test that CD3e has expected UniProt ID as quality check
    if (
        assertthat::are_equal(
            split_panel$markers_with_uniprot |>
                dplyr::filter(marker_id == "CD3e") |>
                dplyr::select(uniprot_id) |>
                dplyr::pull(uniprot_id),
            "P07766"
        )
    ) {
        message("Data check passed: CD3e has expected UniProt ID P07766")
    }

    # Write output file:
    # 1. Create output filename by adding _uniprot suffix
    # 2. Write header
    # 3. Write column names
    # 4. Write marker data
    file <- gsub(".csv", "_uniprot.csv", panel_path)
    readr::write_lines(unlist(split_panel$header), file)
    split_panel$markers_with_uniprot |>
        colnames() |>
        paste0(collapse = ",") |>
        readr::write_lines(file, append = TRUE)
    readr::write_csv(split_panel$markers_with_uniprot, file, append = TRUE)
}
