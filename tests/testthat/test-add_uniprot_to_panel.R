add_uniprot_to_panel(system.file(
  "extdata",
  "proxiome-immuno-155.csv",
  package = "customRfns"
))

test_that("number of rows of panel with UniProt IDs is the same as the original panel", {
  expect_equal(
    nrow(readr::read_csv(system.file(
      "extdata",
      "proxiome-immuno-155_uniprot.csv",
      package = "customRfns"
    ))),
    nrow(readr::read_csv(system.file(
      "extdata",
      "proxiome-immuno-155.csv",
      package = "customRfns"
    )))
  )
})

test_that("Expected column names are present", {
  expect_equal(
    colnames(
      readr::read_csv(
        system.file(
          "extdata",
          "proxiome-immuno-155_uniprot.csv",
          package = "customRfns"
        ),
        comment = "#"
      )
    ),
    c(
      "marker_id",
      "control",
      "nuclear",
      "full_name",
      "alt_id",
      "uniprot_id",
      "sequence_1",
      "conj_id",
      "sequence_2"
    )
  )
  expect_equal(
    colnames(readr::read_csv(
      system.file(
        "extdata",
        "proxiome-immuno-155.csv",
        package = "customRfns"
      ),
      comment = "#"
    )),
    c(
      "marker_id",
      "control",
      "nuclear",
      "full_name",
      "alt_id",
      "sequence_1",
      "conj_id",
      "sequence_2"
    )
  )
})
