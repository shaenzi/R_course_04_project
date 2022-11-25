# testing the csv helper function ------------------------

test_that("get non-empty tibble with proper link", {
  my_url <- "https://data.stadt-zuerich.ch/dataset/sid_srz_hilfsfirsten_rd/download/hilfsfrist_rd.csv"
  expect_type(get_csv_from_link(my_url), "list")
  expect_s3_class(get_csv_from_link(my_url),
                  class(dplyr::tibble()))
  expect_gt(max(dim(get_csv_from_link(my_url))), 0)
})

test_that("get error if no data at that link", {
  bad_url <- "/dataset/sid_srz_hilfsfirsten_rd/download/hilfsfrist_rd.csv"
  expect_error(get_csv_from_link(bad_url))
  # not sure how I could test an empty df?
})

# testing the overall get_zuerich_data function -------------

test_that("get list of tibbles with more than 1 entry", {
  expect_type(get_zurich_data(), "list")
  expect_s3_class(get_zurich_data(), "list")
  expect_gt(length(get_zurich_data()), 1)
  expect_s3_class(get_zurich_data()[[1]], class(dplyr::tibble()))
})
