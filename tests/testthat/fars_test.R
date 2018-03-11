testthat::test_that("fars",{
  testthat::expect_that(fars::fars_summarize_years(c(2013,2014,2015)), is_a("tbl_df"))
  testthat::expect_that(fars::fars_map_state(53,2013), throws_error())
})
