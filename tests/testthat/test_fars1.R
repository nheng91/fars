testthat::test_that("fars test",{
  testthat::expect_that(fars::fars_summarize_years(c(2013,2014,2015)), is_a("tbl_df"))
})
