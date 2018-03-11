testthat::test_that("test_fars_summarize_years",{
  test1 <- fars::fars_summarize_years(c(2013,2014,2015))
  testthat::expect_that(test1, is_a("tbl_df"))
})
