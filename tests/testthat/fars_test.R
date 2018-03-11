test_that("fars",{
  test1 <- fars::fars_summarize_years(c(2013,2014,2015))
  test2 <- fars::fars_map_state(73,2013)
  expect_that(test1, is_a("tbl_df"))
  expect_that(test2, throws_error())
})
