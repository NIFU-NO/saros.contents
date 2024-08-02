testthat::test_that("make_content.cat_table_html works", {
  result <-
    saros.contents::makeme(data = ex_survey,
         dep = p_1:p_4, #indep = x2_human,
         type = "cat_table_html",
         showNA = "never",
         add_n_to_label = TRUE)
  testthat::expect_equal(as.character(result$.variable_label[[4]]), "Blue Party (N = 266)")
})
