test_that("df_to_stanarray correctly converts dataframes", {

    df <- data.frame(
                parameters = c("mu", "mu"),
                iterations = c(0.345, 0.263),
                chains = c(1, 2),
                stringsAsFactors = FALSE
                )

    sample_stanarray <- df_to_stanarray(df)

    load("expected_stan_array.RData")

    testthat::expect_equal(sample_stanarray, expected_stanarray)
})