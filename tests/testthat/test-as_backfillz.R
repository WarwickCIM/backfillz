test_that("that backfillz object can be created", {

    sample_backfillz <- as_backfillz(sample_stanfit)

    load("expected_backfillz.RData")

    # wipe the plot log which has date information
    # the test will otherwise always fail
    sample_backfillz@plot_history <- data.frame()
    expected_backfillz@plot_history <- data.frame()

    testthat::expect_equal(sample_backfillz, expected_backfillz)
})