test_that("spiral stream plot is correctly generated", {
    library(backfillz)
    library(tools)

    # write out plot as a postscript file
    x <- as_backfillz(sample_stanfit)
    postscript(file = "sample_spiral_stream.ps",
     fonts = c("sans", "Palatino"))
    x <- plot_spiral_stream(x, verbose = FALSE)
    dev.off()

    # compare md5sums of file
    # ps files are plain text
    expect_true(
        md5sum("sample_spiral_stream.ps") ==
        md5sum("expected_spiral_stream.ps")
    )

})