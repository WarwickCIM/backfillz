test_that("slice histogram plot is correctly generated", {
    library(backfillz)
    library(tools)

    # write out plot as a postscript file
    x <- as_backfillz(sample_stanfit, verbose = FALSE)
    postscript(file = "sample_slice_histogram.ps",
     fonts = c("sans", "Palatino"))
    x <- plot_slice_histogram(x, verbose = FALSE)
    dev.off()

    # compare md5sums of file
    # ps files are plain text

    # postscript files seem to be platform dependent
    # we have expected files for Windows
    if (version$platform == "x86_64-w64-mingw32") {
        plots_same <-
            md5sum("sample_slice_histogram.ps") ==
            md5sum("expected_slice_histogram_windows10.ps")
        # cleanup
        file.remove("sample_slice_histogram.ps")
        expect_true(plots_same)
    } else {
        expect_true(TRUE)
    }

})