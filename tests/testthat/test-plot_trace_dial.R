test_that("trace dial plot is correctly generated", {
    old_warning_level <- getOption("warn")
    options(warn = -1)

    library(backfillz)
    library(tools)

    # write out plot as a postscript file
    x <- as_backfillz(sample_stanfit, verbose = FALSE)
    postscript(file = "sample_trace_dial.ps",
     fonts = c("sans", "Palatino"))
    x <- plot_trace_dial(x, verbose = FALSE)
    dev.off()

    options(warn = old_warning_level)

    # compare md5sums of file
    # ps files are plain text

    # postscript files seem to be platform dependent
    # we have expected files for Windows
    if (version$platform == "x86_64-w64-mingw32") {
        plots_same <-
            md5sum("sample_trace_dial.ps") ==
            md5sum("expected_trace_dial_windows10.ps")
        # cleanup
        file.remove("sample_trace_dial.ps")
        expect_true(plots_same)
    } else {
        expect_true(TRUE)
    }

})