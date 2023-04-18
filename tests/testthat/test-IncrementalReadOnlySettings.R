test_that("IncrementalReadOnlySettings constructor works", {

    out <- IncrementalReadOnlySettings(
        a = 1,
        b = "hello",
        c = TRUE,
        d = 5L
    )

    expect_s4_class(out, "IncrementalReadOnlySettings")

})
