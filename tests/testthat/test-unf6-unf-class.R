context("UNFv6: UNF Class Object")
test_that("Object is 'UNF' class", {
    expect_equal(class(unf6(1)), "UNF")
})

test_that("'UNF' class object prints", {
    u <- unf6(1)
    expect_equal(class(print(u)), "UNF", label = "print UNF")
    attr(u, "formatted") <- NULL
    expect_equal(class(print(u)), "UNF", label = "print UNF without 'formatted' attribute")
    attr(u, "version") <- NULL
    expect_equal(class(print(u)), "UNF", label = "print UNF without 'formatted' or 'version' attributes")
    u2 <- unf5(1)
    expect_equal(class(print(u2)), "UNF", label = "print UNF version < 6")
})

test_that("Object slots", {
    expect_equal(names(unf6(1)), c("unf","hash","unflong","formatted"), label = "object names correct")
    expect_equal(class(unf6(1)$unf), "character", label = "unf is character")
    expect_equal(class(unf6(1)$hash), "raw", label = "hash is raw")
    expect_equal(class(unf6(1)$unflong), "character", label = "unflong is character")
    expect_equal(class(unf6(1)$formatted), "character", label = "formatted is character")
})

test_that("Attributes", {
    expect_equal(attr(unf6(1), "class"), "UNF", label = "class")
    expect_equal(attr(unf6(1), "version"), 6, label = "version")
    expect_equal(attr(unf6(1), "digits"), 7, label = "digits")
    expect_equal(attr(unf6(1), "characters"), 128, label = "characters")
    expect_equal(attr(unf6(1), "truncation"), 128, label = "truncation")
})
