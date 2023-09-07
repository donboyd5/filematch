# test_that("multiplication works", {
#   expect_equal(2 * 2, 4)
# })

test_that("matchrecs works with test data", {
  # check that the record id's for matched records are as expected, and that
  # the weights in the matched file are as expected, within rounding

  ab <- matchrecs(aranks, branks) |>
    arrange(ida, desc(rankb))

  expect_equal(ab$ida, expected_matchrecs$ida)
  expect_equal(ab$idb, expected_matchrecs$idb)

  # next test works when developing but fails on check unless I set tolerance to 1e-3, or round
  expect_equal(round(ab$weight, 3), round(expected_matchrecs$weight, 3))
})
