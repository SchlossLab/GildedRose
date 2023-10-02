test_that("items works", {
  laptop <- item("laptop", 0, 0)

  expect_s3_class(laptop, "item")
})
