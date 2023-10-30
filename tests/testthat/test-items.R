test_that("items works", {
  laptop <- item("laptop", 0, 0)

  expect_s3_class(laptop, "item")
})


test_that("aged_brie return class aged_brie", {
  item <- aged_brie(0, 0)

  expect_s3_class(item, "aged_brie")
})
