test_that("items works", {
  laptop <- item("laptop", 0, 0)

  expect_s3_class(laptop, "item")
})


test_that("aged_brie return class aged_brie", {
  item <- aged_brie(0, 0)

  expect_s3_class(item, "aged_brie")
})


test_that("check item name is character", {

  expect_error(item(4, 0, 0), "Item name should be")

  item <- backstage_pass(0,0)
  expect_type(item$name,"character")
})

test_that("check quality and sell_in are double", {
  item <- backstage_pass(0, 0)
  items <- update_quality(item)

  expect_type(items$sell_in, "double")
  expect_type(items$quality, "double")

  expect_error(backstage_pass(sell_in = "a", quality = 0), "Item sell_in should be a double")
  expect_error(backstage_pass(0, "b"), "Item quality should be a double")
})
