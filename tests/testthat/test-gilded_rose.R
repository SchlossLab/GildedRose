test_that("update_qualities aborts with NA", {

  expect_error(update_qualities(items = NA), "There was no item supplied")
})

test_that("update_qualities works with a list of items", {
  items <- list(item('+5 Dexterity Vest', 10, 20),
                aged_brie(2, 0),
                item('Elixir of the Mongoose', 5, 7))

  update <- update_qualities(items)
  expect_type(update, "list")
  expect_s3_class(update[[1]], "item")
})

test_that("a new item is created", {
  items <- item('foo', 0 , 0)
  items <- update_item(items)

  expect_equal("foo", items$name)
  expect_equal(-1 , items$sell_in)
  expect_equal(0, items$quality)
})

test_that("check quality is updated correctly", {
  items <-  item('foo', 10 , 10)
  items <- update_item(items)

  expect_equal("foo", items$name)
  expect_equal(9 , items$sell_in)
  expect_equal(9, items$quality)
})


test_that("check quality decreases by 2 after sell by", {
  items <-  item('foo', 0 , 10)
  items <- update_item(items)

  expect_equal("foo", items$name)
  expect_equal(-1 , items$sell_in)
  expect_equal(8, items$quality)
})

test_that("check brie increases in quality", {
  items <-  aged_brie(10 , 10)
  items <- update_item(items)

  expect_equal("Aged Brie", items$name)
  expect_equal(9 , items$sell_in)
  expect_equal(11, items$quality)
})

test_that("check brie increases in quality +2 after sell by", {
  items <-  aged_brie(0 , 10)
  items <- update_item(items)

  expect_equal("Aged Brie", items$name)
  expect_equal(-1 , items$sell_in)
  expect_equal(12, items$quality)
})

test_that("check quality does not increase > 50", {
  items <-  aged_brie(0 , 50)
  items <- update_item(items)

  expect_equal("Aged Brie", items$name)
  expect_equal(-1 , items$sell_in)
  expect_equal(50, items$quality)
})

test_that("check Sulfuras, Hand of Ragnaros is always 80", {
  items <-  sulfuras(0 , 80)
  items <- update_item(items)

  expect_equal("Sulfuras, Hand of Ragnaros", items$name)
  expect_equal(0 , items$sell_in)
  expect_equal(80, items$quality)
})

test_that("check Backstage passes to a TAFKAL80ETC concert increase by 1", {
  items <-  backstage_pass(15 , 20)
  items <- update_item(items)

  expect_equal("Backstage passes to a TAFKAL80ETC concert", items$name)
  expect_equal(14 , items$sell_in)
  expect_equal(21, items$quality)
})

test_that("check Backstage passes to a TAFKAL80ETC concert increase by 2", {
  items <-  backstage_pass(8 , 20)
  items <- update_item(items)

  expect_equal("Backstage passes to a TAFKAL80ETC concert", items$name)
  expect_equal(7 , items$sell_in)
  expect_equal(22, items$quality)
})

test_that("check Backstage passes to a TAFKAL80ETC concert increase by 3", {
  items <-  backstage_pass(4 , 20)
  items <- update_item(items)

  expect_equal("Backstage passes to a TAFKAL80ETC concert", items$name)
  expect_equal(3 , items$sell_in)
  expect_equal(23, items$quality)
})

test_that("check Backstage passes to a TAFKAL80ETC concert is 0 after sell in is reached", {
  items <-  backstage_pass(0 , 20)
  items <- update_item(items)

  expect_equal("Backstage passes to a TAFKAL80ETC concert", items$name)
  expect_equal(-1 , items$sell_in)
  expect_equal(0, items$quality)
})

test_that("check conjured items", {
  item <- conjured(10, 10)
  items <- update_item(item)

  expect_equal(items$sell_in, 9)
  expect_equal(items$quality, 8)

  item <- conjured(10, 0)
  items <- update_item(item)

  expect_equal(items$sell_in, 9)
  expect_equal(items$quality, 0)
})

# test_that("Workflow with list of items works as expected", {
#
#   source("../texttest_fixture.R")
#   expect_snapshot(cat(gilded_rose_fixture()))
#
# })
