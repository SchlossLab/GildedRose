test_that("update_quality aborts with NA", {

  expect_error(update_qualities(items = NA), "There was no item supplied")
})

test_that("a new item is created", {
  items <- item('foo', 0 , 0)
  items <- update_quality(items)

  expect_equal("foo", items$name)
  expect_equal(-1 , items$sell_in)
  expect_equal(0, items$quality)
})

test_that("check quality is updated correctly", {
  items <-  item('foo', 10 , 10)
  items <- update_quality(items)

  expect_equal("foo", items$name)
  expect_equal(9 , items$sell_in)
  expect_equal(9, items$quality)
})


test_that("check quality decreases by 2 after sell by", {
  items <-  item('foo', 0 , 10)
  items <- update_quality(items)

  expect_equal("foo", items$name)
  expect_equal(-1 , items$sell_in)
  expect_equal(8, items$quality)
})

test_that("check brie increases in quality", {
  items <-  aged_brie(10 , 10)
  items <- update_quality(items)

  expect_equal("Aged Brie", items$name)
  expect_equal(9 , items$sell_in)
  expect_equal(11, items$quality)
})

test_that("check brie increases in quality +2 after sell by", {
  items <-  aged_brie(0 , 10)
  items <- update_quality(items)

  expect_equal("Aged Brie", items$name)
  expect_equal(-1 , items$sell_in)
  expect_equal(12, items$quality)
})

test_that("check quality does not increase > 50", {
  items <-  aged_brie(0 , 50)
  items <- update_quality(items)

  expect_equal("Aged Brie", items$name)
  expect_equal(-1 , items$sell_in)
  expect_equal(50, items$quality)
})

test_that("check Sulfuras, Hand of Ragnaros is always 80", {
  items <-  sulfuras(0 , 80)
  items <- update_quality(items)

  expect_equal("Sulfuras, Hand of Ragnaros", items$name)
  expect_equal(0 , items$sell_in)
  expect_equal(80, items$quality)
})

test_that("check Backstage passes to a TAFKAL80ETC concert increase by 1", {
  items <-  backstage_pass(15 , 20)
  items <- update_quality(items)

  expect_equal("Backstage passes to a TAFKAL80ETC concert", items$name)
  expect_equal(14 , items$sell_in)
  expect_equal(21, items$quality)
})

test_that("check Backstage passes to a TAFKAL80ETC concert increase by 2", {
  items <-  backstage_pass(8 , 20)
  items <- update_quality(items)

  expect_equal("Backstage passes to a TAFKAL80ETC concert", items$name)
  expect_equal(7 , items$sell_in)
  expect_equal(22, items$quality)
})

test_that("check Backstage passes to a TAFKAL80ETC concert increase by 3", {
  items <-  backstage_pass(4 , 20)
  items <- update_quality(items)

  expect_equal("Backstage passes to a TAFKAL80ETC concert", items$name)
  expect_equal(3 , items$sell_in)
  expect_equal(23, items$quality)
})

test_that("check Backstage passes to a TAFKAL80ETC concert is 0 after sell in is reached", {
  items <-  backstage_pass(0 , 20)
  items <- update_quality(items)

  expect_equal("Backstage passes to a TAFKAL80ETC concert", items$name)
  expect_equal(-1 , items$sell_in)
  expect_equal(0, items$quality)
})

test_that("check item name is character", {
  item <- item(4, 0, 0)

  expect_error(update_quality(item), "Item name should be")

  item <- item("Backstage passes to a TAFKAL80ETC concert",0,0)
  items <- update_quality(item)
  expect_type(items$name,"character")
})

test_that("check quality and sell_in are double", {
  item <- item("Backstage", 0, 0)
  items <- update_quality(item)

  expect_type(items$sell_in, "double")
  expect_type(items$quality, "double")

  item <- item("Backstage", sell_in = "a", quality = 0)
  expect_error(update_quality(item), "Item sell_in should be a double")

  item <- item("Backstage", 0, "b")
  expect_error(update_quality(item), "Item quality should be a double")

})

test_that("check conjured items", {
  item <- conjured(10, 10)
  items <- update_quality(item)

  expect_equal(items$sell_in, 9)
  expect_equal(items$quality, 8)

  item <- conjured(10, 0)
  items <- update_quality(item)

  expect_equal(items$sell_in, 9)
  expect_equal(items$quality, 0)
})

