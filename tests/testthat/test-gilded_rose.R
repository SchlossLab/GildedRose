test_that("update_quality aborts with NA", {

  expect_error(update_quality(items = NA), "There was no item supplied")
})

test_that("a new item is created", {
  items <- list( item('foo', 0 ,0))
  items <- update_quality(items)

  expect_equal("foo", items[[1]]$name)
  expect_equal(-1 , items[[1]]$sell_in)
  expect_equal(0, items[[1]]$quality)
})

test_that("check quality is updated correctly", {
  items <- list( item('foo', 10 , 10))
  items <- update_quality(items)

  expect_equal("foo", items[[1]]$name)
  expect_equal(9 , items[[1]]$sell_in)
  expect_equal(9, items[[1]]$quality)
})


test_that("check quality decreases by 2 after sell by", {
  items <- list( item('foo', 0 , 10))
  items <- update_quality(items)

  expect_equal("foo", items[[1]]$name)
  expect_equal(-1 , items[[1]]$sell_in)
  expect_equal(8, items[[1]]$quality)
})

test_that("check brie increases in quality", {
  items <- list( item('Aged Brie', 10 , 10))
  items <- update_quality(items)

  expect_equal("Aged Brie", items[[1]]$name)
  expect_equal(9 , items[[1]]$sell_in)
  expect_equal(11, items[[1]]$quality)
})

test_that("check brie increases in quality +2 after sell by", {
  items <- list( item('Aged Brie', 0 , 10))
  items <- update_quality(items)

  expect_equal("Aged Brie", items[[1]]$name)
  expect_equal(-1 , items[[1]]$sell_in)
  expect_equal(12, items[[1]]$quality)
})

test_that("check quality does not increase > 50", {
  items <- list( item('Aged Brie', 0 , 50))
  items <- update_quality(items)

  expect_equal("Aged Brie", items[[1]]$name)
  expect_equal(-1 , items[[1]]$sell_in)
  expect_equal(50, items[[1]]$quality)
})

test_that("check Sulfuras, Hand of Ragnaros is always 80", {
  items <- list( item('Sulfuras, Hand of Ragnaros', 0 , 80))
  items <- update_quality(items)

  expect_equal("Sulfuras, Hand of Ragnaros", items[[1]]$name)
  expect_equal(0 , items[[1]]$sell_in)
  expect_equal(80, items[[1]]$quality)
})

test_that("check Backstage passes to a TAFKAL80ETC concert increase by 1", {
  items <- list( item('Backstage passes to a TAFKAL80ETC concert', 15 , 20))
  items <- update_quality(items)

  expect_equal("Backstage passes to a TAFKAL80ETC concert", items[[1]]$name)
  expect_equal(14 , items[[1]]$sell_in)
  expect_equal(21, items[[1]]$quality)
})

test_that("check Backstage passes to a TAFKAL80ETC concert increase by 2", {
  items <- list( item('Backstage passes to a TAFKAL80ETC concert', 8 , 20))
  items <- update_quality(items)

  expect_equal("Backstage passes to a TAFKAL80ETC concert", items[[1]]$name)
  expect_equal(7 , items[[1]]$sell_in)
  expect_equal(22, items[[1]]$quality)
})

test_that("check Backstage passes to a TAFKAL80ETC concert increase by 3", {
  items <- list( item('Backstage passes to a TAFKAL80ETC concert', 4 , 20))
  items <- update_quality(items)

  expect_equal("Backstage passes to a TAFKAL80ETC concert", items[[1]]$name)
  expect_equal(3 , items[[1]]$sell_in)
  expect_equal(23, items[[1]]$quality)
})

test_that("check Backstage passes to a TAFKAL80ETC concert is 0 after sell in is reached", {
  items <- list( item('Backstage passes to a TAFKAL80ETC concert', 0 , 20))
  items <- update_quality(items)

  expect_equal("Backstage passes to a TAFKAL80ETC concert", items[[1]]$name)
  expect_equal(-1 , items[[1]]$sell_in)
  expect_equal(0, items[[1]]$quality)
})
