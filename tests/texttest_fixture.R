gilded_rose_fixture <- function(){

  writeLines('OMGHAI!')

  items <- list(
    item('+5 Dexterity Vest', 10, 20),
    aged_brie(2, 0),
    item('Elixir of the Mongoose', 5, 7),
    sulfuras(0, 80),
    sulfuras(-1, 80),
    backstage_pass(15, 20),
    backstage_pass(10, 49),
    backstage_pass(5, 49),
    # This Conjured item does not work properly yet
    conjured(3, 6)
  )

  days <- 30
  for (day in 0:days) {
    writeLines(paste('-------- day ', day, ' --------', sep=''))
    writeLines('name, sellIn, quality')
    lapply(items,
      function(item) {
        writeLines(as.character(item))
      }
    )
    writeLines('')
    items <- update_qualities(items)
  }

  rm('day', 'days', 'items')
}
