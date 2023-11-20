#' create items class
#'
#' @param name A name for the item
#' @param sell_in A numeric denoting sell by in days
#' @param quality quality
#'
#' @return An items object
#' @export
#'
#' @examples
#' new_item <- item("laptop", 0, 0)
item <- function(name, sell_in, quality) {
  if(!is.character(name)) {
    cli::cli_abort("Item name should be a character.")
  }

  if(!is.double(quality)){
    cli::cli_abort("Item quality should be a double")
  }

  if(!is.double(sell_in)){
    cli::cli_abort("Item sell_in should be a double")
  }

  newItem <- list(name=name, sell_in=sell_in, quality=quality)
  class(newItem) <- 'item'
  newItem
}

aged_brie <- function(sell_in, quality) {
  newItem <- item(name="Aged Brie", sell_in=sell_in, quality=quality)
  class(newItem) <- c('aged_brie', 'item')
  newItem
}

sulfuras <- function(sell_in, quality) {
  newItem <- item(name="Sulfuras, Hand of Ragnaros", sell_in=sell_in, quality=quality)
  class(newItem) <- c('sulfuras', 'item')
  newItem
}

backstage_pass <- function(sell_in, quality) {
  newItem <- item(name="Backstage passes to a TAFKAL80ETC concert", sell_in=sell_in, quality=quality)
  class(newItem) <- c('backstage_pass', 'item')
  newItem
}

conjured <- function(sell_in, quality) {
  newItem <- item(name="Conjured Mana Cake", sell_in=sell_in, quality=quality)
  class(newItem) <- c('conjured', 'item')
  newItem
}


#' @export
as.character.item <- function(item) {
  paste(item$name, ", ", item$sell_in, ", ", item$quality, sep='')
}

#' @export
print.item <- function(item) {
  print.default(as.character(item))
}
