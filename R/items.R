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

as.character.item <- function(item) {
  paste(item$name, ", ", item$sell_in, ", ", item$quality, sep='')
}

print.item <- function(item) {
  print.default(as.character(item))
}
