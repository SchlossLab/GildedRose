update_quality <- function(item){
  if(!is.character(item$name)) {
    cli::cli_abort("Item name should be a character.")
  }

  if(!is.double(item$quality)){
    cli::cli_abort("Item quality should be a double")
  }

  if(!is.double(item$sell_in)){
    cli::cli_abort("Item sell_in should be a double")
  }

  if (item$name == "Aged Brie") {

    if(item$sell_in <= 0 ) {
      item$quality <- item$quality + 2
    } else {
      item$quality <- item$quality + 1
    }

    item$quality <- ifelse(item$quality > 50, 50, item$quality)
    item$quality <- ifelse(item$quality < 0, 0, item$quality)
    item$sell_in <- item$sell_in - 1

    return(item)
  }

  if (item$name == "Sulfuras, Hand of Ragnaros") {

    item$quality <- 80
    return(item)

  }

  if (item$name == "Backstage passes to a TAFKAL80ETC concert") {

    if (item$sell_in > 10) {
      item$quality <- item$quality + 1
    } else if (item$sell_in > 5) {
      item$quality <- item$quality + 2
    } else if (item$sell_in > 0) {
      item$quality <- item$quality + 3
    } else {
      item$quality <- 0
    }

    item$quality <- ifelse(item$quality < 0, 0, item$quality)
    item$sell_in <- item$sell_in - 1

    return(item)

  }

  if (item$name == "Conjured") {
    item$quality <- item$quality - 2
    item$sell_in <- item$sell_in - 1

    item$quality <- ifelse(item$quality < 0, 0, item$quality)

    return(item)

  }

  return(update_item(item))

}

update_item <- function(item){
  UseMethod("update_item",item)
}

update_item.item <- function(item){
  if (item$sell_in > 0) {
    item$quality <- item$quality - 1
  } else {
    item$quality <- item$quality - 2
  }
  item$sell_in <- item$sell_in - 1

  item$quality <- ifelse(item$quality < 0, 0, item$quality)

  return(item)

}

update_qualities <- function(items) {
  if(is.na(items)) {
    cli::cli_abort("There was no item supplied, check item().")
  }

  lapply(items,
         update_quality
  )
}

# there are no stops/returns/skips, perhaps add one for Sulfuras
# write a function to evaluate quality:
# options: increase, decrease, neither

# add stop for quality < 0
# add stop for quality > 50
