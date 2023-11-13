update_item <- function(x){
  UseMethod("update_item", x)
}

update_item.item <- function(x){
  if (x$sell_in > 0) {
    x$quality <- x$quality - 1
  } else {
    x$quality <- x$quality - 2
  }
  x$sell_in <- x$sell_in - 1

  x$quality <- ifelse(x$quality < 0, 0, x$quality)

  return(x)

}

update_item.aged_brie <- function(x) {

     if(x$sell_in <= 0 ) {
      x$quality <- x$quality + 2
    } else {
      x$quality <- x$quality + 1
    }

    x$quality <- ifelse(x$quality > 50, 50, x$quality)
    x$quality <- ifelse(x$quality < 0, 0, x$quality)
    x$sell_in <- x$sell_in - 1

    return(x)

}

update_item.sulfuras <- function(x) {

  x$quality <- 80
  x$sell_in <- 0
  return(x)

}

update_item.backstage_pass <- function(x){

    if (x$sell_in > 10) {
      x$quality <- x$quality + 1
    } else if (x$sell_in > 5) {
      x$quality <- x$quality + 2
    } else if (x$sell_in > 0) {
      x$quality <- x$quality + 3
    } else {
      x$quality <- 0
    }

    x$quality <- ifelse(x$quality < 0, 0, x$quality)
    x$sell_in <- x$sell_in - 1

    return(x)
}

update_item.conjured <- function(x) {
    x$quality <- x$quality - 2
    x$sell_in <- x$sell_in - 1

    x$quality <- ifelse(x$quality < 0, 0, x$quality)

    return(x)
}

update_qualities <- function(items) {
  if(is.na(items[1])) {
    cli::cli_abort("There was no item supplied, check item().")
  }

  lapply(items[1],
         update_item
  )
  # update_item(items[[1]])

}

# there are no stops/returns/skips, perhaps add one for Sulfuras
# write a function to evaluate quality:
# options: increase, decrease, neither

# add stop for quality < 0
# add stop for quality > 50

# need to create snapshot test with list of items
