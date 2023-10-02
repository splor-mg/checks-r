as_data_table <- function(df) {
  if (is.data.table(df)) {
    result <- data.table::copy(df)
  } else {
    result <- as.data.table(df)
  }
  result
}

pp <- function(x) {
  result <- formattable::accounting(x, big.mark = ".", decimal.mark = ",")
  result
}
