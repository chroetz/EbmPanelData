#' @export
makeDummies <- function(data) {
  x <- lapply(colnames(data), \(nm) {
    column <- data[[nm]]
    if (is.character(column)) {
      chrToDummy(column, nm)
    } else if (is.factor(column)) {
      fctToDummy(column, nm)
    }
  })
  bind_cols(data, bind_cols(x))
}

fctToDummy <- function(values, name) {
  chrToDummy(as.character(values), name)
}

chrToDummy <- function(values, name, removeFirst = FALSE) {
  values <- as.character(values)
  lvls <- unique(values)
  if (removeFirst) {
    lvls <- lvls[-1]
  }
  dummy <- do.call(cbind, lapply(lvls, \(l) values == l))
  colnames(dummy) <- paste0(name, ".", lvls)
  return(dummy)
}
