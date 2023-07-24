

#' @export
logicalToNumeric <- function(data) {
  data |> mutate(across(where(is.logical), as.numeric))
}




#' @export
standardizeNumeric <- function(data, exclude = NULL) {
  data <-
    data |>
    mutate(across(
      where(is.numeric) & !any_of(exclude),
      \(x) (x-mean(x, na.rm=TRUE))/ifelse(sd(x, na.rm=TRUE)!=0,sd(x, na.rm=TRUE),1)))
  return(data)
}


#' @export
renameIsoYear <- function(data) {
  data <-
    data |>
    mutate(t = as.double(year)) |> # continuous time t
    mutate(year = as.character(year)) |> # categorical time year
    rename(i = iso) # short names
  return(data)
}




