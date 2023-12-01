

#' @export
logicalToNumeric <- function(data) {
  data |> mutate(across(where(is.logical), as.numeric))
}




#' @export
standardizeNumeric <- function(data, scale=TRUE, exclude = NULL, includeOnly=NULL) {
  if (is.null(includeOnly)) {
    data <-
      data |>
      mutate(across(
        where(is.numeric) & !any_of(exclude),
        \(x) (x-mean(x, na.rm=TRUE))/ifelse(sd(x, na.rm=TRUE)!=0 & scale,sd(x, na.rm=TRUE),1)))
  } else {
    data <-
      data |>
      mutate(across(
        all_of(includeOnly) & !any_of(exclude),
        \(x) (x-mean(x, na.rm=TRUE))/ifelse(sd(x, na.rm=TRUE)!=0 & scale,sd(x, na.rm=TRUE),1)))
  }
  return(data)
}


#' @export
renameIsoYear <- function(data) {
  data <-
    data |>
    mutate(t = as.double(year)) |> # continuous time t
    mutate(year = as.character(year)) |> # categorical time year
    mutate(i = iso) # short names
  return(data)
}


#' @export
yearCateAndCont <- function(data) {
  data <-
    data |>
    mutate(t = as.double(year), .after = year) |> # continuous time t
    mutate(year = as.character(year)) # categorical time year
  return(data)
}




