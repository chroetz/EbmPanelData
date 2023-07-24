#' @export
getMeta <- function(data, name) {
  counts <- list(
    "number of observations" = nrow(data),
    "number of variables" = ncol(data),
    "number of years" = length(unique(data$year)),
    "number of countries" = length(unique(data$iso)),
    "number of NA rows" = nrow(data) - nrow(drop_na(data))
  )
  ranges <- lapply(data, \(x) paste(format(range(x, na.rm = TRUE), digits=3), collapse=" to "))
  res <- c(
    list(name = name),
    counts,
    ranges,
    list(
      #"variable names" = paste(colnames(data), collapse=","),
      "missing observations to full panel" = data |> complete(iso, year) |> nrow() - nrow(data)
    ),
    NULL
  )
  lapply(res, \(r) str_trim(as.character(r))) |> as_tibble()
}

