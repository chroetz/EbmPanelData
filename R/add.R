#' @export
addLagVariables <- function(data, varNames, lags) {
  names(lags) <- lags
  data <-
    data |>
    ungroup() |>
    arrange(iso, year) |>
    group_by(iso)
  for (varNm in varNames) {
    data <- mutate(data, "{varNm}_" := bind_cols(lapply(lags, \(l) lag(.data[[varNm]], l))))
  }
  data <-
    data |>
    ungroup() |>
    unnest_wider(all_of(paste0(varNames, "_")), names_sep="")
  return(data)
}

#' @export
addMonomials <- function(data, varNames, powers) {
  for (varNm in varNames) {
    for (p in powers) {
      data <- mutate(data, "{varNm}^{p}" := .data[[varNm]]^p)
    }
  }
  return(data)
}

#' @export
addFirstDiff <- function(data, varNames) {
  data <-
    data |>
    ungroup() |>
    arrange(iso, year) |>
    group_by(iso)
  for (varNm in varNames) {
    data <- mutate(data, "d_{varNm}" := .data[[varNm]] - lag(.data[[varNm]], 1))
  }
  data <-
    data |>
    ungroup()
  return(data)
}

#' @export
addTimeMean <- function(data, varNames) {
  dataMeans <-
    data |>
    ungroup() |>
    summarize(across(all_of(varNames), mean, .names="m_{.col}"), .by=i)
  data <-
    data |>
    ungroup() |>
    left_join(dataMeans, by = "i")
  return(data)
}


#' @export
addInteraction <- function(data, interactVars, symbol=":") {
  interactVarsExpanded <-
    interactVars |>
    lapply(\(vars) do.call(
      \(...) {
        df <- expand.grid(..., stringsAsFactors = FALSE)
        lapply(seq_len(nrow(df)), \(i) unlist(df[i,]))
      },
      lapply(vars, expandWildcard, vocabulary=colnames(data)))) |>
     unlist(recursive=FALSE)
  names(interactVarsExpanded) <- sapply(interactVarsExpanded, paste0, collapse=symbol)
  interactValues <- lapply(interactVarsExpanded, \(x) Reduce(`*`, data[x], 1))
  data[names(interactValues)] <- interactValues
  return(data)
}
