#' @export
showTable <- function(data, caption, digits = 3) {
  data |>
    kableExtra::kbl(
      caption = caption,
      digits = digits,
      booktabs = TRUE,
      align = c("l", rep("r", ncol(data)-1))
    ) |>
    kableExtra::kable_styling(
      bootstrap_options = c("condensed", "striped"),
      latex_options = c("HOLD_position"),
      full_width = FALSE)
}

valueToColor <- function(value) {
  colors <- case_when(
    is.na(value) ~ rgb(0.6,0.6,0.6),
    value > 0.1 ~ rgb(1,0.5,0.5),
    value <= 0.1 & value > 0.05 ~ rgb(1,0.75,0.5),
    value <= 0.05 & value > 0.01 ~ rgb(1.0,1,0.5),
    value <= 0.01 ~ rgb(0.5,1,0.5))
  colors[is.na(colors)] <- rgb(0.6,0.6,0.6)
  colors
}

colorizeTable <- function(k, colorCols, valueTbl) {
  for (colId in colorCols) {
    k <- kableExtra::column_spec(
      k,
      colId,
      background = valueToColor(valueTbl[[colId]]))
  }
  k
}

#' @export
showCoefTable <- function(fitData, settings) {
  coefData <-
    fitData |>
    unnest_wider(fit) |>
    select(id, coefName, coef, pValue) |>
    unnest_longer(c(coefName, coef, pValue))
  textData <-
    coefData |>
    mutate(text = sprintf("%6.3f\n(%5.3f)", coef, pValue)) |>
    select(id, coefName, text) |>
    pivot_wider(names_from=coefName, values_from=text) |>
    left_join(settings, by = "id") |>
    relocate(all_of(colnames(settings)))
  valueData <-
    coefData |>
    select(id, coefName, pValue) |>
    pivot_wider(names_from=coefName, values_from=pValue) |>
    left_join(settings, by = "id") |>
    relocate(all_of(colnames(settings)))
  showValueTable(
    valueData,
    textData,
    caption = "coefficient (p-value)",
    preColumns = ncol(settings))
}

#' @export
showValueTable <- function(valueData, textData, caption, preColumns = 1, digits = 3, sort=TRUE, width = "1.5cm") {
  if (sort) {
    valueData <-
      valueData |>
      relocate(seq_len(preColumns), sort(tidyselect::peek_vars()))
    textData <-
      textData |>
      relocate(seq_len(preColumns), sort(tidyselect::peek_vars()))
  }
  valueColums <- preColumns + seq_len(ncol(textData)-preColumns)
  textData |>
    kableExtra::kbl(
      caption = caption,
      digits = digits,
      booktabs = TRUE,
      align = c(rep("l", preColumns), rep("r", ncol(textData)-preColumns))
    ) |>
    kableExtra::kable_styling(
      bootstrap_options = c("condensed"),
      latex_options = c("HOLD_position"),
      full_width = FALSE) |>
    colorizeTable(valueColums, valueData) |>
    kableExtra::column_spec(valueColums, width = width)
}
