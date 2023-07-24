

panelNormalizer <- function(
    data,
    variables,
    dataName = NULL
) {
  variables <- lapply(variables, expandWildcard, vocabulary = colnames(data)) |> unlist()
  x <- data[variables] |> as.matrix()

  normalizerMatrix <-
    diag(1, nrow(x)) - x %*% tcrossprod(ginv(crossprod(x)), x)

  return(normalizerMatrix)
}

ginv <- function(X, tol = sqrt(.Machine$double.eps)) {
  Xsvd <- svd(X)
  posi <- Xsvd$d > max(tol * Xsvd$d[1L], 0)
  if (all(posi)) {
    res <- tcrossprod(Xsvd$v, Xsvd$u * rep(1/Xsvd$d, each=ncol(Xsvd$u)))
  } else {
    warning(paste0(sum(!posi), " low singular values"))
    if (!any(posi)) {
      res <- array(0, dim(X)[2L:1L])
    } else {
      res <- tcrossprod(Xsvd$v[, posi, drop = FALSE], Xsvd$u[, posi, drop = FALSE] * rep(1/Xsvd$d[posi], each=ncol(Xsvd$u)))
    }
  }
  return(res)
}


#' @export
removeEffects <- function(data, variables, dataName = NULL, exclude = NULL) {
  normalizer <- panelNormalizer(data, variables, dataName)
  data <-
    data |>
    mutate(across(
      where(is.numeric) & !any_of(exclude),
      \(x) as.vector(normalizer %*% x)))
  return(data)
}

