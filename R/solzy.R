glmnetPath <- function (fit) {
  if (inherits(fit, "cv.glmnet")) fit <- fit$glmnet.fit
  if (!inherits(fit, "glmnet")) {
    stop("'fit' was not produced by glmnet() or cv.glmnet()!")
  }
  b <- fit$beta
  if (is.null(b)) stop("Matrix of coefficients not found!")
  if (length(b@x) == prod(b@Dim)) {
    stop("No coefficients were shrunk to 0!")
  }
  nm <- b@Dimnames[[1L]]
  lam <- fit$lambda
  pos <- which(!duplicated(b@i))
  i <- b@i[pos] + 1L
  j <- findInterval(pos, b@p, left.open = TRUE)
  ord <- cumsum(c(TRUE, diff.default(j) > 0L))
  enter <- data.frame(i = i, j = j, ord = ord, var = nm[i], lambda = lam[j])
  ind <- logical(length(i))
  ind[i] <- TRUE
  ind <- which(!ind)
  ignored <- data.frame(i = ind, var = nm[ind])
  reverse <- length(i):1L
  j <- j - 1L
  i <- i[reverse]
  j <- j[reverse]
  ord <- ord[length(ord)] + 1L - ord
  ord <- ord[reverse]
  leave <- data.frame(i = i, j = j, ord = ord, var = nm[i], lambda = lam[j])
  list(enter = enter, leave = leave, ignored = ignored)
}



