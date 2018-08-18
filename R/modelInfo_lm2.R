library(caret)
modelInfo_lm2 <- getModelInfo("rlm")$rlm
modelInfo_lm2$fit <- function(x, y, wts, param, lev, last, classProbs, ...) {
  dat <- if (is.data.frame(x)) x else as.data.frame(x)
  dat$.outcome <- y
  psi <- MASS::psi.huber # default
  if (param$psi == "psi.bisquare") {
    psi <- MASS::psi.bisquare
  } else
  if (param$psi == "psi.hampel") {
    psi <- MASS::psi.hampel
  }
  if (!is.null(wts)) {
    if (param$intercept) {
      out <- MASS::rlm(.outcome ~ x + I(x^2), data = dat, weights = wts, psi = psi, ...)
    } else {
      out <- MASS::rlm(.outcome ~ 0 + x + I(x^2),
        data = dat,
        weights = wts, psi = psi, ...
      )
    }
  } else {
    if (param$intercept) {
      out <- MASS::rlm(.outcome ~ x + I(x^2), data = dat, psi = psi, ...)
    } else {
      out <- MASS::rlm(.outcome ~ 0 + x + I(x^2), data = dat, psi = psi, ...)
    }
  }
  out
}
