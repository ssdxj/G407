#' change index to factor for display
#'
#' @param x index vector
#'
#' @return x factored with labels for plotmath
#' @export
#'
index2factor <- function(x) {
  if(is.factor(x)){
    if(all(levels(x) %in% index_levels)){
      return(x)
    } else {
      stop('index must not in factor!!! Use the Raw index name!!!')
    }
  }

  # choose
  flag <- index_levels %in% x
  factor(x, levels = index_levels[flag], labels = index_plotmath[flag])

}


#' change index  to for latex
#'
#' @param x index vector in factor mode
#' @return  x factored with labels for latex
#' @export
index2latex <- function(x) {
  if(is.factor(x)){
    if(all(levels(x) %in% index_levels)){
      return(x)
    } else {
      stop('index must not in factor!!! Use the Raw index name!!!')
    }
  }

  # choose
  flag <- index_levels %in% x
  factor(x, levels = index_levels[flag], labels = index_latex[flag])

}


#' change unsignificant cor value to NA according to pvalue
#'
#' @param pvalue pvalue
#' @param estimate cor value
#' @param thresh  pvalue thresh
#'
#' @return update estimate
#' @export
lowCor2NA <- function(pvalue, estimate, thresh = 0.01){
  estimate[pvalue > thresh] <- NA
  return(estimate)
}


#' change RMSE to normalized RMSE
#'
#' @param tag: tag vector to indicate each group(not factor)
#' @param RMSEvec: RMSE vector
#' @param obsMean: observed mean vector(must have name to match with tag)
#'
#' @return nRMSE in %
#' @export
RMSE2rRMSE <- function(tag, RMSEvec, obsMean) {
  if (is.factor(tag)) stop("tag is factor!!!!")

  for (nm in names(obsMean)) {
    RMSEvec[tag == nm] <- RMSEvec[tag == nm] / obsMean[nm] * 100
  }

  return(RMSEvec)
}

