#' vegetion index calculation handler
#'
#' @param index: name of index, used to match function
#' @param spc: spc obj
#' @param sensor: in c('SVC', 'SEN', 'LS8') to match bands
#'
#' @return vi vector
#' @export
calcVI <- function(index, spc, sensor = "SVC", ...) {

  # define VIs --------------------------------------------------------------

  # VIs use red nir-----------------------------------------------------------------
  # Jordan (1969)
  SR <- function(...) {
    param <- list(...)
    bNIR <- param$bNIR
    bRed <- param$bRed

    bNIR / bRed
  }

  # Tucker (1979)
  NDVI <- function(...) {
    param <- list(...)
    bNIR <- param$bNIR
    bRed <- param$bRed

    (bNIR - bRed) / (bNIR + bRed)
  }

  DVI <- function(...) {
    param <- list(...)
    bNIR <- param$bNIR
    bRed <- param$bRed

    bNIR - bRed
  }

  # Huete (1988)
  SAVI <- function(...) {
    param <- list(...)
    bNIR <- param$bNIR
    bRed <- param$bRed
    L <- ifelse(is.null(param$L), 0.5, param$L)

    (1 + L) * (bNIR - bRed) / (bNIR + bRed + L)
  }

  # Qi et al. (1994)
  MSAVI <- function(...) {
    param <- list(...)
    bNIR <- param$bNIR
    bRed <- param$bRed

    bb <- -(2 * bNIR + 1)
    cc <- 2 * (bNIR - bRed)
    (-bb - sqrt(bb^2 - 4 * cc)) / 2.0
  }

  # Roujean and Breon (1995)
  RDVI <- function(...) {
    param <- list(...)
    bNIR <- param$bNIR
    bRed <- param$bRed

    (bNIR - bRed) / sqrt(bNIR + bRed)
  }

  # Rondeaux et al.(1996)
  OSAVI <- function(...) {
    param <- list(...)
    bNIR <- param$bNIR
    bRed <- param$bRed

    (1 + 0.16) * (bNIR - bRed) / (bNIR + bRed + 0.16)
  }


  WDRVI <- function(...) {
    param <- list(...)
    bNIR <- param$bNIR
    bRed <- param$bRed
    a <- ifelse(is.null(param$a), 0.1, param$L)

    (a * bNIR - bRed) / (a * bNIR + bRed)
  }


  # VIs use green bands-------------------------------------------------------------

  # Gitelson et al. (1996)
  Green_NDVI <- function(...) {
    param <- list(...)
    bNIR <- param$bNIR
    bGreen <- param$bGreen

    (bNIR - bGreen) / (bNIR + bGreen)
  }

  # Gitelson et al, (2003)
  CI_Green <- function(...) {
    param <- list(...)
    bNIR <- param$bNIR
    bGreen <- param$bGreen

    bNIR / bGreen - 1
  }

  # VIs use blue bands-------------------------------------------------------------

  # Huete et al. (1997)
  EVI <- function(...) {
    param <- list(...)
    bNIR <- param$bNIR
    bRed <- param$bRed
    bBlue <- param$bBlue
    if (is.null(bBlue)) {
      stop(sprintf("No valid bule bands: %s", names(param)))
    }

    2.5 * ((bNIR - bRed) / (bNIR - 6 * bRed - 7.5 * bBlue + 1))
  }

  # Haboudane et al.(2004)
  MCARI2 <- function(...) {
    param <- list(...)
    bNIR <- param$bNIR
    bRed <- param$bRed
    bGreen <- param$bGreen

    num <- 1.5 * (2.5 * (bNIR - bRed) - 1.3 * (bNIR - bGreen))
    deo <- sqrt(2 * (bNIR + 1)^2 + 6 * bNIR + 5 * sqrt(bRed) - 0.5)
    num / deo
  }

  MTVI2 <- function(...) {
    param <- list(...)
    bNIR <- param$bNIR
    bRed <- param$bRed
    bGreen <- param$bGreen

    num <- 1.5 * (1.2 * (bNIR - bGreen) - 2.5 * (bRed - bGreen))
    deo <- sqrt(2 * (bNIR + 1)^2 + 6 * bNIR + 5 * sqrt(bRed) - 0.5)
    num / deo
  }

  # VIs use RE bands----------------------------------------------------------------

  # Daughtry et al. (2000)
  MCARI <- function(...) {
    param <- list(...)
    bNIR <- param$bNIR
    r700 <- param$r700
    bRed <- param$bRed

    ((b700 - bRed) - 0.2 * (b700 - bGreen)) * (b700 / bRed)
  }


  # Broge and Leblanc
  TVI <- function(...) {
    param <- list(...)
    r750 <- param$r750
    bRed <- param$bRed
    bGreen <- param$bGreen
    if (is.null(r750)) r750 <- param$r740

    0.5 * (120 * (r750 - bGreen) - 200 * (bRed - bGreen))
  }

  # Wu et al(2010)
  MCARI2_re <- function(...) {
    param <- list(...)
    r750 <- param$r750
    r705 <- param$r705
    bGreen <- param$bGreen
    if (is.null(r750)) r750 <- param$r740

    num <- 1.5 * (2.5 * (r750 - r705) - 1.3 * (r750 - bGreen))
    deo <- sqrt(2 * (r750 + 1)^2 + 6 * r750 + 5 * sqrt(r705) - 0.5)
    num / deo
  }


  CI_705 <- function(...) {
    param <- list(...)
    bNIR <- param$bNIR
    r705 <- param$r705

    bNIR / r705 - 1
  }

  CI_740 <- function(...) {
    param <- list(...)
    bNIR <- param$bNIR
    r740 <- param$r740

    bNIR / r740 - 1
  }

  CI_783 <- function(...) {
    param <- list(...)
    bNIR <- param$bNIR
    r783 <- param$r783

    bNIR / r783 - 1
  }


  # define bands ------------------------------------------------------------

  y <- spectra(spc)
  x <- wavelength(spc)
  param <- list(...)
  param$spc <- spc

  # Sentinel2
  if (sensor == "SEN") {
    param$bNIR <- get_reflectance(y, x, 842, FALSE)
    param$r705 <- get_reflectance(y, x, 705, FALSE)
    param$r740 <- get_reflectance(y, x, 740, FALSE)
    param$r783 <- get_reflectance(y, x, 783, FALSE)
    param$bRed <- get_reflectance(y, x, 665, FALSE)
    param$bGreen <- get_reflectance(y, x, 550, FALSE)
    param$r490 <- get_reflectance(y, x, 490, FALSE)
  } else if (sensor == "LS8") {
    param$bNIR <- get_reflectance(y, x, 863.5, FALSE)
    param$bRed <- get_reflectance(y, x, 655.0, FALSE)
    param$bGreen <- get_reflectance(y, x, 557.0, FALSE)
    param$r490 <- get_reflectance(y, x, 482.5, FALSE)
  } else if (sensor == "SVC") {
    param$bNIR <- get_reflectance(y, x, 830, TRUE)
    param$r705 <- get_reflectance(y, x, 705, TRUE)
    param$r740 <- get_reflectance(y, x, 740, TRUE)
    param$r750 <- get_reflectance(y, x, 750, TRUE)
    param$r783 <- get_reflectance(y, x, 783, TRUE)
    param$bRed <- get_reflectance(y, x, 670, TRUE)
    param$bGreen <- get_reflectance(y, x, 550, TRUE)
    param$r490 <- get_reflectance(y, x, 490, TRUE)
  } else {
    (
      stop(sprint("sensor: %s not defined!!", sensor))
    )
  }


  # calc VI -----------------------------------------------------------------
  vi <- NULL

  # incase rededge param calc by hsdar::rededge
  if (index == "l0") {
    vi <- rededge(spc)$l0
  } else if (index == "lp") {
    vi <- rededge(spc)$lp
  } else if (index == "ls") {
    vi <- rededge(spc)$ls
  } else if (index == "R0") {
    vi <- rededge(spc)$R0
  } else if (index == "Rp") {
    vi <- rededge(spc)$Rp
  } else if (index == "Rs") {
    vi <- rededge(spc)$Rs
  } else if (index == "NDVI_705") { # incase rededge NDVI, DVI, CI
    b1 <- param$r705
    b2 <- param$bNIR
    vi <- (b2 - b1) / (b2 + b1)
  } else if (index == "NDVI_740") {
    b1 <- param$r740
    b2 <- param$bNIR
    vi <- (b2 - b1) / (b2 + b1)
  } else if (index == "NDVI_783") {
    b1 <- param$r783
    b2 <- param$bNIR
    vi <- (b2 - b1) / (b2 + b1)
  } else if (index == "DVI_705") {
    b1 <- param$r705
    b2 <- param$bNIR
    vi <- b2 - b1
  } else if (index == "DVI_740") {
    b1 <- param$r740
    b2 <- param$bNIR
    vi <- b2 - b1
  } else if (index == "DVI_783") {
    b1 <- param$r783
    b2 <- param$bNIR
    vi <- b2 - b1
  } else if (index %in% c("Sum_Dr2", "Sum_Dr1", "MTCI", "mREIP", "REP_Li", "REP_LE")) {
    vi <- vegindex(spc, index, weighted = TRUE)
  } else if (str_detect(index, "^opt_NDVI")) { # incase NRI calc by hsdar::nri
    # wls <- str_extract_all(index, '\\d+(.\\d+)?') %>% unlist() %>% as.numeric()
    wls <- str_split(index, "_")[[1]][c(3, 4)] %>% parse_double()
    if (length(wls) != 2) stop("wrong NDSI index name!!!")
    b1 <- get_reflectance(spc, wls[1], weighted = FALSE)
    b2 <- get_reflectance(spc, wls[2], weighted = FALSE)
    vi <- (b2 - b1) / (b2 + b1)
  } else if (str_detect(index, "^opt_DVI")) { # incase NRI calc by hsdar::nri
    # wls <- str_extract_all(index, '\\d+(.\\d+)?') %>% unlist() %>% as.numeric()
    wls <- str_split(index, "_")[[1]][c(3, 4)] %>% parse_double()
    if (length(wls) != 2) stop("wrong NDSI index name!!!")
    b1 <- get_reflectance(spc, wls[1], weighted = FALSE)
    b2 <- get_reflectance(spc, wls[2], weighted = FALSE)
    vi <- b2 - b1
  } else { # others
    vi <- do.call(index, param)
  }

  if (is.matrix(vi)) {
    print(dim(vi))
    print(vi)
    stop(sprintf("index: %s result is matrix!", index))
  }

  if (length(vi) == 0) stop(sprintf("index: %s result length is 0!!!", index))

  return(vi)
}
