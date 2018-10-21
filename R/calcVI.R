#' vegetion index calculation handler
#'
#' @param index: name of index, used to match function
#' @param spc: spc obj
#' @return vi vector
#' @export
calcVI <- function(index, spc, ...) {
  param <- list(...)
  weighted <- TRUE

  if(index %in% vegindex()){
    out <- vegindex(spc, index, ...)
  } else {
    wl_NIR <- ifelse(is.null(param$NIR), 800, as.double(param$NIR))
    wl_red <- ifelse(is.null(param$red), 670, as.double(param$red))
    wl_green <- ifelse(is.null(param$green), 550, as.double(param$green))
    bNIR <- get_reflectance(spc, wl_NIR, weighted = weighted)
    bR <- get_reflectance(spc, wl_red, weighted = weighted)
    bG <- get_reflectance(spc, wl_green, weighted = weighted)

    if(index == 'Green_NDVI'){
      out <- vegindex(spc, 'Green NDVI', weighted = weighted)
    } else if(index == 'MCARI_OSAVI'){
      out <- vegindex(spc, 'MCARI/OSAVI', weighted = weighted)
    } else if(index == 'MCARI2_OSAVI2'){
      out <- vegindex(spc, 'MCARI2/OSAVI', weighted = weighted)
    } else if(index == 'PRI_CI2'){
      out <- vegindex(spc, 'PRI*CI2', weighted = weighted)
    } else if(index == 'TCARI_OSAVI'){
      out <- vegindex(spc, 'TCARI/OSAVI', weighted = weighted)
    } else if(index == 'TCARI2_OSAVI2'){
      out <- vegindex(spc, 'TCARI2/OSAVI2', weighted = weighted)
    } else if(index == 'DVI') {
      out <- bNIR - bR
    } else if(index == 'RDVI'){
      # Roujean and Breon (1995)
      out <- (bNIR - bR) / sqrt(bNIR + bR)
    } else if(index == 'WDRVI'){
      out <- (0.1 * bNIR - bR) / (0.1 * bNIR + bR)
    } else if(index == 'CI_Green'){
      # Gitelson et al, (2003)
      out <- bNIR / bG - 1
    } else if(index == 'MCARI_Hab'){
      # Haboudane et al.(2004)
      num <- 1.5 * (2.5 * (bNIR - bR) - 1.3 * (bNIR - bG))
      deo <- sqrt(2 * (bNIR + 1)^2 + 6 * bNIR + 5 * sqrt(bR) - 0.5)
      out <- num / deo
    } else if(index == 'MTVI2'){
      num <- 1.5 * (1.2 * (bNIR - bG) - 2.5 * (bR - bG))
      deo <- sqrt(2 * (bNIR + 1)^2 + 6 * bNIR + 5 * sqrt(bR) - 0.5)
      out <- num / deo
    } else if(index == 'MCARI2_wu'){
      r750 <- get_reflectance(spc, 750, weighted = weighted)
      r705 <- get_reflectance(spc, 750, weighted = weighted)
      num <- 1.5 * (2.5 * (r750 - r705) - 1.3 * (r750 - bG))
      deo <- sqrt(2 * (r750 + 1)^2 + 6 * r750 + 5 * sqrt(r705) - 0.5)
      out <- num / deo
    } else if (index == "l0") {
      out <- rededge(spc)$l0
    } else if (index == "lp") {
      out <- rededge(spc)$lp
    } else if (index == "ls") {
      out <- rededge(spc)$ls
    } else if (index == "R0") {
      out <- rededge(spc)$R0
    } else if (index == "Rp") {
      out <- rededge(spc)$Rp
    } else if (index == "Rs") {
      out <- rededge(spc)$Rs
    }  else if (str_detect(index, "^opt_NDVI")) { # incase NRI calc by hsdar::nri
      # wls <- str_extract_all(index, '\\d+(.\\d+)?') %>% unlist() %>% as.numeric()
      wls <- str_split(index, "_")[[1]][c(3, 4)] %>% parse_double()
      if (length(wls) != 2) stop("wrong NDSI index name!!!")
      b1 <- get_reflectance(spc, wls[1], weighted = FALSE)
      b2 <- get_reflectance(spc, wls[2], weighted = FALSE)
      out <- (b2 - b1) / (b2 + b1)
    } else if (str_detect(index, "^opt_DVI")) { # incase NRI calc by hsdar::nri
      # wls <- str_extract_all(index, '\\d+(.\\d+)?') %>% unlist() %>% as.numeric()
      wls <- str_split(index, "_")[[1]][c(3, 4)] %>% parse_double()
      if (length(wls) != 2) stop("wrong NDSI index name!!!")
      b1 <- get_reflectance(spc, wls[1], weighted = FALSE)
      b2 <- get_reflectance(spc, wls[2], weighted = FALSE)
      out <- b2 - b1
    } else {
      msg <- glue::glue('Not defined index: {index}!!!')
      stop(msg)
    }
  }

  if (is.matrix(out)) {
    n <- dim(out)
    msg <- glue::glue('multicol of VI output.\n index:{index}\n dim:{n}')
    stop(n)
  }

  if (all(is.na(out))){
    msg <- glue::glue('NA output of index: {index}!!!')
    stop(msg)
  }

  return(out)
}
