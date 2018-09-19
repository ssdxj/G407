#' Transform Speclib to data.frame (spectral columns name is in '450' format)
#'
#' @param spc: 'speclib' obj
#'
#' @return 'data.frame' obj
#' @export
spc_2df <- function(spc) {
  out <- NULL
  attri <- SI(spc)
  ref <- spectra(spc)

  # incase no attri
  if (ncol(attri) == 0) {
    out <- as.tibble(ref)
  } else {
    out <- as.tibble(cbind(attri, ref))
  }

  # handle colnames
  names(out) <- c(names(attri), hsdar::wavelength(spc))

  return(out)
}


#' transform Speclib into data.frame (spectral columns name is in 'B_450' format)
#'
#' @param spc: 'speclib' obj
#'
#' @return 'data.frame' obj
#' @export
spc_2dfB <- function(spc) {
  out <- NULL
  attri <- SI(spc)
  ref <- spectra(spc)

  # incase no attri
  if (ncol(attri) == 0) {
    out <- as.tibble(ref)
  } else {
    out <- as.tibble(cbind(attri, ref))
  }

  # handle colnames
  names(out) <- c(names(attri), paste("B", wavelength(spc), sep = "_"))

  return(out)
}


#' generate 'Speclib' obj from data.frame, reflectance selected by matches
#' colnames matches('^(\\d)+(\\.\\d+)?$'),wl determined by colnames of spectral data
#'
#' @param df data.frame of data
#' @param bands_reg with default '^(\\d)+(\\.\\d+)?$'
#'
#' @return Speclib obj
#' @export
spc_fromDf <- function(df, bands_reg = "^(\\d)+(\\.\\d+)?$") {
  # do select
  spectra <- dplyr::select(df, matches(bands_reg))
  attri <- dplyr::select(df, -matches(bands_reg)) %>% data.frame()

  # handle spc
  wl <- as.double(names(spectra))
  spc <- speclib(as.matrix(spectra), wl)
  SI(spc) <- as.data.frame(attri)

  return(spc)
}


#' rbind spc
#'
#' @param ... spc
#'
#' @return spc obj
#' @export
spc_rbind <- function(...) {
  out <- NULL

  spc_list <- list(...)
  wl_list <- map(spc_list, wavelength)
  wl_check <- map_lgl(wl_list, all.equal, wl_list[[1]]) %>% all()

  if (wl_check) {
    spc_df_list <- map(spc_list, spc_2df)
    spc_df <- do.call(rbind, spc_df_list)
    out <- spc_fromDf(spc_df)
    return(out)
  } else {
    stop("wavelength not match, stop!!")
  }
}


#' get inTrain spc by index vector
#'
#' @param spc: spc to be filtered
#' @param inTrain: inTrain index vector
#'
#' @return spc obj
#' @export
#'
spc_inTrain <- function(spc, inTrain) {
  spc_fromDf(spc_2df(spc)[inTrain, ])
}

#' melt the reflectance part of spc df
#'
#' @param x spc or spc_df
#' @param band_reg with default '^(\\d)+(\\.\\d+)?$'
#'
#' @return tibble
#' @export
#'
spc_melt <- function(x, band_reg = "^(\\d)+(\\.\\d+)?$") {
  if (is.speclib(x)) x <- spc_2df(x)

  x %>%
    gather(key = "wl", value = "reflect", matches(band_reg), convert = TRUE) %>%
    as.tibble()
}


#' Self use function. Average spc SampleID or PlotID or Treatment.
#'
#' @param spc: spc obj
#' @param by: SampleID/PlotID/Treatment/SampleDate
#' \enumerate{
#'   \item SampleID: group_by(spc_df_melt, FieldID, SampleDate, PlotID, SampleID, wl)
#'   \item PlotID: group_by(spc_df_melt, FieldID, SampleDate, PlotID, wl)
#'   \item Treatment: group_by(spc_df_melt, FieldID, SampleDate, Treatment, wl)
#'   \item SampleDate: group_by(spc_df_melt, FieldID, SampleDate, wl)
#' }
#'
#' @return spc obj
#' @export
#'
spc_ave <- function(spc, by = "SampleID") {
  # melt
  spc_df_melt <- spc_melt(spc)
  out <- NULL

  if (by == "SampleID") {
    out <- group_by(spc_df_melt, FieldID, SampleDate, PlotID, SampleID, wl)
  } else if (by == "PlotID") {
    out <- group_by(spc_df_melt, FieldID, SampleDate, PlotID, wl)
  } else if (by == "Treatment") {
    out <- group_by(spc_df_melt, FieldID, SampleDate, Treatment, wl)
  } else if (by == "SampleDate") {
    out <- group_by(spc_df_melt, FieldID, SampleDate, wl)
  } else {
    stop("Error in parameter value!!!")
  }

  # handle the longitude and latitude
  if (all(c("longitude", "latitude") %in% names(df))) {
    out <- out %>%
      summarise(
        longitude = mean(longitude, na.rm = TRUE),
        latitude = mean(latitude, na.rm = TRUE),
        reflect = mean(reflect, na.rm = TRUE)
      ) %>%
      ungroup() %>%
      spread(wl, reflect)
  } else {
    out <- out %>%
      summarise(reflect = mean(reflect, na.rm = TRUE)) %>%
      ungroup() %>%
      spread(wl, reflect)
  }

  out <- spc_fromDf(out)
  return(out)
}


#' Bandwise cor with responser
#'
#' @param spc  spc
#' @param biochemphy name of responsor in SI
#'
#' @return df with col ('wl', 'estimate', 'p.value')
#' @export
spc_cor <- function(spc, biochemphy) {
  df <- spc_2df(spc) %>%
    dplyr::select(matches("\\d+")) %>%
    map(cor.test, SI(spc)[[biochemphy]]) %>%
    map_df(function(fit) {
      fit[c("estimate", "p.value")]
    }, .id = "wl") %>%
    mutate(wl = parse_double(wl))

  names(df) <- c("wl", "estimate", "pvalue")
  return(df)
}


#' wrapper of \code{\link{spc_cor}} doing bandwise band cor with responser
#' grouped by stage
#'
#' @param stageValue levels of stage in SI
#' @param spc spc
#' @param biochemphy name of reponser in SI
#'
#' @return df
#' @export
spc_cor_stage <- function(stageValue, spc, biochemphy) {
  if (stageValue == "full") {
    spc_cor(spc, biochemphy)
  } else {
    spc_2df(spc) %>%
      dplyr::filter(stage %in% stageValue) %>%
      spc_fromDf() %>%
      spc_cor(biochemphy)
  }
}
