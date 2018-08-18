#' Hyperspectral raster calculation
#'
#' @param fpath_in input raster file path(in ENVI format)
#' @param fpath_out output file path(in .tif format)
#' @param wl input raster file wavelength
#' @param nbands nbands for output
#' @param fun do function, take hsdar::Speclib as first input param, each spectrum
#' is one pixel of fpath_in
#' @param format parameter for writeStart to control output format, default raster.
#' @param noData value for NA
#' @param ... futher param for fun
#'
#' @return Hyperspectral image obj(save output to fpath_out inviable)
#' @export

raster_calc <- function(fpath_in, fpath_out, wl,  nbands,  fun,  format = 'raster',
                        noData = -9999, ...){
  # incase
  library(hsdar)

  # input raster obj
  ra <- HyperSpecRaster(fpath_in, wl)
  NAvalue(ra) <- noData

  # output handler
  res <- writeStart(x = ra, filename = path_out, format = 'raster', nl = nbands,
                    overwrite = TRUE)

  # brick size
  tr <- blockSize(ra)

  # handle progress bar
  pb <- txtProgressBar(min = 0, max = tr$n, style = 3)

  # start loop
  for (i in 1:tr$n){
    # subtract image block into spc
    v <- getValuesBlock(ra, row = tr$row[i], nrows = tr$nrows[i])

    # main routine
    out <- fun(v, ...)

    # write to out file
    res <- raster::writeValues(res, out, tr$row[i])

    # handle progress bar
    setTxtProgressBar(pb, i)
  }

  res <- writeStop(res)

  # handle progress bar
  close(pb)

  # return result
  return(res)
}



#' FIXME: demo
#'
#' @param fpath_in fpath
#' @param title plot title
#' @param noData noDate value default -9999
#' @export
raster_levelplot <- function(fpath_in, title, noData = -9999){
  # load file and handle ha
  ra <- raster(fpath_in)
  NAvalue(ra) <- noData

  # ra <- aggregate(ra, fact = 10, fun = median, na.rm = TRUE)

  # get pixel value range
  ra.values <- na.omit(values(ra))
  ra.values.range <- range(ra.values)

  # get brk
  brk.test <- seq(0, 7, 0.5)
  col.test <- paletteJetter(length(brk.test) + 1)

  tag1 <- brk.test >= ra.values.range[1]
  tag2 <- brk.test <= ra.values.range[2]

  if(all(c(tag1, tag2))){
    # ra.values.range bigger than brk.test
    brk <- c(ra.values.range[1], brk.test, ra.values.range[2])
    col <- col.test
  } else if(all(tag1)) {
    # ra.values.ranges[1] < 0
    tag2 <- brk.test <= ra.values.range[2] + 0.4
    brk <- c(ra.values.range[1], brk.test[tag2])
    col <- c(col.test[c(tag2, FALSE)])
  } else if(all(tag2)){
    # ra.values.ranges[2] > 7
    tag1 <- brk.test >= ra.values.range[1] - 0.4
    brk <- c(brk.test[tag1], ra.values.range[2])
    col <- c(col.test[c(FALSE, tag1)])
  } else {
    # ra.values.range in [0, 7]
    tag1 <- brk.test >= ra.values.range[1] - 0.4
    tag2 <- brk.test <= ra.values.range[2] + 0.4
    brk <- brk.test[tag1 & tag2]
    col <- col.test[c(FALSE, tag1 & tag2)]
  }

  levelplot(
    ra, margin = FALSE,  col.regions = col, main = title, scales=list(draw=FALSE),
    at = brk,
    colorkey = list(
      at = brk,
      labels = list(
        labels = sprintf('%.2f', brk),
        at = brk
      )
    )
  )
}
