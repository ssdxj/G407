# global import -----------------------------------------------------------
#' @import ggplot2
#' @import purrr
#' @import tibble
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @import readr
#' @import forcats
#' @import caret
#' @import hsdar
#' @import ggpubr
#' @import ggsci
#' @import furrr
#' @import minpack.lm

library(ggplot2)
library(ggpubr)
library(ggsci)
library(tidyverse)


# common var --------------------------------------------------------------

index2factorMeta <- list(
  # G1
  rep('SR', times = 3),
  rep('NDVI', times = 3),
  rep('DVI', times = 3),
  rep('SAVI', times = 3),
  rep('MSAVI', times = 3),
  c('GreenNDVI', 'Green~NDVI', 'Green\\ NDVI'),
  c('Green NDVI', 'Green~NDVI', 'Green\\ NDVI'),
  rep('OSAVI', times = 3),
  rep('WDRVI', times = 3),

  # G2
  c('CI_green', 'CI[green]', 'CI_{green}'),
  c('CI_Green', 'CI[green]', 'CI_{green}'),
  c('CI_705', 'CI[705]', 'CI_{705}'),
  c('CI_740', 'CI[740]', 'CI_{740}'),
  c('CI_783', 'CI[783]', 'CI_{783}'),
  c('NDVI_705', 'NDVI[705]', 'NDVI_{705}'),
  c('NDVI_740', 'NDVI[740]', 'NDVI_{740}'),
  c('NDVI_783', 'NDVI[783]', 'NDVI_{783}'),
  c('DVI_705', 'DVI[705]', 'DVI_{705}'),
  c('DVI_740', 'DVI[740]', 'DVI_{740}'),
  c('DVI_783', 'DVI[783]', 'DVI_{783}'),
  rep('TVI', times = 3),
  rep('MTVI2', times = 3),
  rep('MCARI2', times = 3),
  c('MCARI2_re', 'MCARI2[re]', 'MCARI2_{re}'),

  # G3
  rep('MTCI', times = 3),
  rep('lp', times = 3),
  rep('mREIP', times = 3),
  c('Sum_Dr2', 'Sum[Dr2]', 'Sum_{Dr2}'),
  c('REP_Li', 'REP[Li]', 'REP_{Li}'),
  c('REP_LE', 'REP[LE]', 'REP_{LE}')
)

index_levels <- map_chr(index2factorMeta, 1)
index_plotmath <- map_chr(index2factorMeta, 2)
index_latex <- map_chr(index2factorMeta, 3)

# color palette-----------------------------------------------------------------
# colorblind-friendly palette
# from http://jfly.iam.u-tokyo.ac.jp/color/

#' The palette with grey:
#'
#' @return chr list
#' @export
paletteCb <- function(){
  c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442",
               "#0072B2", "#D55E00", "#CC79A7")
}

#' The palette with black:
#'
#' @return chr list
#' @export
paletteCbb <- function(){
  c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442",
                "#0072B2", "#D55E00", "#CC79A7")
}

#' heatmap palette
#'
#' @return function generate by colorRampPalette
#' @export
paletteHm <- function(){
  colorRampPalette(rev(RColorBrewer::brewer.pal(11, 'Spectral')), space = 'Lab')
}

#' palette Jetter
#'
#' @return function generate by colorRampPalette
#' @export
paletteJetter <- function(){
  colorRampPalette(c("#00007F","blue","#007FFF","cyan","#7FFF7F",
                                    "yellow","#FF7F00","red","#7F0000"))
}

#' theme for dotchart plot
#' @return ggplot theme
#' @export
themeDotplot <- function(){
  theme_bw(14) +
  theme(
    axis.text.y = element_text(size = rel(.75)),
    axis.ticks.y = element_blank(),
    axis.title.x = element_text(size = rel(.75)),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(size = 0.5),
    panel.grid.minor.x = element_blank()
  )
}



#' not in
#'
#' @param x x
#' @param table vector
#'
#' @return vector
#' @export
`%not in%` <- function(x, table) {
  is.na(match(x, table, nomatch = NA_integer_))
  }


#' df to html table using kable
#'
#' @param df data.frame
#'
#' @return html
#' @export
df2html <- function(df, digits) {
  library(kableExtra)
  df %>%
    mutate_if(is.numeric, round, digits) %>%
    kable(format = "html", digits = digits, escape = FALSE) %>%
    kable_styling(bootstrap_options = c("striped", "hover"))
}
