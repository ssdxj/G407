#' modelInfo for Keras L1
#'
#' @return modelInfo
#' @export
modelInfo_kerasL1 <- function() {
  modelInfo <- caret::getModelInfo('mlpKerasDecay')$mlpKerasDecay
  # define sort ---------------------------------------------------------------------------------
  modelInfo$sort <- function(x) x[order(x$units01), ]

  # define parameters ---------------------------------------------------------------------------
  modelInfo$parameters <- data.frame(
    parameter = c("units01"),
    class = c("numeric"),
    label = c("#Hidden Units in Layer1")
  )

  modelInfo$grid <- function(x, y, len = NULL, search = "grid") {
      expand.grid(units01 = 1:len)
  }


  modelInfo$fit <- function(x, y, wts, param, lev, last, classProbs, ...) {
      require(dplyr)
      K <- keras::backend()
      K$clear_session()
      if (!is.matrix(x)) x <- as.matrix(x)

      model <- keras::keras_model_sequential()
      model %>%
        keras::layer_dense(
          units = param$units01,
          activation = "relu",
          kernel_initializer = keras::initializer_glorot_uniform(),
          kernel_regularizer = keras::regularizer_l2(),
          input_shape = ncol(x)
        )

      model %>%
        keras::layer_dense(
          units = 1,
          kernel_regularizer = keras::regularizer_l2(),
          activation = "linear"
          )

      model %>%
        keras::compile(
          loss = "mean_squared_error",
          metrics = "mean_squared_error",
          optimizer = "adam"
          )


      model %>%
        keras::fit(
          x = x,
          y = y,
          batch_size = nrow(x),
          epochs = 100,
          verbose = 1
          )

      if (last)  model <- keras::serialize_model(model)
      list(object = model)
  }

  return(modelInfo)
}
