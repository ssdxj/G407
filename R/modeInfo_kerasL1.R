#' modelInfo for Keras L1
#'
#' @return modelInfo
#' @export
modelInfo_kerasL1 <- function() {
  modelInfo <- list()
  modelInfo$label <- "keras with one hidden layer tuning units"
  modelInfo$library <- "keras"
  modelInfo$loop <- NULL
  modelInfo$type <- "Regression"
  modelInfo$prob <- function() {}
  modelInfo$varImp <- NULL
  modelInfo$check <- function(pkg) {
    testmod <- try(keras::keras_model_sequential(), silent = TRUE)
    if (inherits(testmod, "try-error")) {
      stop(
        "Could not start a sequential model. ",
        "`tensorflow` might not be installed. ",
        "See `?install_tensorflow`.",
        call. = FALSE
      )
    }
    TRUE
  }


  # define sort ---------------------------------------------------------------------------------
  modelInfo$sort <- function(x) x[order(x$units01), ]

  # define parameters ---------------------------------------------------------------------------
  modelInfo$parameters <- data.frame(
    parameter = c("units01"),
    class = c("numeric"),
    label = c("#Hidden Units in Layer1")
  )

  modelInfo$grid <- function(x, y, len = NULL, search = "grid") {
      expand.grid(units01 = 1:10)
    }


  modelInfo$fit <- function(x, y, wts, param, lev, last, classProbs, ...) {
      require(dplyr)
      K <- keras::backend()
      K$clear_session()
      if (!is.matrix(x)) {
        x <- as.matrix(x)
      }
      y <- unlist(y)


      model <- keras::keras_model_sequential()
      model %>%
        keras::layer_dense(
          units = param$units01,
          kernel_initializer = keras::initializer_glorot_normal(seed = 666),
          kernel_regularizer = keras::regularizer_l1(),
          activation = "relu",
          input_shape = ncol(x)
        ) %>%
        keras::layer_dense(units = 1, activation = "linear")

      model %>%
        keras::compile(loss = "mae", optimizer = "adam")


      model %>%
        keras::fit(x = x, y = y, batch_size = nrow(x), epochs = 200, verbose = 0)

      if (last) {
        model <- keras::serialize_model(model)
      }
      list(object = model)
    }


  # define predict ------------------------------------------------------------------------------
  modelInfo$predict <- function(modelFit, newdata, submodels = NULL) {
      if (inherits(modelFit$object, "raw")) {
        modelFit$object <- keras::unserialize_model(modelFit$object)
      }

      newdata <- as.matrix(newdata)

      out <- predict(modelFit$object, newdata)

      ## check for model type
      if (ncol(out) == 1) {
        out <- out[, 1]
      } else {
        stop("caret predict error!!!")
      }

      out
    }

  return(modelInfo)
}
