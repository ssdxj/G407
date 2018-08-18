# common routines -----------------------------------------------------------------------------
modelInfo_kerasL2 <- list()
modelInfo_kerasL2$label <- "keras"
modelInfo_kerasL2$library <- "keras"
modelInfo_kerasL2$loop <- NULL
modelInfo_kerasL2$type <- "Regression"
modelInfo_kerasL2$prob <- function() {}
modelInfo_kerasL2$varImp <- NULL
modelInfo_kerasL2$notes <- "note"
modelInfo_kerasL2$check <- function(pkg) {
  testmod <- try(keras::keras_model_sequential(), silent = TRUE)
  if (inherits(testmod, "try-error")) {
    stop("Could not start a sequential model. ",
      "`tensorflow` might not be installed. ",
      "See `?install_tensorflow`.",
      call. = FALSE
    )
  }
  TRUE
}

# define sort ---------------------------------------------------------------------------------
modelInfo_kerasL2$sort <- function(x) x[order(x$units01, x$units02), ]

# define parameters ---------------------------------------------------------------------------
modelInfo_kerasL2$parameters <- data.frame(
  parameter = c("units01", "units02"),
  class = c("numeric", "numeric"),
  label = c("#Hidden Units in Layer1", "#Hidden Units in Layer1")
)

modelInfo_kerasL2$grid <- function(x, y, len = NULL, search = "grid") {
  if(is.null(len)) len <- 10

  expand.grid(
    units01 = ((1:len) * 2) - 1,
    units02 = c(0, ((1:len) * 2) - 1)
  )
}

# define fit ----------------------------------------------------------------------------------
modelInfo_kerasL2$fit <- function(x, y, wts, param, lev, last, classProbs, ...) {
  library(dplyr)
  K <- keras::backend()
  K$clear_session()

  # pre data
  if (!is.matrix(x)) x <- as.matrix(x)
  y <- unlist(y)

  # define model
  # place holder
  model <- keras::keras_model_sequential()
  # layer hidden 01
  model %>%
    keras::layer_dense(
      units = param$units01,
      kernel_initializer = keras::initializer_glorot_normal(seed = 666),
      activation = "relu",
      input_shape = ncol(x)
    )

  # layer hidden 02
  if (param$units02 != 0) {
    model %>%
      keras::layer_dense(
        units = param$units02,
        kernel_initializer = keras::initializer_glorot_normal(seed = 666),
        activation = "relu"
      )
  }

  # layer output
  model %>%
    keras::layer_dense(units = 1, activation = "linear")

  # model compile
  model %>%
    keras::compile(loss = "mae", optimizer = "adam")

  # model fit
  model %>%
    keras::fit(
      x = x,
      y = y,
      batch_size = floor(nrow(x) / 3),
      epochs = 100,
      verbose = 0
    )

  # python/R trick
  if (last) model <- keras::serialize_model(model)
  list(object = model)
}

# define predict ------------------------------------------------------------------------------
modelInfo_kerasL2$predict <- function(modelFit, newdata, submodels = NULL) {
  # python/R trick
  if (inherits(modelFit$object, "raw")) {
    modelFit$object <- keras::unserialize_model(modelFit$object)
  }

  # ready data
  newdata <- as.matrix(newdata)

  # do preicit
  out <- predict(modelFit$object, newdata)

  ## check for model type
  if (ncol(out) == 1) {
    out <- out[, 1]
  } else {
    stop("caret predict error!!!")
  }

  out
}
