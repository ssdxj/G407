#' model info for caret to do keras fitting
#'
#' @return list
#' @export
modelInfo_kerasL1 <- function(){
  modelInfo <- list()
  modelInfo$label <-  "Multilayer Perceptron Network with Weight Decay"
  modelInfo$library <- "keras"
  modelInfo$loop <- NULL
  modelInfo$type <- 'Regression'
  modelInfo$parameters = data.frame(
    parameter = c('size', 'lamda'),
    class = c('character', 'numeric'),
    label = c('#Hidden Units', 'L2 Regularization')
  )
  modelInfo$grid <- function(x, y, len = NULL, search = 'grid'){
    len <- ifelse(is.null(len), 10, len)
    expand.grid(
      size = seq(1, 2*len, by = 2),
      lamda =  c(0, 10 ^ seq(-1, -4, length = len - 1))
    )
  }

  modelInfo$fit <- function(x, y, wts, param, lev, last, classProbs, ...) {
    require(dplyr)
    K <- keras::backend()
    K$clear_session()
    if(!is.matrix(x)) x <- as.matrix(x)
    if(is.data.frame(y)) x <- as.matrix(y)
    model <- keras::keras_model_sequential()

    model %>%
      keras::layer_dense(
        units = param$size,
        activation = 'relu',
        input_shape = ncol(x),
        kernel_initializer = keras::initializer_glorot_uniform(seed = 666),
        kernel_regularizer = keras::regularizer_l2(param$lambda)
        )

    model %>%
      keras::layer_dense(
        units = 1,
        activation = 'linear',
        kernel_regularizer = keras::regularizer_l2(param$lambda)
        )

    model %>%
      compile(loss = "mse", optimizer = 'adam')

    model %>% keras::fit(
      x = x,
      y = y,
      verbose = 1,
      batch_size = nrow(x)
      )

    if(last) model <- keras::serialize_model(model)
    list(object = model)
  }


  modelInfo$predict <- function(modelFit, newdata, submodels = NULL) {
    if(inherits(modelFit$object, "raw"))
      modelFit$object <- keras::unserialize_model(modelFit$object)
    if(!is.matrix(newdata))
      newdata <- as.matrix(newdata)
    out <- predict(modelFit$object, newdata)
    # check for model type
    if(ncol(out) == 1) {
      out <- out[, 1]
    } else {
      stop(sprintf('stop: ncol of pred value is: %s!!!', ncol(out)))
    }
    return(out)
  }

  modelInfo$prob <-  function(modelFit, newdata, submodels = NULL) {
    if(inherits(modelFit$object, "raw"))
      modelFit$object <- keras::unserialize_model(modelFit$object)
    if(!is.matrix(newdata))
      newdata <- as.matrix(newdata)
    out <- predict(modelFit$object, newdata)
    colnames(out) <- modelFit$obsLevels
    as.data.frame(out)
    }
  modelInfo$varImp <- NULL
  modelInfo$tags <- c("Neural Network", "L2 Regularization")
  modelInfo$sort <- function(x) x[order(x$size, -x$lambda),]

  return(modelInfo)
}
