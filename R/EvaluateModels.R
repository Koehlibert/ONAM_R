#' @importFrom rlang .data
evaluateSingleModel <- function(PHOModel, data, modelInfoList,
                                categorical_features)
{
  fitData <- prepareData(data, modelInfoList,
                                categorical_features)
  modelIdxList <- getModelIdxList(modelInfoList)
  U_Object <-
    getU(PHOModel$modelList, modelIdxList, modelInfoList, fitData)
  subModelPredictions <-
    lapply(PHOModel$W_List,
           function(W)
             U_Object$U %*% W)
  subModelPredictionsOld <-
    lapply(PHOModel$W_List_old,
           function(W)
             U_Object$U %*% W)
  modelNames <- list()
  for(orderIdx in seq_along(modelInfoList$theta))
  {
    for(modelIdx in seq_along(modelInfoList$theta[[orderIdx]]))
    {
      tmpName <-
        paste(unlist(modelInfoList$theta[[orderIdx]][[modelIdx]]),
              collapse = "_")
      if(names(modelInfoList$theta)[orderIdx] == "Linear")
      {
        tmpName <- paste(tmpName, "_Linear", sep = "")
      }
      modelNames <- c(modelNames, tmpName)
    }
  }
  names(subModelPredictions) <- modelNames
  names(subModelPredictionsOld) <- modelNames
  return(list(subModelPredictions = subModelPredictions,
              subModelPredictionsOld = subModelPredictionsOld))
}
#' Evaluate orthogonal neural additive model
#' @param PHOModelList Orthogonal neural additive model ensemble object to be evaluated
#' @param data Data for which the model is to be evaluated
#' @returns Returns a list containing data, model output for each observation in `data` and main and interaction effects obtained by the model
#' @export evaluateModel
evaluateModel <- function(PHOModelList,
                          data = PHOModelList$data)
{
  data <- PHOModelList$data
  modelInfoList <- PHOModelList$modelInfoList
  n <- nrow(data)
  nEnsemble <- length(PHOModelList$PHOEnsemble)
  categorical_features <- PHOModelList$categorical_features
  separatePredictions <-
    lapply(PHOModelList$PHOEnsemble, evaluateSingleModel,
           data = data, modelInfoList = modelInfoList,
           categorical_features = categorical_features)
  effectNames <- names(separatePredictions[[1]][[1]])
  nEffects <- length(effectNames)
  predictionsData <-
    data.frame(y = unlist(separatePredictions),
               Effect =
                 rep(rep(effectNames,
                         each = n),
                     2 * nEnsemble),
               PHO = rep(rep(c("After", "Before"),
                             each = n * nEffects),
                         nEnsemble),
               Observation = rep(1:n, nEffects * 2 * nEnsemble),
               Model = rep(1:nEnsemble, each = n * 2 * nEffects))
  totalFeaturePredsPost <-
    lapply(effectNames,
           function(effect)
           {
             predictionsData %>%
               dplyr::filter(.data$PHO == "After", .data$Effect == effect) %>%
               dplyr::group_by(.data$Observation) %>%
               dplyr::summarise(totalEffect = mean(.data$y)) %>%
               dplyr::select(.data$totalEffect) %>% unlist()
           })
  names(totalFeaturePredsPost) <-
    effectNames
  finalTotalPredictions <-
    unlist(totalFeaturePredsPost) %>%
    matrix(nrow = n) %*%
    PHOModelList$finalW
  colnames(finalTotalPredictions) <-
    effectNames
  totalPredictions <-
    predictionsData %>%
    dplyr::filter(PHO == "After") %>%
    dplyr::group_by(.data$Observation) %>%
    dplyr::summarise(Prediction = sum(.data$y)/nEnsemble) %>%
    dplyr::select(.data$Prediction) %>% unlist()
  return(list(data = data,
              totalPredictions = totalPredictions,
              finalTotalPredictions = finalTotalPredictions))
}
evaluateModelGenericPre <- function(PHOModelList)
{
  data <- PHOModelList$data
  modelInfoList <- PHOModelList$modelInfoList
  categorical_features <- PHOModelList$categorical_features
  n <- nrow(data)
  nEnsemble <- length(PHOModelList$PHOEnsemble)
  separatePredictions <-
    lapply(PHOModelList$PHOEnsemble, evaluateSingleModel,
           data = data, modelInfoList = modelInfoList,
           categorical_features = categorical_features)
  effectNames <- names(separatePredictions[[1]][[1]])
  nEffects <- length(effectNames)
  predictionsData <-
    data.frame(y = unlist(separatePredictions),
               Effect =
                 rep(rep(effectNames,
                         each = n),
                     2 * nEnsemble),
               PHO = rep(rep(c("After", "Before"),
                             each = n * nEffects),
                         nEnsemble),
               Observation = rep(1:n, nEffects * 2 * nEnsemble),
               Model = rep(1:nEnsemble, each = n * 2 * nEffects))
  totalFeaturePredsPost <-
    lapply(effectNames,
           function(effect)
           {
             predictionsData %>%
               dplyr::filter(.data$PHO == "After", .data$Effect == effect) %>%
               dplyr::group_by(.data$Observation) %>%
               dplyr::summarise(totalEffect = mean(.data$y)) %>%
               dplyr::select(.data$totalEffect) %>% unlist()
           })
  names(totalFeaturePredsPost) <-
    effectNames
  totalPredictions <-
    predictionsData %>%
    dplyr::filter(PHO == "After") %>%
    dplyr::group_by(.data$Observation) %>%
    dplyr::summarise(Prediction = sum(.data$y)/nEnsemble) %>%
    dplyr::select(.data$Prediction) %>% unlist()
  totalFeaturePredsPre <-
    lapply(effectNames,
           function(effect)
           {
             predictionsData %>%
               dplyr::filter(.data$PHO == "Pre", .data$Effect == effect) %>%
               dplyr::group_by(.data$Observation) %>%
               dplyr::summarise(totalEffect = mean(.data$y)) %>%
               dplyr::select(.data$totalEffect) %>% unlist()
           })
  names(totalFeaturePredsPre) <-
    effectNames
  return(list(data = data,
              totalPredictions = totalPredictions,
              predictionsData = predictionsData,
              totalFeaturePredsPost = totalFeaturePredsPost,
              totalFeaturePredsPre = totalFeaturePredsPre))
}
evaluateModelSimulation <- function(PHOModelList, X_Big, Y)
{
  modelInfoList <- PHOModelList$modelInfoList
  n <- nrow(X_Big)
  nEnsemble <- length(PHOModelList$PHOEnsemble)
  separatePredictions <-
    lapply(PHOModelList$PHOEnsemble, evaluateSingleModel,
           data = X_Big, modelInfoList = modelInfoList)
  effectNames <- names(separatePredictions[[1]][[1]])
  nEffects <- length(effectNames)
  predictionsData <-
    data.frame(y = unlist(separatePredictions),
               Effect =
                 rep(rep(effectNames,
                         each = n),
                     2 * nEnsemble),
               PHO = rep(rep(c("After", "Before"),
                             each = n * nEffects),
                         nEnsemble),
               Observation = rep(1:n, nEffects * 2 * nEnsemble),
               Model = rep(1:nEnsemble, each = n * 2 * nEffects))
  totalFeaturePredsPost <-
    lapply(effectNames,
           function(effect)
           {
             predictionsData %>%
               dplyr::filter(.data$PHO == "After",
                             .data$Effect == effect) %>%
               dplyr::group_by(.data$Observation) %>%
               dplyr::summarise(totalEffect = mean(.data$y)) %>%
               dplyr::select(.data$totalEffect) %>% unlist()
           })
  names(totalFeaturePredsPost) <-
    effectNames
  finalTotalPredictions <-
    unlist(totalFeaturePredsPost) %>%
    matrix(nrow = n) %*%
    PHOModelList$finalW
  colnames(finalTotalPredictions) <-
    effectNames
  totalPredictions <-
    predictionsData %>%
    dplyr::filter(.data$PHO == "After") %>%
    dplyr::group_by(.data$Observation) %>%
    dplyr::summarise(Prediction = sum(.data$y)/nEnsemble) %>%
    dplyr::select(.data$Prediction) %>% unlist()
  totalFeaturePredsPre <-
    lapply(effectNames,
           function(effect)
           {
             predictionsData %>%
               dplyr::filter(.data$PHO == "Pre", .data$Effect == effect) %>%
               dplyr::group_by(.data$Observation) %>%
               dplyr::summarise(totalEffect = mean(.data$y)) %>%
               dplyr::select(.data$totalEffect) %>% unlist()
           })
  names(totalFeaturePredsPre) <-
    effectNames
  Var <- stats::var(PHOModelList$finalOutputs)
  totalVar <- sum(Var)
  resVar <- Var[1,1]
  interVar <- sum(Var[2:4, 2:4])
  mainVar <- sum(Var[5:7,5:7])
  interVarPercent <- interVar/totalVar
  mainVarPercent <- mainVar/totalVar
  interpretPercent <- mainVarPercent + interVarPercent
  interpretMeasureList <- list(mainVarPercent,
                               interVarPercent,
                               interpretPercent)
  return(list(data = cbind(X_Big, Y),
              totalPredictions = totalPredictions,
              predictionsData = predictionsData,
              totalFeaturePredsPost = totalFeaturePredsPost,
              totalFeaturePredsPre = totalFeaturePredsPre,
              finalTotalPredictions = finalTotalPredictions,
              interpretMeasures = interpretMeasureList))
}
evaluateModelNewData <- function(PHOModelList, X_Big)
{
  modelInfoList <- PHOModelList$modelInfoList
  n <- nrow(X_Big)
  nEnsemble <- length(PHOModelList$PHOEnsemble)
  separatePredictions <-
    lapply(PHOModelList$PHOEnsemble, evaluateSingleModel,
           data = X_Big, modelInfoList = modelInfoList)
  effectNames <- names(separatePredictions[[1]][[1]])
  nEffects <- length(effectNames)
  predictionsData <-
    data.frame(y = unlist(separatePredictions),
               Effect =
                 rep(rep(effectNames,
                         each = n),
                     2 * nEnsemble),
               PHO = rep(rep(c("After", "Before"),
                             each = n * nEffects),
                         nEnsemble),
               Observation = rep(1:n, nEffects * 2 * nEnsemble),
               Model = rep(1:nEnsemble, each = n * 2 * nEffects))
  totalFeaturePredsPost <-
    lapply(effectNames,
           function(effect)
           {
             predictionsData %>%
               dplyr::filter(.data$PHO == "After",
                             .data$Effect == effect) %>%
               dplyr::group_by(.data$Observation) %>%
               dplyr::summarise(totalEffect = mean(.data$y)) %>%
               dplyr::select(.data$totalEffect) %>% unlist()
           })
  names(totalFeaturePredsPost) <-
    effectNames
  finalTotalPredictions <-
    unlist(totalFeaturePredsPost) %>%
    matrix(nrow = n) %*%
    PHOModelList$finalW
  colnames(finalTotalPredictions) <-
    effectNames
  totalPredictions <-
    predictionsData %>%
    dplyr::filter(.data$PHO == "After") %>%
    dplyr::group_by(.data$Observation) %>%
    dplyr::summarise(Prediction = sum(.data$y)/nEnsemble) %>%
    dplyr::select(.data$Prediction) %>% unlist()
  totalFeaturePredsPre <-
    lapply(effectNames,
           function(effect)
           {
             predictionsData %>%
               dplyr::filter(.data$PHO == "Pre",
                             .data$Effect == effect) %>%
               dplyr::group_by(.data$Observation) %>%
               dplyr::summarise(totalEffect = mean(.data$y)) %>%
               dplyr::select(.data$totalEffect) %>% unlist()
           })
  names(totalFeaturePredsPre) <-
    effectNames
  Var <- stats::var(PHOModelList$finalOutputs)
  totalVar <- sum(Var)
  resVar <- Var[1,1]
  interVar <- sum(Var[2:4, 2:4])
  mainVar <- sum(Var[5:7,5:7])
  interVarPercent <- interVar/totalVar
  mainVarPercent <- mainVar/totalVar
  interpretPercent <- mainVarPercent + interVarPercent
  interpretMeasureList <- list(mainVarPercent,
                               interVarPercent,
                               interpretPercent)
  return(list(data = cbind(X_Big),
              totalPredictions = totalPredictions,
              predictionsData = predictionsData,
              totalFeaturePredsPost = totalFeaturePredsPost,
              totalFeaturePredsPre = totalFeaturePredsPre,
              finalTotalPredictions = finalTotalPredictions,
              interpretMeasures = interpretMeasureList))
}
