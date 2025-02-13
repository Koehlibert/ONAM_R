#Define DNN architecture for simulation
getSubModel <- function(inputs, regularizer = NULL)
{
  outputs <- inputs %>%
    # layer_dense(units = 512, activation = "relu",
    #             use_bias = TRUE) %>%
    # layer_dropout(rate = 0.2) %>%
    # layer_dense(units = 256, activation = "relu",
    #             use_bias = TRUE,
    #             kernel_regularizer = regularizer) %>%
    # # layer_dropout(rate = 0.2) %>%
    keras::layer_dense(units = 128, activation = "relu",
                       use_bias = TRUE,
                       kernel_regularizer = regularizer) %>%
    keras::layer_dropout(rate = 0.2) %>%
    keras::layer_dense(units = 64, activation = "relu",
                       use_bias = TRUE,
                       kernel_regularizer = regularizer,
                       dtype = "float64") %>%
    # layer_dropout(rate = 0.2) %>%
    keras::layer_dense(units = 32, activation = "relu",
                       use_bias = TRUE,
                       kernel_regularizer = regularizer,
                       dtype = "float64") %>%
    # layer_dropout(rate = 0.2) %>%
    keras::layer_dense(units = 16, activation = "relu",
                       use_bias = TRUE,
                       kernel_regularizer = regularizer,
                       dtype = "float64") %>%
    keras::layer_dense(units = 8, activation = "linear",
                       use_bias = TRUE,
                       kernel_regularizer = regularizer,
                       dtype = "float64") %>%
    keras::layer_dense(units = 1, activation = "linear",
                       use_bias = TRUE)
  subModel <- keras::keras_model(inputs, outputs)
  return(subModel)
}
#Define Linear Model DNN structure
getLinearSubModel <- function(inputs)
{
  outputs <- inputs %>%
    keras::layer_dense(units = 1, activation = "linear",
                       use_bias = FALSE,
                       dtype = "float64")
  subModel <- keras::keras_model(inputs, outputs)
}
#Define model architecture for categorical variable
getCategoricalSubModel <- function(inputs)
{
  outputs <- inputs %>%
    keras::layer_dense(units = 1, activation = "linear",
                       use_bias = TRUE,
                       dtype = "float64")
  subModel <- keras::keras_model(inputs, outputs)
}
#Derive Theta from model Formula
getThetaFromFormula <- function(modelFormula, list_of_deep_models)
{
  #Separate Symbols
  thetaList <- lapply(modelFormula, findSymbol)
  outcomeVar <- thetaList[[2]][[1]]
  partsList <- list()
  while(length(thetaList) > 0)
  {
    tmpItem <- thetaList[[length(thetaList)]]
    idxToRemove <- c(length(thetaList))
    while(length(tmpItem) > 1 & any(unlist(lapply(tmpItem, is.list))))
    {
      idxToRemove <- c(idxToRemove, length(tmpItem))
      tmpItem <- tmpItem[[length(tmpItem)]]
    }
    partsList <- c(partsList, list(tmpItem))
    thetaList[[idxToRemove]] <- NULL
  }
  signIdx <- which(lapply(partsList, function(item)
    all(lapply(item, function(subItem) subItem == as.name("+") |
                 subItem == as.name("~") | subItem == as.name(outcomeVar)) %>%
          unlist())) %>% unlist())
  partsList[signIdx] <- NULL
  additiveCompsIdx <- which(lapply(partsList, function(item)
    any(lapply(item, function(subItem) subItem == as.name("+")) %>%
          unlist()) | length(item) == 1) %>% unlist())
  LongAdditiveComps <- partsList[additiveCompsIdx]
  partsList[additiveCompsIdx] <- NULL
  additiveComps <- lapply(LongAdditiveComps, function(item)
    if(length(item) == 1) return(item) else
      return(item[which(lapply(item,
                               function(subItem) subItem != as.name("+")) %>%
                          unlist())])) %>% unlist() %>% unique()
  interOrder <- lapply(partsList, function(item) length(item) - 1) %>% unlist()
  highestOrder <- max(interOrder)
  orderedPartsList <-
    partsList[(1:length(partsList))[order(interOrder,
                                          decreasing = TRUE)]]
  orderCounts <- table(interOrder)
  orderCounts <- orderCounts[order(as.numeric(names(orderCounts)),
                                   decreasing = TRUE)]
  countIndex <- list()
  start_index <- 1
  for (i in seq_along(orderCounts)) {
    end_index <- start_index + orderCounts[i] - 1
    seq_list <- start_index:end_index
    countIndex[[i]] <- seq_list
    start_index <- end_index + 1
  }
  uniqueOrders <- as.numeric(names(orderCounts))
  thetaDeepWithName <- lapply(seq_along(uniqueOrders), function(countIdx)
    orderedPartsList[countIndex[[countIdx]]])
  namesIdx <- lapply(seq_along(thetaDeepWithName), function(itemIdx)
    lapply(seq_along(thetaDeepWithName[[itemIdx]]), function(innerItemIdx)
      lapply(seq_along(thetaDeepWithName[[itemIdx]][[innerItemIdx]]),
             function(innerInnerItemIdx)
               if(as.character(thetaDeepWithName[[itemIdx]][[innerItemIdx]][[innerInnerItemIdx]]) %in%
                  names(list_of_deep_models))
                 return(c(itemIdx, innerItemIdx, innerInnerItemIdx))))) %>%
    unlist() %>% split(rep(1:(length(.)/3), each = 3))
  thetaDeep <- thetaDeepWithName
  modelList <- list()
  for(item in namesIdx)
  {
    name = as.character(thetaDeep[[item]])
    thetaDeep[[item]] <- NULL
    if(length(modelList) < item[1])
      modelList[[item[1]]] <- list()
    if(length(modelList[[item[1]]]) < item[2])
      modelList[[item[1]]][[item[2]]] <- name else
        modelList[[item[1]]][[item[2]]] <- c(modelList[[item[2]]], name)
  }
  names(thetaDeep) <- uniqueOrders
  names(modelList) <- uniqueOrders
  theta <- c(thetaDeep, Linear = list(additiveComps))
  modelInfoList <- list(theta = theta,
                        modelNames = modelList,
                        outcome = outcomeVar)
  return(modelInfoList)
}
#help function to detect symbols
findSymbol <- function(currList)
{
  return(lapply(currList, function(item) if(is.symbol(item)) item else findSymbol(item)))
}
#create ONAM model based on model info and deep model list
createModel <- function(modelInfoList, list_of_deep_models,
                        categorical_features, cat_counts)
{
  inputsList <- createInputs(modelInfoList,
                                    categorical_features, cat_counts)
  modelList <- createModels(modelInfoList, inputsList, list_of_deep_models)
  wholeModel <- compileModel(inputsList, modelList)
  return(list(model = wholeModel,
              modelList = modelList))
}
#create inputs for each submodel
createInputs <- function(modelInfoList, categorical_features,
                         cat_counts)
{
  if(length(modelInfoList$theta$Linear) > 0)
    linInputs <- lapply(1:length(modelInfoList$theta$Linear),
                        function(x) keras::layer_input(shape = 1))
  else
    linInputs <- NULL
  deepInputs <-
    lapply(1:length(modelInfoList$theta[setdiff(names(modelInfoList$theta),
                                                "Linear")]),
           function(interCountIdx)
             lapply(modelInfoList$theta[[interCountIdx]],
                    function(subtheta)
                    {
                      n_inputs <- length(subtheta)
                      for(feature_name in subtheta)
                      {
                        if(as.character(feature_name) %in% names(cat_counts))
                          n_inputs <- n_inputs + cat_counts[[feature_name]] - 1
                      }
                      keras::layer_input(shape = n_inputs)
                    })
    )
  names(deepInputs) <-
    names(modelInfoList$theta[setdiff(names(modelInfoList$theta),
                                      "Linear")])
  return(list(Deep = deepInputs,
              Additive = linInputs))
}
#create all submodels
createModels <- function(modelInfoList, inputsList, list_of_deep_models)
{
  linModels <- lapply(inputsList$Additive,
                      getLinearSubModel)
  deepModels <- list()
  for(p_idx in
      names(modelInfoList$theta[setdiff(names(modelInfoList$theta),
                                        "Linear")]))
  {
    deepModels[[p_idx]] <-
      lapply(seq_along(modelInfoList$modelNames[[p_idx]]),
             function(modelIdx)
             {
               modelName <- modelInfoList$modelNames[[p_idx]][[modelIdx]]
               return(list_of_deep_models[[modelName]](inputsList$Deep[[p_idx]][[modelIdx]]))
             })
  }
  all_models <- c(deepModels,
                  Additive = list(linModels))
  return(all_models)
}
#concatenate models in modelList
concatenate_ModelList <- function(modelList, bias = FALSE)
{
  tmpOutput <-
    keras::layer_concatenate(lapply(modelList,
                                    function(model) model$output)) %>%
    keras::layer_dense(1, use_bias = bias, trainable = FALSE)
  tmpWeights <- tmpOutput$node$layer$get_weights()
  tmpWeights[[1]] <- matrix(rep(1, length(modelList)),
                            ncol = 1)
  if(bias) tmpWeights[[2]] <- tmpWeights[[2]] - tmpWeights[[2]]
  tmpOutput$node$layer %>% keras::set_weights(tmpWeights)
  return(tmpOutput)
}
#compile created pho ensemble member
compileModel <- function(inputsList, modelList)
{
  subModels <- unlist(modelList, use.names = FALSE) #Why does this matter?
  all_inputs <- unlist(inputsList, use.names = FALSE)
  wholeModel <- subModels %>%
    concatenate_ModelList(bias = TRUE)
  wholeModel <- keras::keras_model(all_inputs, wholeModel)
  wholeModel <- wholeModel %>%
    keras::compile(loss = keras::loss_mean_squared_error(),
                   optimizer = keras::optimizer_adam())
  return(wholeModel)
}
#prepare data for model fitting by bringing it into the right input dimensions
prepareData <- function(originalData, modelInfoList,
                        categorical_features)
{
  additiveData <-
    lapply(modelInfoList$theta$Linear,
           function(featureName)
             originalData[,as.character(featureName)])
  deepData <-
    lapply(unlist(modelInfoList$theta[setdiff(names(modelInfoList$theta),
                                              "Linear")], recursive = FALSE),
           function(subTheta)
           {
             ret_mat <- matrix(nrow = nrow(originalData), ncol = 0)
             for(feature in subTheta)
             {
               feature <- as.character(unlist(feature))
               ret_mat <-
                 cbind(ret_mat,
                       if(feature %in% categorical_features)
                         encode_dummy(originalData[,feature], feature)
                       else
                         originalData[,feature])
               if(!feature %in% categorical_features)
                 colnames(ret_mat)[ncol(ret_mat)] <- feature
             }
             return(ret_mat)
           })
  newData <- c(deepData, additiveData)
  newData <- unname(newData)
  return(newData)
}
#Create "dictionary" yielding data indices for nested model list
getDataDictionary <- function(modelInfoList)
{
  dictionaryList <- list()
  tmpIdx <- 0
  for(i1 in seq_along(modelInfoList$theta))
  {
    dictionaryList[[i1]] <- list()
    for(i2 in seq_along(modelInfoList$theta[[i1]]))
    {
      tmpIdx <- tmpIdx + 1
      dictionaryList[[i1]][[i2]] <- tmpIdx
    }
  }
  return(dictionaryList)
}
#create dummy variables for categorical data
encode_dummy <- function(x, name)
{
  uq_vals <- unique(x)
  ret_mat <- matrix(sapply(uq_vals[-1], function(val) x == val),
                    nrow = length(x))
  colnames(ret_mat) <- paste(name, uq_vals[-1], sep = "_")
  return(ret_mat)
}
#get number of unique categories per categorical feature - 1 (because of dummy encoding)
get_category_counts <- function(categorical_features,
                                data)
{
  ret_list <-
    lapply(categorical_features,
           function(feature)
             length(unique(data[,feature])) - 1)
  names(ret_list) <- categorical_features
  return(ret_list)
}
