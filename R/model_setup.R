#' @import keras
#Define DNN architecture for simulation
get_submodel <- function(inputs, regularizer = NULL) {
  outputs <- inputs %>%
    # layer_dense(units = 512, activation = "relu",
    #             use_bias = TRUE) %>%
    # layer_dropout(rate = 0.2) %>%
    # layer_dense(units = 256, activation = "relu",
    #             use_bias = TRUE,
    #             kernel_regularizer = regularizer) %>%
    # # layer_dropout(rate = 0.2) %>%
    keras::layer_dense(
      units = 128,
      activation = "relu",
      use_bias = TRUE,
      kernel_regularizer = regularizer
    ) %>%
    keras::layer_dropout(rate = 0.2) %>%
    keras::layer_dense(
      units = 64,
      activation = "relu",
      use_bias = TRUE,
      kernel_regularizer = regularizer,
      dtype = "float64"
    ) %>%
    # layer_dropout(rate = 0.2) %>%
    keras::layer_dense(
      units = 32,
      activation = "relu",
      use_bias = TRUE,
      kernel_regularizer = regularizer,
      dtype = "float64"
    ) %>%
    # layer_dropout(rate = 0.2) %>%
    keras::layer_dense(
      units = 16,
      activation = "relu",
      use_bias = TRUE,
      kernel_regularizer = regularizer,
      dtype = "float64"
    ) %>%
    keras::layer_dense(
      units = 8,
      activation = "linear",
      use_bias = TRUE,
      kernel_regularizer = regularizer,
      dtype = "float64"
    ) %>%
    keras::layer_dense(units = 1,
                       activation = "linear",
                       use_bias = TRUE)
  keras::keras_model(inputs, outputs)
}
#Define linear Model DNN structure
get_linear_submodel <- function(inputs) {
  outputs <- inputs %>%
    keras::layer_dense(
      units = 1,
      activation = "linear",
      use_bias = FALSE,
      dtype = "float64"
    )
  keras::keras_model(inputs, outputs)
}
#Define model architecture for categorical variable
get_categorical_submodel <- function(inputs) {
  outputs <- inputs %>%
    keras::layer_dense(
      units = 1,
      activation = "linear",
      use_bias = TRUE,
      dtype = "float64"
    )
  keras::keras_model(inputs, outputs)
}
#Derive Theta from model Formula
get_theta <- function(model_formula, list_of_deep_models) {
  #Separate Symbols
  theta_list <- lapply(model_formula, find_symbol)
  outcome_var <- theta_list[[2]][[1]]
  parts_list <- list()
  while (length(theta_list) > 0) {
    tmp_item <- theta_list[[length(theta_list)]]
    idx_to_remove <- c(length(theta_list))
    while (length(tmp_item) > 1 &
           any(unlist(lapply(tmp_item, is.list)))) {
      idx_to_remove <- c(idx_to_remove, length(tmp_item))
      tmp_item <- tmp_item[[length(tmp_item)]]
    }
    parts_list <- c(parts_list, list(tmp_item))
    theta_list[[idx_to_remove]] <- NULL
  }
  idx_sign <- which(lapply(parts_list, function(item) {
    all(lapply(item, function(sub_item) {
      sub_item == as.name("+") |
        sub_item == as.name("~") | sub_item == as.name(outcome_var)
    }) %>%
      unlist())
  }) %>% unlist())
  parts_list[idx_sign] <- NULL
  idx_additive_comps <- which(lapply(parts_list, function(item) {
    any(lapply(item, function(sub_item) {
      sub_item == as.name("+")
    }) %>%
      unlist()) | length(item) == 1
  }) %>% unlist())
  additive_comps_long <- parts_list[idx_additive_comps]
  parts_list[idx_additive_comps] <- NULL
  additive_comps <- lapply(additive_comps_long, function(item) {
    if (length(item) == 1) {
      item
    } else {
      item[which(lapply(item,
                        function(sub_item)
                          sub_item != as.name("+")) %>%
                   unlist())]
    }
  }) %>% unlist() %>% unique()
  order_inter <-
    lapply(parts_list, function(item)
      length(item) - 1) %>%
    unlist()
  order_highest <- max(order_inter)
  ordered_parts_list <-
    parts_list[(1:length(parts_list))[order(order_inter,
                                            decreasing = TRUE)]]
  order_counts <- table(order_inter)
  order_counts <-
    order_counts[order(as.numeric(names(order_counts)),
                       decreasing = TRUE)]
  idx_counts <- list()
  start_index <- 1
  for (i in seq_along(order_counts)) {
    end_index <- start_index + order_counts[i] - 1
    seq_list <- start_index:end_index
    idx_counts[[i]] <- seq_list
    start_index <- end_index + 1
  }
  orders_unique <- as.numeric(names(order_counts))
  theta_deep_name <-
    lapply(seq_along(orders_unique), function(countIdx)
      ordered_parts_list[idx_counts[[countIdx]]])
  idx_names <-
    lapply(seq_along(theta_deep_name),
           function(idx_item) {
             lapply(seq_along(theta_deep_name[[idx_item]]),
                    function(idx_item_in) {
                      lapply(seq_along(theta_deep_name[[idx_item]][[idx_item_in]]),
                             function(idx_item_in_2) {
                               if (as.character(theta_deep_name[[idx_item]][[idx_item_in]][[idx_item_in_2]])
                                   %in% names(list_of_deep_models)) {
                                 return(c(idx_item, idx_item_in, idx_item_in_2))
                               }
                             })
                    })
           }) %>%
    unlist()
  idx_names <-
    idx_names %>% split(rep(1:(length(idx_names) / 3), each = 3))
  theta_deep <- theta_deep_name
  model_list <- list()
  for (item in idx_names) {
    name = as.character(theta_deep[[item]])
    theta_deep[[item]] <- NULL
    if (length(model_list) < item[1]) {
      model_list[[item[1]]] <- list()
    }
    if (length(model_list[[item[1]]]) < item[2]) {
      model_list[[item[1]]][[item[2]]] <- name
    } else {
      model_list[[item[1]]][[item[2]]] <- c(model_list[[item[2]]], name)
    }
  }
  names(theta_deep) <- orders_unique
  names(model_list) <- orders_unique
  theta <- c(theta_deep, linear = list(additive_comps))
  list(theta = theta,
       name_models = model_list,
       outcome = outcome_var)
}
#help function to detect symbols
find_symbol <- function(list_current) {
  lapply(list_current, function(item) {
    if (is.symbol(item)) {
      item
    } else {
      find_symbol(item)
    }
  })
}
#create ONAM model based on model info and deep model list
create_model <- function(model_info,
                         list_of_deep_models,
                         categorical_features,
                         cat_counts) {
  inputs_list <- create_inputs(model_info,
                               categorical_features, cat_counts)
  model_list <-
    create_models(model_info, inputs_list, list_of_deep_models)
  model_whole <- compile_model(inputs_list, model_list)
  list(model = model_whole,
       model_list = model_list)
}
#create inputs for each submodel
create_inputs <- function(model_info,
                          categorical_features,
                          cat_counts) {
  if (length(model_info$theta$linear) > 0) {
    inputs_linear <- lapply(1:length(model_info$theta$linear),
                            function(x)
                              keras::layer_input(shape = 1))
  } else {
    inputs_linear <- NULL
  }
  inputs_deep <-
    lapply(1:length(model_info$theta[setdiff(names(model_info$theta),
                                             "linear")]),
           function(idx_inter_count) {
             lapply(model_info$theta[[idx_inter_count]],
                    function(theta_sub) {
                      n_inputs <- length(theta_sub)
                      for (feature_name in theta_sub) {
                        if (as.character(feature_name) %in% names(cat_counts)) {
                          n_inputs <- n_inputs + cat_counts[[feature_name]] - 1
                        }
                      }
                      keras::layer_input(shape = n_inputs)
                    })
           })
  names(inputs_deep) <-
    names(model_info$theta[setdiff(names(model_info$theta),
                                   "linear")])
  list(deep = inputs_deep,
       additive = inputs_linear)
}
#create all submodels
create_models <-
  function(model_info,
           inputs_list,
           list_of_deep_models) {
    models_linear <- lapply(inputs_list$additive,
                            get_linear_submodel)
    models_deep <- list()
    for (idx_p in
         names(model_info$theta[setdiff(names(model_info$theta),
                                        "linear")])) {
      models_deep[[idx_p]] <-
        lapply(seq_along(model_info$name_models[[idx_p]]),
               function(idx_model) {
                 name_model <- model_info$name_models[[idx_p]][[idx_model]]
                 return(list_of_deep_models[[name_model]](inputs_list$deep[[idx_p]][[idx_model]]))
               })
    }
    c(models_deep,
      additive = list(models_linear))

  }
#concatenate models in model_list
concatenate_model_list <- function(model_list, bias = FALSE) {
  tmp_output <-
    keras::layer_concatenate(lapply(model_list,
                                    function(model)
                                      model$output)) %>%
    keras::layer_dense(1, use_bias = bias, trainable = FALSE)
  tmp_weights <- tmp_output$node$layer$get_weights()
  tmp_weights[[1]] <- matrix(rep(1, length(model_list)),
                             ncol = 1)
  if (bias) {
    tmp_weights[[2]] <- tmp_weights[[2]] - tmp_weights[[2]]
  }
  tmp_output$node$layer %>% keras::set_weights(tmp_weights)
  tmp_output
}
#compile created pho ensemble member
compile_model <- function(inputs_list, model_list) {
  submodels <-
    unlist(model_list, use.names = FALSE) #Why does this matter?
  all_inputs <- unlist(inputs_list, use.names = FALSE)
  model_whole <- submodels %>%
    concatenate_model_list(bias = TRUE)
  model_whole <- keras::keras_model(all_inputs, model_whole)
  model_whole %>%
    keras::compile(
      loss = keras::loss_mean_squared_error(),
      optimizer = keras::optimizer_adam()
    )

}
#prepare data for model fitting by bringing it into the right input dimensions
prepare_data <- function(data_original,
                         model_info,
                         categorical_features) {
  data_additive <-
    lapply(model_info$theta$linear,
           function(feature_name) {
             data_original[, as.character(feature_name)]
           })
  deep_data <-
    lapply(unlist(model_info$theta[setdiff(names(model_info$theta),
                                           "linear")], recursive = FALSE),
           function(theta_sub) {
             ret_mat <- matrix(nrow = nrow(data_original), ncol = 0)
             for (feature in theta_sub) {
               feature <- as.character(unlist(feature))
               ret_mat <-
                 cbind(ret_mat,
                       if (feature %in% categorical_features) {
                         encode_dummy(data_original[, feature], feature)
                       } else {
                         data_original[, feature]
                       })
               if (!feature %in% categorical_features) {
                 colnames(ret_mat)[ncol(ret_mat)] <- feature
               }
             }
             return(ret_mat)
           })
  newData <- c(deep_data, data_additive)
  unname(newData)
}
#Create "dictionary" yielding data indices for nested model list
get_data_dictionary <- function(model_info) {
  dictionary_list <- list()
  idx_tmp <- 0
  for (i1 in seq_along(model_info$theta)) {
    dictionary_list[[i1]] <- list()
    for (i2 in seq_along(model_info$theta[[i1]])) {
      idx_tmp <- idx_tmp + 1
      dictionary_list[[i1]][[i2]] <- idx_tmp
    }
  }
  dictionary_list
}
#create dummy variables for categorical data
encode_dummy <- function(x, name) {
  uq_vals <- unique(x)
  ret_mat <- matrix(sapply(uq_vals[-1],
                           function(val) {
                             x == val
                           }),
                    nrow = length(x))
  colnames(ret_mat) <- paste(name, uq_vals[-1], sep = "_")
  ret_mat
}
#get number of unique categories per categorical feature - 1 (because of dummy
#encoding)
get_category_counts <- function(categorical_features,
                                data) {
  ret_list <-
    lapply(categorical_features,
           function(feature) {
             length(unique(data[, feature])) - 1
           })
  names(ret_list) <- categorical_features
  ret_list
}
