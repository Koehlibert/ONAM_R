#Define linear Model DNN structure
get_linear_submodel <- function(inputs) {
  outputs <- inputs %>%
    keras3::layer_dense(
      units = 1,
      activation = "linear",
      use_bias = FALSE,
      dtype = "float64"
    )
  keras3::keras_model(inputs, outputs)
}
#Define model architecture for categorical variable
get_categorical_submodel <- function(inputs) {
  outputs <- inputs %>%
    keras3::layer_dense(
      units = 1,
      activation = "linear",
      use_bias = TRUE,
      dtype = "float64"
    )
  keras3::keras_model(inputs, outputs)
}
#Derive Theta from model Formula
get_theta <-
  function(model_formula,
           list_of_deep_models,
           feature_names,
           categorical_features,
           target) {
    #Separate Symbols
    all_feature_indic <- FALSE
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
        if (length(tmp_item) > 1) {
          if (!is.list(tmp_item[[2]])) {
            if (as.character(tmp_item[[2]]) == ".") {
              all_feature_indic <- TRUE
              tmp_item[[2]] <- NULL
              outcome_idx <- which(feature_names == outcome_var)
              tmp_item <-
                if (length(outcome_idx) > 0) {
                  c(tmp_item, feature_names[-outcome_idx])
                } else {
                  c(tmp_item, feature_names)
                }
            }
          }
        }
      }
      parts_list <- c(parts_list, list(tmp_item))
      theta_list[[idx_to_remove]] <- NULL
    }
    idx_sign <- which(lapply(parts_list, function(item) {
      all(lapply(item, function(sub_item) {
        sub_item == as.name("+") |
          sub_item == as.name("~") |
          sub_item == as.name(outcome_var)
      }) %>%
        unlist())
    }) %>% unlist())
    parts_list[idx_sign] <- NULL
    check_inputs_formula(
      parts_list,
      list_of_deep_models,
      feature_names,
      categorical_features,
      outcome_var
    )
    order_inter <-
      lapply(parts_list, function(item)
        length(item) - 1) %>%
      unlist()
    order_highest <- max(order_inter)
    ordered_parts_list <-
      parts_list[(1:length(parts_list))[order(order_inter, decreasing = TRUE)]]
    order_counts <- table(order_inter)
    order_counts <-
      order_counts[order(as.numeric(names(order_counts)), decreasing = TRUE)]
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
      lapply(seq_along(theta_deep_name), function(idx_item) {
        lapply(seq_along(theta_deep_name[[idx_item]]), function(idx_item_in) {
          lapply(seq_along(theta_deep_name[[idx_item]][[idx_item_in]]), function(idx_item_in_2) {
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
    theta <- theta_deep
    list(
      theta = theta,
      name_models = model_list,
      categorical_features = categorical_features,
      outcome = outcome_var,
      target = target,
      all_feature_indic = all_feature_indic,
      model_formula = model_formula
    )
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
                         cat_counts,
                         target = "continuous",
                         learning_rate) {
  feature_inputs <-
    create_feature_inputs(model_info, categorical_features, cat_counts)
  model_list <-
    create_models(model_info, feature_inputs, list_of_deep_models)
  model_whole <-
    compile_model(feature_inputs,
                  model_list,
                  target = target,
                  learning_rate = learning_rate)
  list(model = model_whole, model_list = model_list)
}
#extract the character feature names referenced by a single theta term
get_term_feature_names <- function(theta_sub) {
  vapply(theta_sub, function(feature)
    as.character(unlist(feature)), character(1))
}
#nested list (same shape as model_info$theta) of character vectors holding
#the feature names used by each fitted term
get_all_term_feature_names <- function(model_info) {
  lapply(model_info$theta, function(theta_order) {
    lapply(theta_order, get_term_feature_names)
  })
}
#create one keras Input per raw feature referenced in theb model formula, so
#that features shared between several terms (e.g. main and interaction effects)
#are fed into the model only once.
create_feature_inputs <- function(model_info,
                                  categorical_features,
                                  cat_counts) {
  feature_names <-
    get_all_term_feature_names(model_info) %>% unlist() %>% unique()
  inputs <- lapply(feature_names, function(feature_name) {
    n_inputs <- if (feature_name %in% names(cat_counts)) {
      cat_counts[[feature_name]]
    } else {
      1
    }
    keras3::layer_input(shape = n_inputs, name = feature_name)
  })
  names(inputs) <- feature_names
  inputs
}
#create all submodels, reusing shared per-feature inputs. Terms that reference
#more than one feature receive a list of the relevant shared input tensors; the
#deep model constructor combines them while still passing the original input
#tensor(s) to `keras3::keras_model()`, so that the submodel's own `$input` stays
#the feedable Input tensor(s).
create_models <-
  function(model_info,
           feature_inputs,
           list_of_deep_models) {
    term_feature_names <- get_all_term_feature_names(model_info)
    models <- list()
    for (idx_p in
         names(model_info$theta)) {
      models[[idx_p]] <-
        lapply(seq_along(model_info$name_models[[idx_p]]), function(idx_model) {
          name_model <- model_info$name_models[[idx_p]][[idx_model]]
          feature_names <- term_feature_names[[idx_p]][[idx_model]]
          term_inputs <- unname(feature_inputs[feature_names])
          if (length(term_inputs) == 1) {
            term_inputs <- term_inputs[[1]]
          }
          return(list_of_deep_models[[name_model]](term_inputs))
        })
    }
    models
  }
#concatenate models in model_list
concatenate_model_list <-
  function(model_list,
           bias = FALSE,
           activation = "linear") {
    concatenated <-
      keras3::layer_concatenate(lapply(model_list, function(model)
        model$output))
    dense_layer <-
      keras3::layer_dense(
        units = 1,
        use_bias = bias,
        trainable = FALSE,
        activation = activation,
        dtype = "float64"
      )
    output <- dense_layer(concatenated)
    tmp_weights <- dense_layer$get_weights()
    tmp_weights[[1]] <- matrix(rep(1, length(model_list)), ncol = 1)
    if (bias) {
      tmp_weights[[2]] <- tmp_weights[[2]] - tmp_weights[[2]]
    }
    dense_layer$set_weights(tmp_weights)
    output
  }
#compile created pho ensemble member
compile_model <-
  function(feature_inputs,
           model_list,
           target = "continuous",
           learning_rate) {
    submodels <-
      unlist(model_list, use.names = FALSE) #Why does this matter?
    all_inputs <- unname(feature_inputs)
    target_activation <-
      if (target == "continuous") {
        "linear"
      } else {
        "sigmoid"
      }
    model_whole <- submodels %>%
      concatenate_model_list(bias = TRUE, activation = target_activation)
    model_whole <- keras3::keras_model(all_inputs, model_whole)
    loss <- if (target == "continuous") {
      keras3::loss_mean_squared_error()
    } else if (target == "binary") {
      keras3::loss_binary_crossentropy()
    } else {
      stop()
    }
    model_whole %>%
      keras3::compile(
        loss = loss,
        optimizer = keras3::optimizer_adam(learning_rate = learning_rate)
      )

  }
#prepare data for model fitting: builds one array per raw feature referenced in
#model formula. A feature used in several terms is only ever extracted/copied
#once; `fit()`/`predict()` route the shared array to every submodel that needs
#it based on this name.
prepare_data <- function(data_original,
                         model_info,
                         categorical_features) {
  feature_names <-
    get_all_term_feature_names(model_info) %>% unlist() %>% unique()
  data_list <- lapply(feature_names, function(feature) {
    if (feature %in% categorical_features) {
      encode_dummy(data_original[, feature], feature)
    } else {
      matrix(data_original[, feature], ncol = 1)
    }
  })
  names(data_list) <- feature_names
  data_list
}
#extract the data needed to call `predict()`/`fit()` on a single term's
#submodel: a single array when the term uses one feature (matching a
#single Input tensor), or a named list of arrays (matching a list of
#shared Input tensors) when the term uses several.
get_term_data <- function(data, feature_names) {
  if (length(feature_names) == 1) {
    data[[feature_names]]
  } else {
    data[feature_names]
  }
}
#Generate a display/column name for a single effect.
get_effect_name <- function(features, max_order) {
  order <- length(features)
  if (order == max_order) {
    paste0("Interaction ", order)
  } else {
    paste(features, collapse = "_")
  }
}
#Generate display names for every effect in one order group (an element
#of model_info$theta).
get_effect_names_for_order <- function(theta_order, max_order) {
  vapply(theta_order, function(effect) {
    get_effect_name(unlist(effect), max_order)
  }, character(1))
}
#Generate display names for every fitted effect, in the same order
#model_info$theta is iterated in evaluate_onam_single().
get_all_effect_names <- function(model_info) {
  max_order <- max(as.numeric(names(model_info$theta)))
  c(unlist(lapply(model_info$theta, get_effect_names_for_order,
                  max_order = max_order)),
    "Intercept")
}
#Create "dictionary" yielding, for each term in the nested model list, the
#character vector of (shared) feature names its submodel was built from.
get_data_dictionary <- function(model_info) {
  get_all_term_feature_names(model_info)
}
#create dummy variables for categorical data
encode_dummy <- function(x, name) {
  uq_vals <- unique(x)
  ret_mat <- matrix(sapply(uq_vals[-1], function(val) {
    x == val
  }), nrow = length(x))
  colnames(ret_mat) <- paste(name, uq_vals[-1], sep = "_")
  ret_mat
}
#get number of unique categories per categorical feature - 1 (because of dummy
#encoding)
get_category_counts <- function(categorical_features, data) {
  ret_list <-
    lapply(categorical_features, function(feature) {
      length(unique(data[, feature])) - 1
    })
  names(ret_list) <- categorical_features
  ret_list
}
get_output <-
  function(model,
           prediction_function,
           model_data,
           data,
           target,
           model_info) {
    if (!is.null(model)) {
      if (is.null(prediction_function)) {
        for (tmp_class in class(model)) {
          prediction_function <-
            utils::getS3method("predict", tmp_class, optional = TRUE)
          if (!is.null(prediction_function))
            break
        }
        if (is.null(prediction_function)) {
          stop(
            paste0(
              "Model of class ",
              class(model),
              " supplied without `prediction_function`, but no S3 method for class ",
              class(model),
              "exists. Please specify a prediction function that returns a vector of predictions."
            ),
            call. = FALSE
          )
        }
      }
      if (is.null(model_data)) {
        model_data <- data
      }
      y <- prediction_function(model, model_data)
      if (inherits(y, "factor")) {
        if (is.null(target)) {
          warning("Prediction function supplied factor. Binary outcome is assumed.")
          target <- "binary"
          y <- as.numeric(as.character(y))
        } else if (target != "binary") {
          stop(
            paste0(
              "Prediction function supplied factor, but target was supplied as ",
              target,
              "."
            ),
            call. = FALSE
          )
        } else {
          y <- as.numeric(as.character(y))
        }
      }
      if (!is.vector(y)) {
        if ("predictions" %in% names(y)) {
          y <- y[["predictions"]]
        } else {
          stop(
            "Prediction function does not return an appropriate outcome. Please specify a prediction function that returns a vector of predictions.",
            call. = FALSE
          )
        }
      }

    } else {
      outcome_index <- which(colnames(data) ==
                               as.character(model_info$outcome))
      if (length(outcome_index) != 1) {
        stop(
          paste0(
            "Outcome ",
            model_info$outcome,
            " specified in model formula, but not present in data."
          ),
          call. = FALSE
        )
      } else {
        y <- data[, outcome_index]
      }
    }
    y
  }
