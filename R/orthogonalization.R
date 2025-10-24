#Get submodel until penultimate layer
get_intermediate_model <-
  function(model,
           idx_layer = NULL) {
    if (is.null(idx_layer)) {
      idx_layer <- length(model$layers) - 1
    }
    deep_part_in <-
      keras3::get_layer(model, index = as.integer(idx_layer))
    keras3::keras_model(model$input,
                        deep_part_in$output)
  }
#Solve linear system after removing linear dependencies
solve_singular_matrix <- function(tmp_u, pivot) {
  tmp_u_reduced <- tmp_u[, pivot]
  tryCatch(
    tmp <- solve(t(tmp_u_reduced) %*% tmp_u_reduced),
    error = function(error)
    {
      pivot <<-
        solve_singular_matrix(tmp_u,
                              pivot[1:(length(pivot) - 1)])
    }
  )
  return(pivot)
}
#get list of all submodel indices in model_info$theta
get_model_idx_list <- function(model_info) {
  lapply(seq_along(model_info$theta),
         function(idx_order_inter)
           lapply(seq_along(model_info$theta[[idx_order_inter]]),
                  function(x)
                    c(idx_order_inter, x))) %>%
    unlist(recursive = FALSE)
}
#get penultimate output for all submodels
get_u <- function(model_list,
                  model_idx_list,
                  model_info,
                  data) {
  data_dictionary <- get_data_dictionary(model_info)
  get_bias_helper <- function(model) {
    utils::tail(model$layers, 1)[[1]]$use_bias
  }
  u_list <-
    lapply(seq_along(model_idx_list),
           function(idx) {
             input <- model_list[[model_idx_list[[idx]]]] %>%
               get_intermediate_model() %>%
               stats::predict(data[[data_dictionary[[model_idx_list[[idx]]]]]],
                              verbose = 0)
             if (get_bias_helper(model_list[[model_idx_list[[1]]]])) {
               input <- cbind(input, 1)
             }
             return(input)
           })
  u_dims <-
    lapply(seq_along(u_list),
           function(idx_model)
             ncol(u_list[[idx_model]]))
  u <- unlist(u_list) %>%
    matrix(ncol =
             sum(u_dims %>% unlist()))
  u <- cbind(u, 1)
  u_idx_list <- list(1:u_dims[[1]])
  for (idx in seq_along(u_dims)[-1]) {
    idx_last <- u_idx_list[[idx - 1]][u_dims[[idx - 1]]]
    u_idx_list[[idx]] <- (idx_last + 1):(idx_last + u_dims[[idx]])
  }
  list(u = u,
       u_dims = u_dims,
       u_idx_list = u_idx_list)

}
#get list of weights in last layer of each subfunction
get_w_list <- function(model_list,
                       model_idx_list,
                       model_info,
                       u_idx_list) {
  w_list_sep <- lapply(seq_along(model_idx_list),
                       function(idx) {
                         weights <- model_list[[model_idx_list[[idx]]]] %>%
                           keras3::get_layer(index = -1) %>%
                           keras3::get_weights()
                         return(unlist(weights))
                       })
  w <- c(unlist(w_list_sep), 0)
  lapply(seq_along(w_list_sep),
         function(idx_model) {
           tmp_w <- w
           tmp_w[-u_idx_list[[idx_model]]] <- 0
           return(tmp_w)
         })
}
#get model orders based on model index list
get_model_order <- function(model_idx_list) {
  lapply(model_idx_list,
         function(idx_model)
           idx_model[1]) %>% unlist()
}
#post hoc orthogonalization of fitted submodels
pho <- function(model_list, model_info, data_fit) {
  model_idx_list <- get_model_idx_list(model_info)
  u_object <-
    get_u(model_list, model_idx_list, model_info, data_fit)
  w_list <-
    get_w_list(model_list, model_idx_list, model_info,
               u_object$u_idx_list)
  w_list_old <- w_list
  model_order <- get_model_order(model_idx_list)
  # all_orders <- as.numeric(setdiff(names(model_info$theta), "linear"))
  # highest_order <- max(all_orders)
  #Iterate over interaction depth
  for (idx_ortho in 2:(length(model_info$theta) -
                       is.null(model_info$theta$Linear))) {
    if (length(model_info$theta) -
        is.null(model_info$theta$Linear) == 1) {
      break
    }
    list_idx_order_lower <- which(model_order >= idx_ortho)
    idx_rel <- u_object$u_idx_list[list_idx_order_lower] %>%
      unlist()
    tmp_u <- u_object$u
    tmp_u[,-idx_rel] <- 0
    tmp_u[, ncol(tmp_u)] <- 1
    h <- crossprod(tmp_u)
    qr_res <- qr(h)
    h_order <- qr_res$pivot[1:qr_res$rank]
    pivot <- solve_singular_matrix(tmp_u, h_order)
    tmp_u_reduced <- tmp_u[, pivot]
    tmp_inverse <- solve(t(tmp_u_reduced) %*% tmp_u_reduced)
    list_idx_order_higher <- which(model_order == (idx_ortho - 1))
    outputs_list <-
      lapply(list_idx_order_higher,
             function(idx_order_higher) {
               return(u_object$u %*%
                        w_list[[idx_order_higher]])
             })
    z_list <-
      lapply(outputs_list,
             function(outputs) {
               tmp_z <- tmp_inverse %*% t(tmp_u_reduced) %*%
                 outputs
               z <- rep(0, ncol(tmp_u))
               z[pivot] <- tmp_z
               z
             })
    for (idx_order_higher in seq_along(list_idx_order_higher)) {
      idx_model <- list_idx_order_higher[[idx_order_higher]]
      w_list[[idx_model]] <-
        w_list[[idx_model]] -
        z_list[[idx_order_higher]]
    }
    for (idx_order_lower in list_idx_order_lower) {
      tmp_weight_update <-
        rep(0, u_object$u_dims[[idx_order_lower]])
      for (idx_order_higher in seq_along(list_idx_order_higher)) {
        tmp_weight_update <-
          tmp_weight_update +
          z_list[[idx_order_higher]][u_object$u_idx_list[[idx_order_lower]]]
      }
      weight_update <- rep(0, ncol(tmp_u))
      weight_update[u_object$u_idx_list[[idx_order_lower]]] <-
        tmp_weight_update
      w_list[[idx_order_lower]] <-
        w_list[[idx_order_lower]] + weight_update
    }
  }
  allOutputMeans <-
    lapply(seq_along(model_idx_list),
           function(idx_model) {
             u_object$u %*%
               w_list[[idx_model]] %>% mean()
           })
  w_list <-
    lapply(seq_along(w_list),
           function(w_Idx) {
             tmp_w <- w_list[[w_Idx]]
             tmp_w[length(tmp_w)] <- if (w_Idx == 1)
               sum(unlist(allOutputMeans)[-1])
             else
               - allOutputMeans[[w_Idx]]
             tmp_w
           })
  list(
    model_list = model_list,
    w_list = w_list,
    u_dims = u_object$u_dims,
    w_list_old = w_list_old
  )
}
show_progress <- function(i, n_ensemble) {
  cat('\r', paste0("Fitting model ", i, " of ", n_ensemble))
  utils::flush.console()
}
#post hoc orthogonalization of fitted (and  separately orthogonalized) ensemble
#members
pho_ensemble <- function(data_model_eval, model_info) {
  n_ensemble <- max(data_model_eval$data_predictions$model)
  data <- data_model_eval$data
  n <- nrow(data)
  model_idx_list <- get_model_idx_list(model_info)
  model_order <- get_model_order(model_idx_list)
  n_models <- length(model_order)
  u <- unlist(data_model_eval$predictions_features_ensemble) %>%
    matrix(nrow = n)
  w <- diag(1, nrow = n_models)
  #Iterate over interaction depth
  for (idx_ortho in 2:(length(model_info$theta) -
                       is.null(model_info$theta$Linear))) {
    list_idx_order_lower <- which(model_order >= idx_ortho)
    tmp_u <- u
    tmp_u[,-list_idx_order_lower] <- 0
    h <- crossprod(tmp_u)
    qr_res <- qr(h)
    h_order <- qr_res$pivot[1:qr_res$rank]
    pivot <- solve_singular_matrix(tmp_u, h_order)
    tmp_u_reduced <- tmp_u[, pivot]
    tmp_inverse <- solve(t(tmp_u_reduced) %*% tmp_u_reduced)
    list_idx_order_higher <- which(model_order == (idx_ortho - 1))
    outputs_list <-
      lapply(list_idx_order_higher,
             function(idx_order_higher) {
               return(u %*% w[, idx_order_higher])
             })
    z_list <-
      lapply(outputs_list,
             function(outputs) {
               tmp_z <- tmp_inverse %*% t(tmp_u_reduced) %*% outputs
               z <- rep(0, n_models)
               z[pivot] <- tmp_z
               return(z)
             })
    for (idx_order_higher in seq_along(list_idx_order_higher)) {
      idx_model <- list_idx_order_higher[[idx_order_higher]]
      w[, idx_model] <-
        w[, idx_model] -
        z_list[[idx_order_higher]]
    }
    for (idx_order_lower in list_idx_order_lower) {
      tmp_weight_update <- 0
      for (idx_order_higher in seq_along(list_idx_order_higher)) {
        tmp_weight_update <-
          tmp_weight_update +
          z_list[[idx_order_higher]][idx_order_lower]
      }
      w[idx_order_lower, idx_order_lower] <-
        w[idx_order_lower, idx_order_lower] + tmp_weight_update
    }
  }
  outputs_post_ensemble <-
    u %*% w
  colnames(outputs_post_ensemble) <-
    names(data_model_eval$predictions_features_ensemble)
  list(w, outputs_post_ensemble)
}
#' @importFrom dplyr %>%
#' @export
NULL
