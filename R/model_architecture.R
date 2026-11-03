#' Build a stacked dense (DNN) model constructor
#'
#' Returns a *function* `function(inputs) {...}` that builds a feed-forward
#' stack of `layer_dense()` calls on top of the `inputs` tensor it is
#' later called with, and returns the resulting `keras3::keras_model()`.
#'
#' Layers can be specified either as a simple vector of unit counts, or
#' as a data frame / tibble giving full per-layer control over
#' activation, use_bias, initializers, regularizers, layer name, etc.
#'
#' @param units Optional integer vector of layer sizes. Ignored if
#'   `layer_df` is supplied. Every layer uses `default_activation` as
#'   activation function, with `activation = "gelu"` as the default.
#'   `use_bias = TRUE`.
#' @param layer_df Optional data frame / tibble with one row per layer.
#'   Must contain a `units` column. May also contain any of:
#'   \itemize{
#'     \item `activation` (character)
#'     \item `use_bias` (logical)
#'     \item `kernel_initializer`, `bias_initializer` (character, or a
#'       list-column of initializer objects, e.g.
#'       `keras3::initializer_he_normal()`)
#'     \item `kernel_regularizer`, `bias_regularizer`,
#'       `activity_regularizer` (list-column; each element is either
#'       `NULL` or an object like `keras3::regularizer_l2(0.01)`)
#'     \item `name` (character, layer name)
#'   }
#'   Any column not supplied falls back to `layer_dense()`'s own default.
#'   Columns holding actual keras objects (regularizers, and
#'   initializers if you don't just want a string) must be *list-columns*
#'   so each row can carry a different object or NULL, e.g.:
#'   \preformatted{
#'   tibble::tibble(
#'     units = c(64, 32, 1),
#'     activation = c("gelu", "gelu", "linear"),
#'     kernel_regularizer = list(keras3::regularizer_l2(0.001), NULL, NULL)
#'   )
#'   }
#' @param default_activation Activation function to be used if not otherwise
#' specified in `layer_df`.
#'
#' @return A function of a single argument `inputs` (a keras input
#'   tensor) that returns a `keras3::keras_model()`.
#'
#' @examplesIf reticulate::py_module_available(tensorflow)
#' mod_units <- build_dnn(units = c(16, 8, 1))
#' mod_df <- build_dnn(layer_df = data.frame(
#'   units = c(32, 16, 8, 1),
#'   activation = c("gelu", "gelu", "gelu", "linear"),
#'   use_bias = c(FALSE, FALSE, FALSE, TRUE)
#' ))
#' @export
build_dnn <- function(units = NULL,
                      layer_df = NULL,
                      default_activation = NULL) {
  if (is.null(units) && is.null(layer_df)) {
    stop("Either `units` or `layer_df` must be supplied.")
  }
  if (!is.null(units) && !is.null(layer_df)) {
    stop("Supply only one of `units` or `layer_df`, not both.")
  }
  if (is.null(default_activation))
  {
    default_activation <- "gelu"
  }
  # --- Mode 1: plain units vector --------------------------------------
  if (!is.null(units)) {
    layer_df <- data.frame(
      units      = as.integer(units),
      activation = default_activation,
      use_bias   = TRUE,
      stringsAsFactors = FALSE
    )
  }

  if (!"units" %in% names(layer_df)) {
    stop("`layer_df` must contain a `units` column.")
  }
  if (layer_df$units[nrow(layer_df)] != 1)
  {
    warning(
      "Last layer must have exactly 1 output unit. An additional layer with 1 output unit was added."
    )
    layer_df <- rbind(layer_df, layer_df[-1, ])
    layer_df$units[nrow(layer_df)] <- 1
  }
  if (!is.null(units)) {
    layer_df$activation[nrow(layer_df)] <- "linear"
  }
  # --- fill in missing optional columns with sensible defaults --------
  scalar_defaults <- list(activation = default_activation, use_bias   = TRUE)
  # columns that may hold real keras objects -> always kept as list-columns
  object_cols <- c(
    "kernel_initializer",
    "bias_initializer",
    "kernel_regularizer",
    "bias_regularizer",
    "activity_regularizer"
  )

  n <- nrow(layer_df)

  for (col in names(scalar_defaults)) {
    if (!col %in% names(layer_df))
      layer_df[[col]] <- scalar_defaults[[col]]
  }
  for (col in object_cols) {
    if (!col %in% names(layer_df))
      layer_df[[col]] <- vector("list", n)
  }
  if (!"name" %in% names(layer_df))
    layer_df[["name"]] <- NA_character_
  if (layer_df$activation[nrow(layer_df)] != "linear")
  {
    warning("Last layer must have linear activation. Activation in last layer was set to linear.")
    layer_df$activation[nrow(layer_df)] <- "linear"
  }
  # helper: get row-i value whether column is a list-column or atomic
  get_val <- function(df, col, i) {
    if (!col %in% names(df))
      return(NULL)
    v <- df[[col]]
    if (is.list(v))
      v[[i]]
    else
      v[i]
  }

  # --- return model constructor -------------------------------------
  function(inputs) {
    # `inputs` is a list of shared Input tensors for a term that spans
    # several features (e.g. an interaction); combine them into a single
    # tensor to build the dense stack on, but keep the original `inputs`
    # (list or single tensor) for `keras_model()` below, so the submodel's
    # own `$input` stays the real, shared, feedable Input tensor(s) rather
    # than the (non-feedable) output of the concatenation.
    outputs <- if (is.list(inputs)) {
      keras3::layer_concatenate(inputs)
    } else {
      inputs
    }
    for (i in seq_len(n)) {
      args <- list(
        object     = outputs,
        units      = layer_df$units[[i]],
        activation = get_val(layer_df, "activation", i),
        use_bias   = get_val(layer_df, "use_bias", i)
      )

      for (col in object_cols) {
        val <- get_val(layer_df, col, i)
        if (!is.null(val) &&
            !(length(val) == 1 && all(is.na(val)))) {
          args[[col]] <- val
        }
      }

      args$dtype <- "float64"

      nm <- get_val(layer_df, "name", i)
      if (!is.null(nm) && !is.na(nm))
        args$name <- nm

      outputs <- do.call(keras3::layer_dense, args)
    }

    keras3::keras_model(inputs, outputs)
  }
}
