#Compute per-feature mean/sd for continuous (non-categorical) features so that
#model inputs can be standardized before fitting and reconstructed
#consistently for prediction. Categorical features are dummy-encoded and
#are left untouched.
compute_feature_scaling <- function(data,
                                    feature_names,
                                    categorical_features) {
  continuous_features <- setdiff(feature_names, categorical_features)
  if (length(continuous_features) == 0) {
    return(NULL)
  }
  scaling <- lapply(continuous_features, function(feature) {
    x <- as.numeric(data[, feature])
    feature_mean <- mean(x)
    feature_sd <- stats::sd(x)
    if (!is.finite(feature_sd) || feature_sd == 0) {
      feature_sd <- 1
    }
    list(mean = feature_mean, sd = feature_sd)
  })
  names(scaling) <- continuous_features
  scaling
}
#Standardize a single feature's values using stored scaling parameters.
#Returns `x` unchanged if the feature has no stored scaling (categorical
#features, or scaling disabled).
scale_feature <- function(x, feature, feature_scaling) {
  if (is.null(feature_scaling) || !feature %in% names(feature_scaling)) {
    return(x)
  }
  (x - feature_scaling[[feature]]$mean) / feature_scaling[[feature]]$sd
}
#Compute mean/sd of a continuous outcome for reversible output scaling.
compute_output_scaling <- function(y) {
  y_mean <- mean(y)
  y_sd <- stats::sd(y)
  if (!is.finite(y_sd) || y_sd == 0) {
    y_sd <- 1
  }
  list(mean = y_mean, sd = y_sd)
}
#Standardize outcome values before fitting. Returns `y` unchanged if
#`output_scaling` is `NULL` (scaling disabled, or non-continuous target).
scale_output <- function(y, output_scaling) {
  if (is.null(output_scaling)) {
    return(y)
  }
  (y - output_scaling$mean) / output_scaling$sd
}
#Transform fitted effects back from standardized outcome scale to original
#outcome scale.
backtransform_effects <- function(feature_effects, output_scaling) {
  if (is.null(output_scaling)) {
    return(feature_effects)
  }
  feature_effects <- feature_effects * output_scaling$sd
  if ("Intercept" %in% colnames(feature_effects)) {
    feature_effects[, "Intercept"] <- feature_effects[, "Intercept"] +
      output_scaling$mean
  }
  feature_effects
}
#Transform vector of total predictions back to original outcome scale.
#See `backtransform_effects()` for details.
backtransform_predictions <- function(predictions, output_scaling) {
  if (is.null(output_scaling)) {
    return(predictions)
  }
  predictions * output_scaling$sd + output_scaling$mean
}
