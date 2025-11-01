#' Set up conda environment for keras functionality
#'
#' @description
#' Helper function to install Keras and packages necessary for package
#' functionality into a conda environment. Use this function if
#' [keras3::install_keras()] does not work, esp. on windows machines.
#'
#'
#' @param envname Name for the conda environment to be created.
#' @param python_version Python version to be installed in the conda environment.
#'
#' @return No return value, called for side effects
#' @seealso [keras3::install_keras()]
#' @export
install_conda_env <- function(envname = "r-keras",
                           python_version = "python=3.10") {
  reticulate::conda_create(envname, packages = python_version)
  reticulate::use_condaenv(envname, required = TRUE)

  reticulate::conda_install(envname, packages = c("tensorflow", "keras"))

  reticulate::py_run_string(
    "import sys, tensorflow as tf, keras; print(sys.version); print(tf.__version__); print(keras.__version__)"
  )
}
