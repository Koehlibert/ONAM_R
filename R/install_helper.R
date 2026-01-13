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
#' @param overwrite Should an existing conda environment of name `envname` be
#' overwritten if present?
#'
#' @return No return value, called for side effects
#' @seealso [keras3::install_keras()]
#' @export
install_conda_env <- function(
    envname = "r-keras",
    python_version = "python=3.10",
    overwrite = FALSE
) {
  conda_bin <- reticulate::conda_binary()
  envs <- reticulate::conda_list(conda = conda_bin)$name

  if (envname %in% envs) {

    if (!overwrite) {
      if (interactive()) {
        ans <- readline(
          paste0(
            "Conda environment '", envname,
            "' already exists. Overwrite it? [y/N]: "
          )
        )
        overwrite <- tolower(ans) %in% c("y", "yes")
      }
    }

    if (!overwrite) {
      message("Using existing conda environment: ", envname)
      return(invisible(FALSE))
    }

    message("Removing existing conda environment: ", envname)
    reticulate::conda_remove(envname, conda = conda_bin)
  }

  message("Creating conda environment: ", envname)
  reticulate::conda_create(
    envname,
    packages = python_version,
    conda = conda_bin
  )

  reticulate::conda_install(
    envname,
    packages = c("tensorflow", "keras"),
    conda = conda_bin
  )

  reticulate::use_condaenv(envname, required = TRUE)

  reticulate::py_run_string(
    "import sys, tensorflow as tf, keras;
     print(sys.version);
     print(tf.__version__);
     print(keras.__version__)"
  )

  invisible(TRUE)
}
