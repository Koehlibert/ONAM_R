.onAttach <- function(libname, pkgname) {
  # --- prevent automatic Python/conda setup ---
  options(reticulate.auto_configure = FALSE)
  options(reticulate.conda_auto_install = FALSE)

  # --- check availability WITHOUT initializing Python ---
  if (!reticulate::py_available(initialize = FALSE) ||
      !reticulate::py_module_available("tensorflow")) {
    packageStartupMessage(
      "TensorFlow backend not detected.\n",
      "Please run keras3::install_keras() before using {", pkgname, "}."
    )
  }
}
