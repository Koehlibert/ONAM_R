.onLoad <- function(libname, pkgname) {
  if (!reticulate::py_module_available("tensorflow")) {
    packageStartupMessage(
      "TensorFlow backend not detected.\n",
      "Please run keras3::install_keras() before using {", pkgname, "}."
    )
  }
}
