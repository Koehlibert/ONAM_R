.onLoad <- function(libname, pkgname) {
  if (!reticulate::py_module_available("tensorflow")) {
    packageStartupMessage(
      "TensorFlow backend not detected.\n",
      "Please run keras::install_keras() before using {", ONAM, "}."
    )
  }
}
