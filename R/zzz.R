

install_tensorflow = function(..., envname = "r-tensorflow"){

  # Function install_tensorflow() to create a python virtual environment

  reticulate::py_install("tensorflow", envname = envname, ...)

}

# To create a Python virtual environment with "numpy" and "scipy",
# run:

# install_tensorflow()
# reticulate::use_virtualenv("r-tensorflow", required = FALSE)
# reticulate::py_install("numpy")
# reticulate::py_install("scipy")

numpy = NULL
scipy = NULL

.onLoad = function(libname, pkgname) {

  cat("Welcome to the R package VecDep!")

  # install_tensorflow()

  # Use the "r-tensorflow" environment

  # reticulate::use_virtualenv("r-tensorflow", required = FALSE)

  # Delay load foo module (will only be loaded when accessed via $)

  # reticulate::py_install("numpy")
  # reticulate::py_install("scipy")

  numpy <<- reticulate::import("numpy", delay_load = TRUE)
  scipy <<- reticulate::import("scipy", delay_load = TRUE)

}

