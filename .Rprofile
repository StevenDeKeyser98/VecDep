.onLoad = function(...) {
  install.packages("reticulate")
  reticulate::py_install("tensorflow", envname = "r-tensorflow", ...)
  reticulate::use_virtualenv("r-tensorflow", required = FALSE)
  numpy = reticulate::import("numpy")
  scipy = reticulate::import("scipy")
}
