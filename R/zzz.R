.onLoad = function(...) {
  install.packages("reticulate")
  cat("test")
  reticulate::py_install("tensorflow", envname = "r-tensorflow", ...)
  reticulate::use_virtualenv("r-tensorflow", required = FALSE)
  numpy = reticulate::import("numpy")
  scipy = reticulate::import("scipy")
}
