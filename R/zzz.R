
numpy = NULL
scipy = NULL

.onLoad = function(libname, pkgname) {

  welcome.message = cat("Welcome to the R package VecDep!")

  numpy <<- reticulate::import("numpy", delay_load = TRUE)
  scipy <<- reticulate::import("scipy", delay_load = TRUE)

  suppressPackageStartupMessages(welcome.message)

}

