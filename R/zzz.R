
numpy = NULL
scipy = NULL

.onLoad = function(libname, pkgname) {

  numpy <<- reticulate::import("numpy", delay_load = TRUE)
  scipy <<- reticulate::import("scipy", delay_load = TRUE)

  if(!interactive() || stats::runif(1) > 0.1)
    return()

  welcome.message = paste0("Welcome to the R package VecDep!")

  packageStartupMessage(welcome.message)

}

