
numpy = NULL
scipy = NULL

.onLoad = function(libname, pkgname) {

  numpy <<- reticulate::import("numpy", delay_load = TRUE)
  scipy <<- reticulate::import("scipy", delay_load = TRUE)

}

