#' @title install_tensorflow
#'
#' @description This function installes a python virtual environment.
#'
#' @param envname Name of the environment.
#'
#' @export

install_tensorflow = function(envname = "r-tensorflow"){

  # Function install_tensorflow() to create a python virtual environment

  reticulate::py_install("tensorflow", envname = envname, ...)

}
