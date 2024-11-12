#' @title install_tensorflow
#'
#' @description This function installs a python virtual environment.
#'
#' @param envname Name of the environment.
#'
#' @return No return value, used for creating a python virtual environment.
#'
#' @export

install_tensorflow = function(envname = "r-tensorflow"){

  # Function install_tensorflow() to create a python virtual environment

  reticulate::py_install("tensorflow", envname = envname)

}
