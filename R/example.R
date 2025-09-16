#' Get example script.
#'
#' @param name character string that is the name of the script
#'
#' @returns a character string which contains a full file location and name
#' @importFrom fs path_package
#' @export
#'
#' @examples
#' example_script("generic_script")
example_script = function(name) {
  fs::path_package(package = "autogradeR") |>
    paste0("/examples/", name, ".R")
}
