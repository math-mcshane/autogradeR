#' Validate a file.
#'
#' Checks whether file exists (error) and
#' check whether file is an R script (warning)
#'
#' @param filename a character string which is a file name
#'
#' @returns NULL
#' @export
#'
#' @examples
#' example_script("generic_script") |>
#'   file_validation()
file_validation = function(filename) {
  if (!base::file.exists(filename)) {
    stop("couldn't find file ", filename)
  }
  if (!tools::file_ext(x = filename) == "R") {
    warning("expecting *.R file, will try to proceed")
  }
}

#' Parse a script into a `tibble`.
#'
#' Parses file and puts it in a tibble, with file validation
#'
#' @param filename a character string which is a file name
#' @param file_validation if `TRUE` calls `file_validation` function (default). If `FALSE`, skips file validation; defaults to system validation.
#'
#' @returns a tibble of parsed code on which to operate
#' @export
#'
#' @importFrom tibble as_tibble
#'
#' @examples
#' example_script("generic_script") |>
#'   get_parsed_tibble()
get_parsed_tibble = function(filename, file_validation = TRUE) {
  if (file_validation) {
    file_validation(filename = filename)
  }
  parsed_file = filename |>
    base::parse(file = _, keep.source = TRUE) |>
    utils::getParseData(x = _) |>
    tibble::as_tibble(x = _)
  return(parsed_file)
}
