# Checks whether file exists (error) and is R (warning)
file_validation = function(filename) {
  if (!base::file.exists(filename)) {
    stop("couldn't find file ", filename)
  }
  if (!tools::file_ext(filename) == "R") {
    warning("expecting *.R file, will try to proceed")
  }
}

# Parses file and puts it in a tibble, with file validation
get_parsed_tibble = function(filename, file_validation = TRUE) {
  if (file_validation) file_validation(filename = filename)
  parsed_file = filename |>
    base::parse(keep.source = TRUE) |>
    utils::getParseData() |>
    tibble::as_tibble()
  return(parsed_file)
}
