#' check_for_special_functions
#'
#' A generic function to check whether specific special functions have been used
#' special functions include `if`, `for`, `while`, `repeat`, `break`, `next`, `return`, `function`, `quote`, `switch`, `{`, `(`
#' This link has the updated list of special function names
#' https://cran.r-project.org/doc/manuals/r-release/R-ints.html#g_t_002eInternal-vs-_002ePrimitive
#'
#' @param filename a character string which is a file name
#' @param func_vector a character vector of special functions. It is best if this is just one or two functions, especially `for` and `while`
#'
#' @returns a numeric count of violations for the specified functions
#' @export
#'
#' @examples
#' try(check_for_special_functions())
check_for_special_functions = function(filename, func_vector) {
  count_violations = filename |>
    get_parsed_tibble() |>
    dplyr::filter(token %in% func_vector) |>
    base::nrow()
  return(count_violations)
}

#' return_checker
#' This function takes an R script and
#' outputs a vector of function names that don't have return calls
#'
#' @param filename a character string which is a file name
#'
#' @returns a vector of functions that are missing a `return` call
#' @export
#'
#' @examples
#' try(return_checker())
return_checker = function(filename) {
  missing_returns = filename |>
    get_parsed_tibble() |>
    dplyr::filter(terminal == TRUE) |>
    dplyr::filter(
      token == "FUNCTION" |
        #Get the return calls
        (token == "SYMBOL_FUNCTION_CALL" & text == "return") |
        token == "SYMBOL"
    ) |>
    # Filter out SYMBOL tokens without FUNCTION in the next row
    dplyr::filter(
      token != "SYMBOL" | (dplyr::lead(token, default = "") == "FUNCTION")
    ) |>
    dplyr::mutate(
      next_token = dplyr::lead(token),
      previous_text = dplyr::lag(text)
    ) |>
    dplyr::filter(
      token == "FUNCTION" &
        (is.na(next_token) | next_token != "SYMBOL_FUNCTION_CALL")
    ) |>
    dplyr::pull(previous_text)
  return(missing_returns)
}

#' pipe_counter
#' This function takes an R script and
#' outputs a tibble with pipe counts by type
#' always outputs a count for both
#'
#' @param filename a character string which is a file name
#'
#' @returns a tibble with counts of each pipe type, `|>` and `%>%`
#' @export
#'
#' @examples
#' try(pipe_counter())
pipe_counter = function(filename) {
  pipe_counts = filename |>
    get_parsed_tibble() |>
    dplyr::filter(
      (token == "SPECIAL" & text == "%>%") |
        (token == "PIPE" & text == "|>")
    ) |>
    dplyr::group_by(text) |>
    dplyr::summarize(n = dplyr::n()) |>
    dplyr::rename(pipe = text) |>
    tidyr::complete(pipe = c("|>", "%>%"), fill = list(n = 0))
  return(pipe_counts)
}

#' dollar_sign_counter
#' This function takes an R scripts and
#' outputs a count of dollar sign uses
#' works within function calls, too
#'
#' @param filename a character string which is a file name
#'
#' @returns a count of dollar sign, `$`, uses, e.g., to extract elements of a `list` (and thus `data.frame`)
#' @export
#'
#' @examples
#' try(dollar_sign_counter())
dollar_sign_counter = function(filename) {
  dollar_count = filename |>
    get_parsed_tibble() |>
    dplyr::filter(token == "'$'" & text == "$") |>
    base::nrow()
  return(dollar_count)
}
