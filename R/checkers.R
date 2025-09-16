#' Check whether specified special functions have been used.
#'
#' A generic function to check whether specific special functions have been used
#' special functions include `if`, `for`, `while`, `repeat`, `break`, `next`, `return`, `function`, `quote`, `switch`, `{`, `(`.
#' [Check here](https://cran.r-project.org/doc/manuals/r-release/R-ints.html#g_t_002eInternal-vs-_002ePrimitive) for the updated list of special function names.
#'
#' @param filename a character string which is a file name
#' @param func_vector a character vector of special functions. It is best if this is just one or two functions, especially `for` and `while`
#'
#' @returns a numeric count of violations for the specified functions
#' @export
#'
#' @importFrom dplyr filter
#' @importFrom rlang .data
#'
#' @examples
#' example_script("generic_script") |>
#'   check_for_special_functions(func_vector = "for")
check_for_special_functions = function(filename, func_vector) {
  count_violations = filename |>
    get_parsed_tibble() |>
    dplyr::filter(.data$token %in% func_vector) |>
    base::nrow()
  return(count_violations)
}

#' Checks whether functions call `return`.
#'
#' This function takes an R script and
#' outputs a vector of function names that don't have return calls
#'
#' @param filename a character string which is a file name
#'
#' @returns a vector of functions that are missing a `return` call
#' @export
#'
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr lead
#' @importFrom dplyr lag
#' @importFrom dplyr pull
#' @importFrom rlang .data
#'
#' @examples
#' example_script("generic_script") |>
#'   return_checker()
return_checker = function(filename) {
  missing_returns = filename |>
    get_parsed_tibble() |>
    dplyr::filter(.data$terminal == TRUE) |>
    dplyr::filter(
      .data$token == "FUNCTION" |
        #Get the return calls
        (.data$token == "SYMBOL_FUNCTION_CALL" & .data$text == "return") |
        .data$token == "SYMBOL"
    ) |>
    # Filter out SYMBOL tokens without FUNCTION in the next row
    dplyr::filter(
      .data$token != "SYMBOL" |
        (dplyr::lead(.data$token, default = "") == "FUNCTION")
    ) |>
    dplyr::mutate(
      next_token = dplyr::lead(.data$token),
      previous_text = dplyr::lag(.data$text)
    ) |>
    dplyr::filter(
      .data$token == "FUNCTION" &
        (is.na(.data$next_token) | .data$next_token != "SYMBOL_FUNCTION_CALL")
    ) |>
    dplyr::pull(.data$previous_text)
  return(missing_returns)
}

#' Count `base` and `magrittr` pipe usages
#'
#' This function takes an R script and
#' outputs a tibble with pipe counts by type
#' always outputs a count for both
#'
#' @param filename a character string which is a file name
#'
#' @returns a tibble with counts of each pipe type, `|>` and `%>%`
#' @export
#'
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr rename
#' @importFrom tidyr complete
#' @importFrom rlang .data
#'
#' @examples
#' example_script("generic_tidyverse_script") |>
#'   pipe_counter()
pipe_counter = function(filename) {
  pipe_counts = filename |>
    get_parsed_tibble() |>
    dplyr::filter(
      (.data$token == "SPECIAL" & .data$text == "%>%") |
        (.data$token == "PIPE" & .data$text == "|>")
    ) |>
    dplyr::group_by(.data$text) |>
    dplyr::summarize(n = dplyr::n()) |>
    dplyr::rename(pipe = .data$text) |>
    tidyr::complete(pipe = c("|>", "%>%"), fill = list(n = 0))
  return(pipe_counts)
}

#' Count the number of dollar signs used.
#'
#' This function takes an R scripts and
#' outputs a count of dollar sign uses
#' works within function calls, too
#'
#' @param filename a character string which is a file name
#'
#' @returns a count of dollar sign, `$`, uses, e.g., to extract elements of a `list` (and thus `data.frame`)
#' @export
#'
#' @importFrom dplyr filter
#' @importFrom rlang .data
#'
#' @examples
#' example_script("generic_script") |>
#'   dollar_sign_counter()
dollar_sign_counter = function(filename) {
  dollar_count = filename |>
    get_parsed_tibble() |>
    dplyr::filter(.data$token == "'$'" & .data$text == "$") |>
    base::nrow()
  return(dollar_count)
}
