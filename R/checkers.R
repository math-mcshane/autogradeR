# include this link to check for special function names
# https://cran.r-project.org/doc/manuals/r-release/R-ints.html#g_t_002eInternal-vs-_002ePrimitive
# A generic function to check whether special functions have been used
check_for_special_functions = function(filename, func_vector) {
  count_violations = filename |>
    get_parsed_tibble() |>
    dplyr::filter(token %in% func_vector) |>
    base::nrow()
  return(count_violations)
}

# This function takes an R script and
# outputs a vector of function names that don't have return calls
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

# This function takes an R script and
# outputs a tibble with pipe counts by type
# always outputs a count for both
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

# This function takes an R scripts and
# outputs a count of dollar sign uses
# works within function calls, too
dollar_sign_counter = function(filename) {
  dollar_count = filename |>
    get_parsed_tibble() |>
    dplyr::filter(token == "'$'" & text == "$") |>
    base::nrow()
  return(dollar_count)
}
