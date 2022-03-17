


filterOutlier <- function(.data, vars, std = 3) {
  df <- .data
  for (var in vars) {
    df <- df %>%
      mutate(zscore = scale(.data[[var]])) %>%
      mutate(across({{var}}, .fns = ~ ifelse(abs(zscore) > std, NA, .data[[var]]))) %>%
      mutate(across({{var}}, .fns = ~ ifelse(
        zscore > std,
        mean(.data[[var]], na.rm =
               T) + std * sd(.data[[var]], na.rm = T),
        .data[[var]]
      )),
      across({{var}}, .fns = ~ ifelse(
        zscore < -std,
        mean(.data[[var]], na.rm =
               T) - std * sd(.data[[var]], na.rm = T),
        .data[[var]]
      ))) %>%
      select(!zscore)
  }
  return (df)
}
