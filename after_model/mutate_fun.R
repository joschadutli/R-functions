#'Create a new column according to the input function
#'
#'@param .data a data frame
#'@param formulas a formula variable, the formula use to calculate the value of new variable.
#'The dependent variable will be the name of the new column.
#'@return a data frame with new column
#'
mutate_fun <- function(.data, formulas) {
  
  if (!require(tidyverse)) {
    install.packages("tidyverse")
    library("tidyverse")
    }
  
  # copy the data frame
  results <- .data
  
  # convert formulas to list
  formulas = c(formulas)
  
  # calculate the formula one by one
  for (i in 1:length(formulas)) {
    
    # get the formula
    form = formulas[[i]]
    
    # extract dependent variable
    y = all.vars(form)[1]
    # extract right-side formula
    right_from = as.character(form[-2]) %>% 
      stringr::str_remove("~")
    # add the dependent variable to the data frame
    results[, y] = with(results, eval(parse(text = right_from)))
  }
  
  return(results)
  
}