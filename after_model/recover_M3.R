




#' Generate data from M3 model
#' 
#' @param parsData a data frame with parameters
#' @param respOpts a data frame with response options
#' @param formulas a formula variable, the formula use to calculate the value of new variable.
#' @param nResp a number, the number of response options
#' @param group a string, the name of the group variable
recover_M3 <- function(
    parsData,
    respOpts,
    formulas,
    group = c(".chain", ".iteartion", ".draw")
    ){
  
  source("https://raw.githubusercontent.com/chenyu-psy/R-functions/main/after_model/mutate_fun.R")
  
  ## check packages and install if necessary
  if (!require(dplyr)) {
    install.packages("dplyr")
    library(dplyr)
  }
  
  ## get the vector of response categories
  responseNames <- c()
  for (i in 1:length(formulas)) {
    responseNames <- c(responseNames, all.vars(formulas[[i]])[1])
  }
  
  ## calculate the activation for each response according to the formula
  actData <- parsData %>% 
    mutate_fun(formulas)
  
  
  # name ID
  if (is.null(group)) {
    actData$ID = 1:nrow(actData)
    group = "ID"
  }
  
  pData <- actData %>% 
    select(all_of(c(group, responseNames))) %>% 
    pivot_longer(
      cols = all_of(responseNames), 
      names_to = "response", 
      values_to = "activation") %>% 
    left_join(respOpts) %>%
    mutate(exprob = exp(activation) * nOpt) %>% 
    group_by(across({{group}})) %>% 
    mutate(sumExp = sum(exprob)) %>% 
    ungroup() %>% 
    mutate(prob = exprob / sumExp)

    return(pData)
}
