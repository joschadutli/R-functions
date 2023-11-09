


#' Generate data from M3 model
#' 
#' @param parsData a data frame with parameters
#' @param respOpts a data frame with response options
#' @param formulas a formula variable, the formula use to calculate the value of new variable.
#' @param nResp a number, the number of response options
#' @param group a string, the name of the group variable
gen_M3_data <- function(
    parsData,
    respOpts,
    formulas,
    nResp = 100, 
    group = NULL,
    choiceRule = "Luce",){
  
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
    mutate(
      exprob = case_when(
        choiceRule == "Softmax" | choiceRule == "expLuce" ~ exp(activation) * nOpt,
        choiceRule == "Luce" ~ activation * nOpt
      )) %>% 
    group_by(across({{group}})) %>% 
    mutate(sumExp = sum(exprob)) %>% 
    ungroup() %>% 
    mutate(prob = exprob / sumExp)
  
  respData <- pData %>% 
    select(all_of(group), response, prob) %>% 
    group_by(across({{group}})) %>% 
    mutate(count = unlist(rmultinom(1, nResp, prob = prob)[,1])) %>% 
    ungroup() %>% 
    select(!prob) %>% 
    left_join(respOpts)
    
  exportData <- respData %>% 
    pivot_wider(
      names_from = response, 
      values_from = c(count, nOpt)) %>% 
    rename_with(~ str_remove(.x, "count_"), starts_with("count")) %>%
    rename_with(~ str_replace(.x, "nOpt_","n"), starts_with("nOpt")) %>%
    mutate(y = cbind(across({{responseNames}}), deparse.level = 0),
           nTotal = nResp)
  
  exportData$y <- as.matrix(exportData[, responseNames])
  colnames(exportData$y) = 1:ncol(exportData$y)
  exportData <- exportData %>% 
    select(-all_of(responseNames))
  
  exportData <- left_join(parsData, exportData)
  
  return(exportData)

}