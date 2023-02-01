



#' Combinations
#'
#' @param vars 
#'
#' @return
#' @export
#'
#' @examples
getFactComb <- function(vars) {
  
  # main effect
  factors <- vars
  
  # n-way interaction
  for (i in 2:length(vars)) {
    tmp_com <- gtools::combinations(length(vars), i, vars)
    
    for (n in 1:nrow(tmp_com)) {
      factors <- c(factors, str_c(tmp_com[n,], collapse = ":"))
    }
  }
  
  # return all the factors
  return (factors) 
}



#' Create Formula
#' 
#' Using input arguments creates formula
#'
#' @param DV Dependent variable;
#' @param fixFact Fixed factors;
#' @param randFact Random factors;
#' @param ID group variable;
#' @param intercept Integer (0, 1); If 1, the intercept will be free estimated; If 0, the intercept will not be estimated.
#' @param corr Logical;
#'
#' @return Formula
#' @export
#'
#' @examples
createForm <- function(DV, fixFact = NULL, randFact = NULL, ID = "subject", intercept = 1, corr = FALSE) {
  
  fixForm <- str_c(c(intercept, fixFact), collapse = " + ") 
  randForm <- str_c(c(intercept, randFact), collapse = " + ") 
  
  bar <- ifelse(corr, "|", "||")
  
  return(as.formula(str_glue("{DV} ~ {fixForm} + ({randForm} {bar} {ID})")))

}



#' Calculate Bayes Factor
#'
#' @param data An object of class data.frame (or one that can be coerced to that class) containing data of all variables used in the model.
#' @param DV Dependent variable;
#' @param fixFact Fixed factors;
#' @param randFact Random factors;
#' @param ID group variable;
#' @param intercept Integer (0, 1); If 1, the intercept will be free estimated; If 0, the intercept will not be estimated.
#' @param corr Logical;
#' @param args A list of arguments that will input into the brm function.
#' @param name The prefix of the models' name
#' @param path The path where saved the models
#'
#' @return A list of Bayes factors
#' @export
#'
#' @examples
calculateBF <- function(data, DV, fixFact, randFact, testFact=NULL, ID, intercept = 1, corr= FALSE, args, name, path) {
  
  # set default arguments for the functions ------------------------------------
  
  myModel <- function(fixFact=NULL, randFact=NULL, name) {
    
    fixForm <- str_c(c(intercept, fixFact), collapse = " + ") 
    randForm <- str_c(c(intercept, randFact), collapse = " + ") 
    
    bar <- ifelse(corr, "|", "||")
    
    myForm <- as.formula(str_glue("{DV} ~ {fixForm} + ({randForm} {bar} {ID})"))
    
    new_args <- list(
      formula = myForm,
      file = stringr::str_glue("{path}{name}"),
      data = data
    )
    
    if (length(fixFact)<1) {
      input_args <- args
      input_args$prior <- input_args$prior %>% filter(!class=="b")}
    
    do.call(brms::brm,args = append(new_args, args))
  }
  
  # Full model -----------------------------------------------------------------
  
  fullModel <- myModel(fixFact, randFact, name = stringr::str_glue("{name}_full"))
  
  
  # Output variable
  outputBF <- list()
  
  
  # Fix effect -------------------------------------------
  
  if (is.null(testFact)) testFact <- fixFact
  
  
  for (i in 1:length(testFact)) {
    
    target <- fixFact[length(fixFact)+1-i]
    tmpFixFact <- fixFact[!fixFact==target]
    model_name <- stringr::str_glue("{name}_noFix-{gsub(':', '-', target)}")
    
    # fit the model or read the model from the folder.
    if (!file.exists(stringr::str_glue("{path}{model_name}"))) {
      testModel <- myModel(tmpFixFact, randFact, name = model_name)
    } else {
      testModel <- readRDS(stringr::str_glue("{path}{model_name}"))
    }
    
    # which model is the best model
    factBF <- brms::bayes_factor(fullModel, testModel)$bf
    outputBF[stringr::str_glue("fix_{target}")] = factBF
    
  }
  
  return(outputBF)
  
}











##### Load model #####

runModel <- function( args, formula, data, path, reload=FALSE){
  
  # model arguments
  model_args <- append(
    list(formula = formula, 
         data = data,
         file = path),args)
  
  if (!file.exists(path) | reload){
    # run model with brms
    brm_model <- do.call(brms::brm, model_args)
  } else {
    # load model from local file
    brm_model <- readRDS(file = path)
  }
  return(brm_model)
}


##### monitor the status of job #####

monitorJob <- function(status = "read", path) {
  
  filePath <- str_glue("{path}monitor_number.rds")
  
  if (!file.exists(filePath)) {
    mon_num <- 0
  } else {
    mon_num <- readRDS(filePath)
  }
  
  if (status=='start') {
    mon_num = mon_num + 1
  } else if (status=='stop') {
    mon_num = mon_num - 1
  } else if (status=="reset"){
    mon_num <- 0
  } else {
    return (mon_num)
  }
  
  saveRDS(mon_num,filePath)
  
}






