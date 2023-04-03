

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






#' Calculating Bayes Factors
#' 
#' @author Chenyu Li
#' 
#' Credits to Henrik Singmann for providing the idea and template for a similar function implementation
#' Credits to Philipp Musfeld for providing the initial verision of the function
#' 
#' This function runs several models and calculates Bayes factors.
#' 1. The function creates a new data set before running the models.
#' 2. In the data set, we create new columns for the interactions between/among variables.
#' 3. The main reason to do this is to prevent the main effect is included into model when the interaction is in the model.
#' 
#'
#' @param data the dataset which is used for fitting the model
#' @param DV a character string stating how the dependent variable should be written in the formula.
#' For most models, this is only naming the variable (e.g., "Reaction_Time"), 
#' but for some models, it requires a more detailed descriptions 
#' (e.g., for binomial models in brms: "n_correct | trials(n_trials)")
#' @param fixEffects  a character vector specifying the fixed effects of the model
#' @param randEffects a character vector naming the effects which should be tested / the alternative model formulas to create. Names must match effect 
#' @param group a character vector naming a grouping variable for random effects 
#' (currently only 1 grouping variable is supported, e.g. nesting effects within subject)
#' @param by a character vector naming an additional variable, 
#' if random effect structures should be estimated independently for different groups (e.g., for between-subject conditions)
#' @param corr logical. Specifies whether correlations between effects should be estimated. Default is FALSE
#' @param intercept 
#' @param testEffects specified in fixed effects. Per default, this is set to the fixed effects, 
#' meaning that all effects will be tested. If no effects need to be tested, set to NULL
#' @param args a list of args that will input into brm function.
#' @param path a character string. Where the models need to be saved.
#' @param name a character string. The prefix of all the models
#'
#' @return a list including all to-be-test effects
#' 
#' 
cal_BF <-  function(
    data, DV, fixEffects, randEffects, group, by = NULL, 
    corr = FALSE, intercept = TRUE, testEffects = fixEffects, args, path, name) {
  
  
  ### Default parameters
  Int <- ifelse(intercept, 1, 0)
  ### check if an additional grouping argument is provided:
  by_text <- ifelse(is.null(by), "NULL", stringr::str_glue("c({stringr::str_c(by, collapse = ', ')})"))
  
  
  ## Full model ----------------------------------------------------------------
  
  # fit the model or read the model from the folder.
  if (file.exists(stringr::str_glue("{path}{name}_full"))) {
    testModel <- readRDS(stringr::str_glue("{path}{name}_full"))
  } else {
    full_fixForm <- str_c(c(Int, fixEffects), collapse = " + ") 
    full_randForm <- str_c(c(Int, randEffects), collapse = " + ")
    
    full_form <- as.formula(
      stringr::str_glue("{DV} ~ {full_fixForm} + ({full_randForm} | gr({group}, by = {by_text}, cor = {corr}))")
    )
    
    full_args <- append(args, list(
      formula = full_form,
      file = stringr::str_glue("{path}{name}_full"),
      data = data
    ))
    
    fullModel <- do.call(brms::brm,args = full_args)
  }
  

  # if testEffects equals NULL, return full model
  if (is.null(testEffects)) return (fullModel)
  
  
  ## Prepare new data set and new variable names -----------------------------------------
  
  ### create the model fixed part of the model formula based on the passed arguments:
  formula_fixed = reformulate(c(Int, fixEffects), intercept = TRUE)
  
  ### create the model matrix based on the data and the specified effects
  model_matrix = model.matrix(formula_fixed, data)
  
  ### combine model matrix and data to include participant id and DV:
  exl_cols <- unique(c(fixEffects, randEffects))
  exl_cols <- exl_cols[exl_cols %in% names(data)]
  new_data = data.frame(dplyr::select(data, !any_of(exl_cols)), model_matrix)

  
  
  ### get the new names of the fixed effects from the model matrix
  fixEffects_new = names(data.frame(model_matrix))[-1]

    
  ### if a random part of the model formula was specifyied, create this part of the model formula:
  ### create the random part of the formula based on the new variable names:
  ### select the correct column names based on the new effect names:
  
  
  varName_match <- function(oldname, newVars) {
    
    # N-way interaction
    N_inter = stringr::str_count(oldname, ":")
    oldVars = stringr::str_split(oldname, ":")[[1]]
    
    output = c()
    
    # to check whether the new variable is a part of the old one.
    for (i in 1:length(newVars)) {
      
      # N-way interaction
      N_dot = stringr::str_count(newVars[i], "\\.")
      if (N_dot != N_inter) next
      
      # Whether all the variables are included
      matchCount = 0
      for (a in 1: length(oldVars)) {
        if (stringr::str_detect(newVars[i], oldVars[a])) matchCount = matchCount + 1
      }
      
      # If yes, save the new variable
      if (matchCount == length(oldVars)) output = c(output, newVars[i])
    }
    
    return(output)
  }
  
  ### create empty vector for the new random effect names:
  randEffects_new = c()
  
  #### loop over the random effects
  for (i in 1:length(randEffects)) {
    new_rand <- varName_match(randEffects[i], fixEffects_new)
    randEffects_new <- c(randEffects_new, new_rand)
  }
  
  
  ## set default arguments for the functions -----------------------------------
  
  myModel <- function(fixFacts=NULL, randFacts=NULL, name) {
    
    fixForm <- str_c(c(Int, fixFacts), collapse = " + ") 
    randForm <- str_c(c(Int, randFacts), collapse = " + ") 
    
    myForm <- as.formula(
      stringr::str_glue("{DV} ~ {fixForm} + ({randForm} | gr({group}, by = {by_text}, cor = {corr}))")
      )
    
    new_args <- list(
      formula = myForm,
      file = stringr::str_glue("{path}{name}"),
      data = new_data
    )
    
    input_args <- args
    
    if (length(fixFacts)<1) {
      input_args$prior <- input_args$prior %>% filter(!class=="b")}
    
    do.call(brms::brm,args = append(new_args, input_args))
  }
  
  # Full model -----------------------------------------------------------------
  
  # fullModel <- myModel(fixEffects_new, randEffects_new, name = stringr::str_glue("{name}_full"))
  

  # Output variable
  outputBF <- list()
  
  # Fix effect -------------------------------------------
  
  testEffects = rev(testEffects) # to make sure that effects are tested from complex to simple
  
  
  for (i in 1:length(testEffects)) {
    
    target <- testEffects[i]
    excl_factors <- varName_match(target, fixEffects_new)
    tmpFixFact <- fixEffects_new[!fixEffects_new %in% excl_factors]
    model_name <- stringr::str_glue("{name}_noFix_{gsub(':', '-', target)}")
    
    # fit the model or read the model from the folder.
    if (!file.exists(stringr::str_glue("{path}{model_name}"))) {
      testModel <- myModel(tmpFixFact, randEffects_new, name = model_name)
    } else {
      testModel <- readRDS(stringr::str_glue("{path}{model_name}"))
    }
    
    # which model is the best model
    factBF <- brms::bayes_factor(fullModel, testModel)$bf
    outputBF[stringr::str_glue("fix_{target}")] = factBF
    
  }
  
  return(outputBF)
  
    
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






#' Fit the model using brm
#'
#' @param formula An object of class formula.
#' @param data An object of class data.frame containing data of all variables used in the model.
#' @param args A list of arguments that need to input into brm function.
#' @param path A character string. The folder you want to store the model.
#' @param name A character string. The name of the new model.
#'

runModel <- function(formula, data, args, path, name){
  
  file_path <- paste(path,name, sep = "")
  
  # model arguments
  model_args <- append(
    list(formula = formula, 
         data = data,
         file = file_path,
         file_refit = "on_change"),args)

  brm_model <- do.call(brms::brm, model_args)

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


Pairwise_Comparisons <- function(model, prior_model = NULL, specs, interaction = FALSE) {
  
  if (is.null(prior_model)) prior_model <- bayestestR::unupdate(model, verbose = TRUE)
  
  for (i in 1:length(specs)) {
    
    par = as.formula(specs[i])
    
    if (interaction) {
      post_contrasts <- emmeans::emmeans(model, par) %>% 
        contrast(interaction = c("pairwise","pairwise"))
      prior_contrasts <- emmeans::emmeans(prior_model, par) %>% 
        contrast(interaction = c("pairwise","pairwise"))
    } else {
      post_contrasts <- pairs(emmeans::emmeans(model, par))
      prior_contrasts <- pairs(emmeans::emmeans(prior_model, par))
    }
    
    
    print(bayestestR::bayesfactor_parameters(post_contrasts, prior_contrasts))
    
  }
  
}





