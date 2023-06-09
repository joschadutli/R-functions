

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




#' Initialize job log.
#' 
#' @description
#' This function will create a data frame including model information and store it in local disk
#' 
#' @param path A folder path used to save the file
#' 
#' 
init_multijobs <- function(path = NULL) {
  
  Table_status <- data.frame(
    index = 0,
    core = 0,
    status = "completed",
    priority = 1)
  
  if (is.null(path)){
    file_path = "./job_log.rds"
  } else {
    file_path = str_glue("{path}job_log.rds")
  }
  
  write_rds(Table_status, file_path)
  
}


#' Set up multiple job system
#' 
#' @author Chenyu Li
#' 
#' @param operation description
#' 
#' @param path A folder path used to save the file
#' 
#' 
read_jobLog <- function(path = NULL) {
  
  if (is.null(path)){
    file_path = "./job_log.rds"
  } else {
    file_path = str_glue("{path}job_log.rds")
  }
  
  Table_status <- read_rds(file_path)
  
  return(Table_status)
  
}





#' A smart function which can find a appropriate time to launch models.
#' 
#' @author Chenyu Li
#' @description
#' This is a function used to running brms in job environment. 
#' The function relies on job log file. It will detect how many jobs are running 
#' and how many cores are available on the computer.
#' Only when the computer has sufficient free cores and the model is in the first sequence, the model will start.
#' 
#'@param formula a formula. The formula that you want to input into `brm` function.
#'@param data a data.frame. The data that you want to use to train the model.
#'@param args a list. All the other arguments that you want to input into `brm` function.
#'@param path a folder path. Where do you want to save your model.(It should also be the place to store the job log file)
#'@param name a string. The name of the file.
#'@param priority a number. The default is 1, higher number indicates higher priority. 
#'Models with higher priority will be launched in advance.
#'@param maxCore a number. How many cores are available on this computer.
#'@param checkInt a number. How long would the function check the job log. The default value is 30 seconds.
#'
#'@return There is no return. The model will be saved as a file automatically.
#'
#'
smart_runModels <- function(formula, data, args, path, name, priority = 1, maxCore = 8, checkInt = 30){
  
  # get information
  nCore = args$cores
  log_path = str_glue("{path}job_log.rds")
  Table_status <- read_rds(log_path)
  myIndex <- nrow(Table_status)
  
  # check whether the maxCore was smaller than the need of the core
  if (nCore > maxCore) stop("The current model requires more cores than the maximum number of cores available on the computer.")
  
  # if the job log file has not been created, create a new one
  if (!file.exists(log_path)) {
    init_multijobs(path = path)
    message("There is no job log file, a new one has been created")
  }
  
  # input model information to the log
  Table_status <- Table_status %>% 
    rbind(data.frame(
      index = myIndex,
      core = nCore,
      status = "waiting",
      priority = priority
    ))
  
  write_rds(Table_status, log_path)
  
  # set up the progress bar
  message("The model is not in the waiting list ...")
  pb = txtProgressBar(min = 0, max = myIndex, initial = 0, style = 3)
  
  
  #' check how many models are running,
  #' wait until the model meet the running condition
  while (TRUE) {
    
    # Read job log
    Table_status <- read_rds(log_path)
    
    # How many models are running
    Table_running <- Table_status %>% 
      filter(status=="running")
    # How many cores have been used
    Using_cores = sum(Table_running$core)
    
    # Adjust the waiting list
    Table_waiting <- Table_status %>% 
      filter(status=="waiting") %>% 
      mutate(prio_idx = max(priority) - priority) %>% 
      arrange(prio_idx,index)
    
    # Get the waiting index
    WaitIndex = which(Table_waiting$index == myIndex)
    
    
    # If the model are in the first place and there are sufficient cores, run the model
    if (WaitIndex==1 & nCore <= maxCore - Using_cores) {
      
      # update progress bar
      setTxtProgressBar(pb, value = myIndex)
      close(pb)
      
      break
      
      # Print the waiting information
    } else {
      # message(str_glue("There is/are {nrow(Table_running)} models running. The current model is in {WaitIndex} place."))
      
      # update progress bar
      setTxtProgressBar(pb, value = myIndex - WaitIndex)
      
      # Recheck everything after n*checkInt seconds
      Sys.sleep(WaitIndex*checkInt)
    }
    
  }
  
  # The full name of the file (including the path)
  file_name <- paste(path,name, sep = "")
  
  # print the start time
  start_time = Sys.time()
  message(str_glue("The model start to run at {start_time}"))
  
  # model arguments
  model_args <- append(
    list(formula = formula, 
         data = data,
         file = file_name,
         file_refit = "on_change"),args)
  
  
  tryCatch(
    expr = {
      # Adjust the status of the model as running.
      Table_status <- read_rds(log_path)
      Table_status[which(Table_status$index == myIndex), "status"] = "running"
      write_rds(Table_status, log_path)
      
      # Run the model
      brm_model <- do.call(brms::brm, model_args)
      
      # Adjust the status of the model as completed.
      Table_status <- read_rds(log_path)
      Table_status[which(Table_status$index == myIndex), "status"] = "completed"
      write_rds(Table_status, log_path)
      
    },
    error = function(e){
      
      # Adjust the status of the model as running.
      Table_status <- read_rds(log_path)
      Table_status[which(Table_status$index == myIndex), "status"] = "failed"
      write_rds(Table_status, log_path)
      
      # message error
      message(e)
      
      return(NA)
    },
    finally = {
      # print the end time
      end_time = Sys.time()
      message(str_glue("The model has done at {end_time}"))
      message(str_glue("It takes {round(as.numeric(end_time - start_time, units = 'hours'),2)} hours."))
    }
  )
  
}




#' A smart function which can find a appropriate time to launch input function
#' 
#' @author Chenyu Li
#' 
#' @description
#' This is a function used to running `brm` package in job environment. 
#' The function relies on job log file. It will detect how many jobs are running 
#' and how many cores are available on the computer.
#' Only when the computer has sufficient free cores and the model is in the first sequence, the model will start.
#' 
#'@param formula a formula. The formula that you want to input into `brm` function.
#'@param data a data.frame. The data that you want to use to train the model.
#'@param args a list. All the other arguments that you want to input into `brm` function.
#'@param path a folder path. Where do you want to save your model.(It should also be the place to store the job log file)
#'@param name a string. The name of the file.
#'@param priority a number. The default is 1, higher number indicates higher priority. 
#'Models with higher priority will be launched in advance.
#'@param maxCore a number. How many cores are available on this computer.
#'@param checkInt a number. How long would the function check the job log. The default value is 30 seconds.
#'
#'@return There is no return. The model will be saved as a file automatically.
#'
#'
smart_runFuns <- function(fun, args, path, core = 1, priority = 1, maxCore = 8, checkInt = 30){
  
  # get information
  nCore = core
  log_path = str_glue("{path}job_log.rds")
  Table_status <- read_rds(log_path)
  myIndex <- nrow(Table_status)
  
  # check whether the maxCore was smaller than the need of the core
  if (nCore > maxCore) stop("The current model requires more cores than the maximum number of cores available on the computer.")
  
  # if the job log file has not been created, create a new one
  if (!file.exists(log_path)) {
    init_multijobs(path = path)
    message("There is no job log file, a new one has been created")
  }
  
  # input model information to the log
  Table_status <- Table_status %>% 
    rbind(data.frame(
      index = myIndex,
      core = nCore,
      status = "waiting",
      priority = priority
    ))
  
  write_rds(Table_status, log_path)
  
  # set up the progress bar
  message("The model is not in the waiting list ...")
  pb = txtProgressBar(min = 0, max = myIndex, initial = 0, style = 3)
  
  
  #' check how many models are running,
  #' wait until the model meet the running condition
  while (TRUE) {
    
    # Read job log
    Table_status <- read_rds(log_path)
    
    # How many models are running
    Table_running <- Table_status %>% 
      filter(status=="running")
    # How many cores have been used
    Using_cores = sum(Table_running$core)
    
    # Adjust the waiting list
    Table_waiting <- Table_status %>% 
      filter(status=="waiting") %>% 
      mutate(prio_idx = max(priority) - priority) %>% 
      arrange(prio_idx,index)
    
    # Get the waiting index
    WaitIndex = which(Table_waiting$index == myIndex)
    
    # If the model are in the first place and there are sufficient cores, run the model
    if (WaitIndex==1 & nCore <= maxCore - Using_cores) {
      
      # update progress bar
      setTxtProgressBar(pb, value = myIndex)
      close(pb)
      
      break
      
      # Print the waiting information
    } else {
      # message(str_glue("There is/are {nrow(Table_running)} models running. The current model is in {WaitIndex} place."))
      
      # update progress bar
      setTxtProgressBar(pb, value = myIndex - WaitIndex)
      
      
      # Recheck everything after n*checkInt seconds
      Sys.sleep(WaitIndex*checkInt)
    }
    
  }
  
  # print the start time
  start_time = Sys.time()
  message(str_glue("The model start to run at {start_time}"))
  

  tryCatch(
    expr = {
      # Adjust the status of the model as running.
      Table_status <- read_rds(log_path)
      Table_status[which(Table_status$index == myIndex), "status"] = "running"
      write_rds(Table_status, log_path)
      
      # Run the model
      results <- do.call(fun, args)
      
      # Adjust the status of the model as completed.
      Table_status <- read_rds(log_path)
      Table_status[which(Table_status$index == myIndex), "status"] = "completed"
      write_rds(Table_status, log_path)
      
      return(results)
      
    },
    error = function(e){
      
      # Adjust the status of the model as running.
      Table_status <- read_rds(log_path)
      Table_status[which(Table_status$index == myIndex), "status"] = "failed"
      write_rds(Table_status, log_path)
      
      # message error
      message(e)
      
      return(NA)
    },
    finally = {
      # print the end time
      end_time = Sys.time()
      message(str_glue("The model has done at {end_time}"))
      message(str_glue("It takes {round(as.numeric(end_time - start_time, units = 'hours'),2)} hours."))
    }
  )
  
}


#' A function used to extract posterior distribution from a model fit
#'
#'@author Chenyu Li
#'
#'@param fit a model fit
#'@param effects a string or vector; The name of the effect
#'
#'
extractPost <- function(fit, effects) {
  
  post_data <- fit %>% 
    tidy_draws() %>% 
    select(.chain, .iteration, .draw, starts_with("b_")) %>% 
    pivot_longer(cols = starts_with("b_"),
                 names_to = "coef",
                 values_to = "post") %>% 
    separate_wider_delim(coef, "_", names = c(NA, "M3_par","effect"), cols_remove = TRUE) %>% 
    separate_wider_delim(effect, ":", names = effects) %>% 
    mutate_at(effects, .funs = function(x) x = str_remove(x,str_glue(effects, collapse = "|")))
  
  return (post_data)
  
}


#' Fit data with specific models in sequence and compare models with calculating Bayes factor.
#' 
#' @param data A data.frame; The data that models want to fit
#' @param table A data.frame; The argument that you want to input into the function (to generate the model)
#' @param fun A function; The function used to generate models
#' @param model_path string; The place where models are saved.
#' @param task description string; The name of the task.
#' @param sample A number; The number of iteration for each chain.
#' @param warmup A number; The number of warm up before sampling.
#' @param core A number;
#' 
#' 
modelComparison <- function(data, table, fun, model_path, task, brmArgs, maxCore = 8) {
  
  for (i in 1:nrow(table)) {
    
    model_name = as.character(table[i,"model_name"])
    
    output <- do.call(fun, args = as.list(table[i,1:(ncol(table)-4)]))
    
    Formula_M3 <- output$formula
    Prior_M3 <- output$prior
    
    ## Import arguments
    Args_M3 <- list(
      family = multinomial(refcat = NA),
      prior = Prior_M3) %>% 
      append(brmArgs)
    
    ## Fit model
    job({
      
      print(task)
      source("https://raw.githubusercontent.com/chenyu-psy/R-functions/main/brmsFun.R")
      
      smart_runModels(
        formula = Formula_M3,
        data = data,
        args = Args_M3,
        path = model_path,
        name = model_name,
        priority = 1,
        checkInt = 180,
        maxCore = maxCore
      )
    }, title = str_glue("Model {i}: {model_name}"), import = "auto")
    
    Sys.sleep(20)
    
    # skip the model comparison for the first model
    if (i == 1)
      next
    
    # calculate the Bayes factor and save the results
    job({
      
      print(task)
      source("https://raw.githubusercontent.com/chenyu-psy/R-functions/main/brmsFun.R")
      
      smart_runFuns(
        fun = function(current_index) {
          Table_BF = read_csv(str_glue("{model_path}BayesFactor_{task}_rarc.csv"))
          
          best_index = as.numeric(Table_BF[current_index - 1, "best_model"])
          
          # model_name
          current_name = Table_BF[current_index, 'model_name']
          best_name = Table_BF[best_index, 'model_name']
          # read model
          fit_current <-
            readRDS(str_glue("{model_path}{current_name}.rds"))
          fit_best <-
            readRDS(str_glue("{model_path}{best_name}.rds"))
          
          BF = bayes_factor(fit_current, fit_best)
          
          Table_BF = read_csv(str_glue("{model_path}BayesFactor_{task}_rarc.csv"))
          
          Table_BF[current_index, "comparison"] = str_glue("model {current_index} vs. model {best_index}")
          Table_BF[current_index, "BF"] = BF$bf
          Table_BF[current_index, "best_model"] = ifelse(BF$bf > 5, current_index, best_index)
          
          write_csv(Table_BF, str_glue("{model_path}BayesFactor_{task}_rarc.csv"))
          
        },
        args = list(current_index = i),
        path = model_path,
        core = 1,
        maxCore = 1,
        checkInt = 180
      )
      
      
    }, import = "auto", title = str_glue("BF: model {i} vs. the best model"))
    
    Sys.sleep(20)
    
  }
}






