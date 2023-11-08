
#' Run multiple models sequentially.
#' 
#' @author Chenyu Li
#' @param task A string; The name of the task
#' @param pars A list; The parameters to be tested
#' @param form_fun A function; The function used to generate the formula
#' @param prior_fun A function; The function used to generate the prior
#' @param brmArgs A list; The arguments used in brm
#' @param model_path A string; The path to save the model
#' @param sample_path A string; The path to save the samples
#' @param bf_path A string; The path to save the Bayes factor
#' @param maxCore An integer; The maximum number of cores to use
#' @param sample_check A boolean; Whether to check the samples
#' 
#' @return There is no return. The model and results will be saved as a file automatically.
#'
seq_model_comparsion <- function(
    task,
    pars,
    form_fun,
    prior_fun,
    brmArgs,
    model_path,
    sample_path,
    bf_path,
    maxCore=NULL,
    sample_check = TRUE) {
  
  # check packages
  packages = c("brms", "dplyr", "stringr", "glue", "progress")
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg, character.only = TRUE)
      library(pkg, character.only = TRUE)
    }
  }
  
  # original model
  Pars_best <- do.call(expand_grid, args = pars)[1, ]
  
  # All the assumptions that need to be tested for the parameter
  List_parameters <- as.list(Pars_best)
  List_parameters[[names(pars)[1]]] <- unname(unlist(pars[names(pars)[1]]))
  
  Table_temp <- do.call(expand_grid, args = List_parameters) %>% 
    rowwise() %>% 
    mutate(part_name = paste(names(.), unlist(c_across(1:ncol(.))), sep = "", collapse = "_"),
           model_name = str_glue("Model_{task}_M3_{part_name}"),
           sample_name = str_glue("Sample_{task}_M3_{part_name}"),
           comparison = NA,
           BF = NA,
           logBF = NA,
           reliability = NA,
           best_model = NA) %>% 
    select(!part_name) %>% 
    rowwise() %>%
    mutate(
      model_file = str_glue("{model_path}{model_name}.rds"),
      sample_file = str_glue("{sample_path}{sample_name}.rds"),
      model_ck = ifelse(file.exists(model_file), 1, 0),
      sample_ck = ifelse(file.exists(sample_file), 1, 0)
    )
  
  write_rds(Table_temp, file = str_glue("{model_path}temporary_table.rds"))
  
  # test assumptions for each parameter
  for (par in names(pars)) {
    
    # test assumptions for each parameter step by step
    for (i in 1:length(pars[[par]])) {
      ## Fit model
      job({
        source(
          "https://raw.githubusercontent.com/chenyu-psy/R-functions/main/smart_jobs/smart_runFuns.R"
        )
        
        smart_runFuns(
          fun = function(model_path, iRow) {
            
            Table_temp <- read_rds(str_glue("{model_path}temporary_table.rds"))
            
            model_formula <- do.call(form_fun, args = as.list(Table_temp[iRow, names(pars)]))
            model_prior <- do.call(prior_fun, args = as.list(Table_temp[iRow, names(pars)]))
            
            
            ## Import arguments
            Args_M3 <- list(
              formula = model_formula,
              prior = model_prior,
              file = str_glue('{model_path}{Table_temp[iRow, "model_name"]}')
            ) %>%
              append(brmArgs)
            
            # Run the model
            if (Table_temp[iRow, "model_ck"] == 0 &
                (Table_temp[iRow, "sample_ck"] == 0 |
                 !sample_check)) {
              brm_model <- do.call(brms::brm, Args_M3)
            }
          },
          args = list(
            model_path = model_path,
            iRow = i),
          log_path = model_path,
          core = brmArgs$core,
          maxCore = maxCore,
          checkInt = 5
        )
        
      }, title = str_glue("Model {i}: {par}-{pars[[par]][i]}"), import = "auto")
      
      Sys.sleep(9)
      
    }
    
    ############################### run samples
    for (i in 1:length(pars[[par]])) {
      # read samples, if the sample does not existent, sampling data and save it.
      
      # calculate the Bayes factor and save the results
      job({
        source(
          "https://raw.githubusercontent.com/chenyu-psy/R-functions/main/smart_jobs/smart_runFuns.R"
        )
        
        smart_runFuns(
          fun = function(model_path, iRow, maxCore) {
            
            Table_temp <- read_rds(str_glue("{model_path}temporary_table.rds"))
            
            # the path of the model
            Path_model <- as.character(Table_temp[iRow, "model_file"])
            Path_sample <- as.character(Table_temp[iRow, "sample_file"])
            
            # if the sample does not exist, sample the model
            if (Table_temp[iRow, "sample_ck"] == 0) {
              # read the model
              model_current <- read_rds(Path_model)
              # sampling data
              sample_current <-
                bridgesampling::bridge_sampler(
                  model_current,
                  cores = round(maxCore / 2),
                  repetition = 10,
                  maxiter = 1000
                )
              # save the sample
              write_rds(sample_current, Path_sample)
            }
            
          },
          args = list(
            model_path = model_path,
            iRow = i,
            maxCore = maxCore),
          log_path = model_path,
          core = round(maxCore / 2),
          maxCore = maxCore,
          checkInt = 5
        )
        
      }, title = str_glue("Sample {i}: {par}-{pars[[par]][i]}"), import = "auto")
      
      Sys.sleep(9)
      
    }
    
    ############################### calculate BF
    job({
      
      source(
        "https://raw.githubusercontent.com/chenyu-psy/R-functions/main/smart_jobs/smart_runFuns.R"
      )
      
      smart_runFuns(
        fun = function(model_path) {
          
          table <- read_rds(str_glue("{model_path}temporary_table.rds"))
          
          table[1, "best_model"] = 1
          
          # Compare models and calculate BF if there is more than one model.
          if ( nrow(table) >= 2) {
            for (i in 2:nrow(table)) {
              
              # set first model as the best model
              index_best_sample = as.numeric(table[i-1, "best_model"])
              
              name_best_sample <- table[index_best_sample, "sample_name"]
              name_current_sample <- table[i, "sample_name"]
              
              Sample_best <- read_rds(as.character(table[index_best_sample, "sample_file"]))
              Sample_currect <- read_rds(as.character(table[i, "sample_file"]))
              
              BF <- bridgesampling::bf(Sample_currect, Sample_best)
              
              table[i, "comparison"] = str_glue("Model {i} vs. Model {index_best_sample}")
              table[i, "BF"] = BF$bf_median_based
              table[i, "logBF"] = log(BF$bf_median_based)
              table[i, "reliability"] = str_glue("{round(log(min(BF$bf)),2)} ~ {round(log(max(BF$bf)),2)}")
              table[i, "best_model"] = ifelse(log(BF$bf_median_based) > 1.6, i, index_best_sample)
            }
          }
          
          # save the table
          save_path = str_glue("{bf_path}BayesFactor_{task}_{par}.csv")
          
          # select columns and save the table
          table <- table %>% 
            select(names(pars), comparison, BF, logBF, reliability, best_model) %>% 
            write_csv(save_path)
          
          ### update temporary table if the current parameter is not the last parameter
          if (par != names(pars)[length(pars)]) {
            
            Index_best_sample <- as.numeric(table[nrow(table), "best_model"])
            Pars_best <- table[Index_best_sample, ]
            next_par <- names(pars)[which(names(pars)==par) + 1]
            
            List_parameters <- as.list(Pars_best %>% select(names(pars)))
            List_parameters[[next_par]] <- unname(unlist(pars[next_par]))
            Table_toBeTeseted <- do.call(expand_grid, args = List_parameters) %>% 
              rowwise() %>% 
              mutate(part_name = paste(names(.), unlist(c_across(1:ncol(.))), sep = "", collapse = "_"),
                     model_name = str_glue("Model_{task}_M3_{part_name}"),
                     sample_name = str_glue("Sample_{task}_M3_{part_name}"),
                     model_file = str_glue("{model_path}{model_name}.rds"),
                     sample_file = str_glue("{sample_path}{sample_name}.rds"),
                     model_ck = ifelse(file.exists(model_file), 1, 0),
                     sample_ck = ifelse(file.exists(sample_file), 1, 0),
                     comparison = NA,
                     BF = NA,
                     logBF = NA,
                     reliability = NA,
                     best_model = NA) %>% 
              select(!part_name)
            
            write_rds(Table_toBeTeseted, file = str_glue("{model_path}temporary_table.rds"))
          }
          
        },
        args = list(model_path = model_path),
        log_path = model_path,
        core = maxCore,
        maxCore = maxCore,
        checkInt = 5
      )
    }, title = str_glue("Bayes Factor: {par}"))
    
    Sys.sleep(9)
    
  }
  
}