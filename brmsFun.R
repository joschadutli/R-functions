
##### write formulas #####

genFormula <- function(DV, between=NULL, within=NULL, ID='subject',noFix = NULL,noRand = NULL,bar="||"){
  
  genFixForm <- function(var,drop=NULL){
    
    # one variable
    if (length(var)==1) {
      if (is.null(drop)){
        formula <- var[1]
      } else {
        formula <- "1"
      }
      
      # two variable 
    } else if (length(var)==2) {
      # include everything
      if (is.null(drop)){
        formula <- str_c(var, collapse = " * ")
        # exclude interaction
      } else if(drop == 'inter'){
        formula <- str_c(var, collapse = " + ")
        # exclude interaction and one variable
      } else if (drop %in% var){
        formula <- var[var!=drop]
        # exclude all effect
      } else if (drop == 'all') {
        formula <- '1'
      } else {
        stop("The 'drop' parameter you entered is wrong, please re-enter it")
      }
    } else {
      stop("Unfortunately, variables above two are not supported")
    }
    return (formula)
  }
  
  
  genRandForm <- function (var=NULL, ID, drop=NULL, bar="||") {
    
    # one variable
    if (is.null(var)) {
      formula <- str_glue("(1 {bar} {ID})")
      
    } else if (length(var)==1) {
      # include the variable
      if (is.null(drop)) {
        formula <- str_glue("(1 + {var[1]} {bar} {ID})")
        # exclude the variable
      } else {
        formula <- str_glue("(1 {bar} {ID})")
      }
      
      # two variable 
    } else if (length(var)==2) {
      # include everything
      if (is.null(drop)){
        formula <- str_glue('(1 + {str_c(var, collapse = " * ")}  {bar} {ID})') 
        # exclude interaction
      } else if(drop == 'inter'){
        formula <- str_glue('(1 + {str_c(var, collapse = " + ")}  {bar} {ID})') 
        # exclude interaction and one variable
      } else if (drop %in% var){
        formula <- str_glue('(1 + {var[var!=drop]}  {bar} {ID})')
        # exclude all effect
      } else if (drop == 'all') {
        formula <- str_glue("(1 {bar} {ID})")
      }  else {
        stop("The 'drop' parameter you entered is wrong, please re-enter it")
      }
    } else {
      stop("Unfortunately, variables above two are not supported")
    }
    return (formula)
  }
  
  # combine independent variables
  fix_vars <- c(between,within)

  # fix formula
  fix_from <- genFixForm(fix_vars,drop=noFix)
  # random formula
  rand_form <- genRandForm(within,ID, drop = noRand, bar = bar)
  # combine formula
  stringForm <- str_glue('{DV} ~ {fix_from} + {rand_form}')
  form <- as.formula(stringForm)
  
  return(form)
}

##### Load model #####

runModel <- function( args, formula, data, path, reload=FALSE){
  # model arguments
  model_args <- append(list(formula = formula, data = data),args)
  if (!file.exists(path) | reload){
    # run model with brms
    brm_model <- do.call(brms::brm, model_args)
    saveRDS(brm_model, file = path)
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


##### one-way/two-way BANOVA #####

STBANOVA <- function(data, DV, between=NULL, within=NULL, ID, args, name, path, bar="||", criterion=0.2, reload=FALSE) {
  
  # check the number of variable
  fixVar <- c(between,within)
  if (length(fixVar)==0) stop("You have to input at least one variable")
  
  
  # load function (in case "job" fails to load function)
  runModel = get('runModel', envir = parent.frame())
  genFormula = get("genFormula", envir = parent.frame())
  
  # set the default value for modelling
  tmpModel <- function(new_args = args, formula, filePath) {
    do.call(
      runModel,
      list(args = new_args,formula = formula,data = data, path=filePath, reload = reload))
  }
  
  # set the default value of formula for testing random effect
  testRandFrom <- function(noRand=NULL) {
    do.call(
      genFormula, 
      list(DV = DV, between=between, within = within, ID = ID, bar = bar, noFix = NULL,noRand = noRand))
  }
  
  # A variable used to store BF
  BF_list <- list()
  
  if (is.null(within)) {
    
    model_baseline <- tmpModel(
      formula = testRandFrom(noRand = "all"),
      file_path <- str_glue("{path}{name}_noRand.rds"))
    noRand <- "all"
    
  } else if (length(within)==1) {
    
    # full model
    model_full <- tmpModel(
      formula = testRandFrom(),
      filePath = str_glue("{path}{name}_full.rds"))
    
    # model without random effect
    model_noRand <- tmpModel(
      formula = testRandFrom(noRand = "all"),
      filePath = str_glue("{path}{name}_noRand.rds"))
    
    # model comparison
    BF_randVar <- bayes_factor(model_full,model_noRand)$bf
    BF_list[str_glue("Rand_{within[1]}")] <- BF_randVar
    
    if (BF_randVar > criterion) {
      model_baseline <- model_full
      noRand <-  NULL
      print_text <- "We left the random effect"
    } else {
      model_baseline <- model_noRand
      noRand <- "all"
      print_text <- "We kicked out the random effect"
    }
    
    message(str_glue("Since the BF = {BF_randVar}, {print_text}"))

    
  } else if (length(within)==2) {
    
    # full model
    model_full <- tmpModel(
      formula = testRandFrom(),
      filePath = str_glue("{path}{name}_full.rds"))
    
    # model without interaction
    model_noInter <- tmpModel(
      formula = testRandFrom(noRand = "inter"),
      filePath = str_glue("{path}{name}_noRandInter.rds"))
    
    # check the interaction in random effect
    BF_randInter <- bayes_factor(model_full,model_noInter)$bf
    BF_list["Rand_Inter"] <- BF_randInter
    
    if (BF_randInter > criterion) {
      model_baseline <- model_full
      noRand <-  NULL
      message(str_glue("Since the BF = {BF_randInter}, We left the interaction in random effect"))

    } else {
      
      message(str_glue("Since the BF = {BF_randInter}, We kicked out the interaction in random effect"))
      
      # model without main effect
      model_noRandVar1 <- tmpModel(
        formula = testRandFrom(noRand = within[1]),
        filePath = str_glue("{path}{name}_noRand{str_to_title(within[1])}.rds")
      )
      
      model_noRandVar2 <- tmpModel(
        formula = testRandFrom(noRand = within[2]),
        filePath = str_glue("{path}{name}_noRand{str_to_title(within[2])}.rds")
      )
      
      BF_randVar1 <- bayes_factor(model_noInter,model_noRandVar1)$bf
      BF_randVar2 <- bayes_factor(model_noInter,model_noRandVar2)$bf
      BF_list[str_glue("Rand_{within[1]}")] <- BF_randVar1
      BF_list[str_glue("Rand_{within[2]}")] <- BF_randVar2
      
      # check the main effect in random effect
      # leave both main effects
      if (BF_randVar1 > criterion & BF_randVar2 > criterion) {
        model_baseline <- model_noInter
        noRand <- "inter"
      # leave Var2
      } else if (BF_randVar1 <= criterion & BF_randVar2 > criterion) {
        model_baseline <- model_noRandVar2 # kick out Var1, leave Var2
        noRand <- within[1]
      # leave Var1
      } else if (BF_randVar1 > criterion & BF_randVar2 <= criterion) {
        model_baseline <- model_noRandVar1 # kick out Var2, leave Var1
        noRand <- within[2]
      # no random effect
      } else {
        model_baseline <- tmpModel(
          formula = testRandFrom(noRand = "all"),
          filePath = str_glue("{path}{name}_noRand.rds"))
        noRand <- "all"
      }
    }
  }
  
  message(str_glue("Start to test the fix effect ..."))
  
  # set the default value of formula for testing fix effect
  testFixFrom <- function(noFix=NULL) {
    do.call(
      genFormula, 
      list(DV = DV, between=between, within = within, ID = ID, bar=bar, noRand=noRand, noFix = noFix))
  }
  
  if (length(fixVar)==1) {
    
    model_noFixVar <- tmpModel(
      new_args = args[!names(args)=='prior'],
      formula = testFixFrom(noFix="all"),
      filePath = str_glue("{path}{name}_no{str_to_title(fixVar[1])}.rds"))
    
    BF_FixVar <- bayes_factor(model_baseline,model_noFixVar)$bf
    BF_list[str_glue("Fix_{fixVar[1]}")] <- BF_FixVar
    
  } else if (length(fixVar)==2) {
    
    model_noFixInter <- tmpModel(
      formula = testFixFrom(noFix="inter"),
      filePath = str_glue("{path}{name}_noInter.rds"))
    
    model_noFixVar1 <- tmpModel(
      formula = testFixFrom(noFix=fixVar[1]),
      filePath = str_glue("{path}{name}_no{str_to_title(fixVar[1])}.rds"))
    
    model_noFixVar2 <- tmpModel(
      formula = testFixFrom(noFix=fixVar[2]),
      filePath = str_glue("{path}{name}_no{str_to_title(fixVar[2])}.rds"))
    
    BF_fixInter <- bayes_factor(model_baseline,model_noFixInter)$bf
    BF_fixVar1 <- bayes_factor(model_noFixInter,model_noFixVar1)$bf
    BF_fixVar2 <- bayes_factor(model_noFixInter,model_noFixVar2)$bf
    
    BF_list[str_glue("Fix_Inter")] <- BF_fixInter
    BF_list[str_glue("Fix_{fixVar[1]}")] <- BF_fixVar1
    BF_list[str_glue("Fix_{fixVar[2]}")] <- BF_fixVar2
    
  }
  return (BF_list)
}



##### one-way/two-way BANOVA (multiple processes) #####

MTBANOVA <- function(data, DV, between=NULL, within=NULL, ID, args, name, path, bar="||", criterion=0.2, reload=FALSE) {
  
  # check the number of variable
  fixVar <- c(between,within)
  if (length(fixVar)==0) stop("You have to input at least one variable")
  
  # monitor the status of jobs
  monitorJob("reset",path)
  
  # set the default value for modelling
  saveModel <- function(new_args = args, formula, filePath) {
    
    path <- path
    data <- data
    reload <- reload
    
    job::job({
      
      numPath <- str_glue("{path}monitor_number.rds")
      
      # start
      start_num <- readRDS(numPath)
      start_num = start_num + 1
      saveRDS(start_num, numPath)
      print(str_glue("The monitor number is: {start_num}"))
      
      # running model
      model_args <- append(list(formula = formula, data = data),new_args)
      if (!file.exists(filePath) | reload){
        # run model with brms
        brm_model <- do.call(brms::brm, model_args)
        saveRDS(brm_model, file = filePath)
      } 
      
      # check whether the data has been saved
      while (!file.exists(filePath)) {
        Sys.sleep(2)
      }
      
      # stop
      stop_num <- readRDS(numPath)
      stop_num = stop_num - 1
      saveRDS(stop_num, numPath)
      print(str_glue("The monitor number is: {stop_num}"))
      
    })
    
    # the time used to launch "job"
    Sys.sleep(10)
    
  }
  
  # set the default value of formula for testing random effect
  testRandFrom <- function(noRand=NULL) {
    do.call(
      genFormula, 
      list(DV = DV, between=between, within = within, ID = ID, bar = bar, noFix = NULL,noRand = noRand))
  }
  
  # A variable used to store BF
  BF_list <- list()
  
  if (is.null(within)) {
    
    saveModel(
      formula = testRandFrom(noRand = "all"),
      file_path = str_glue("{path}{name}_noRand.rds")
      )
    noRand <- "all"
    
    while (monitorJob("read",path)>0) {Sys.sleep(3)}

    model_baseline <- readRDS(str_glue("{path}{name}_noRand.rds"))
    
  } else if (length(within)==1) {
    
    # full model
    saveModel(
      formula = testRandFrom(),
      filePath = str_glue("{path}{name}_full.rds"))
    
    # model without random effect
    saveModel(
      formula = testRandFrom(noRand = "all"),
      filePath = str_glue("{path}{name}_noRand.rds"))
    
    while (monitorJob("read",path)>0) {
      Sys.sleep(3)}
    
    
    model_full <- readRDS(str_glue("{path}{name}_full.rds"))
    model_noRand <- readRDS(str_glue("{path}{name}_noRand.rds"))
    
    # model comparison
    BF_randVar <- bayes_factor(model_full,model_noRand)$bf
    BF_list[str_glue("Rand_{within[1]}")] <- BF_randVar
    
    if (BF_randVar > criterion) {
      model_baseline <- model_full
      noRand <-  NULL
      print_text <- "We left the random effect"
    } else {
      model_baseline <- model_noRand
      noRand <- "all"
      print_text <- "We kicked out the random effect"
    }
    
    message(str_glue("Since the BF = {BF_randVar}, {print_text}"))
    
    
  } else if (length(within)==2) {
    
    # full model
    saveModel(
      formula = testRandFrom(),
      filePath = str_glue("{path}{name}_full.rds"))
    
    # model without interaction
    saveModel(
      formula = testRandFrom(noRand = "inter"),
      filePath = str_glue("{path}{name}_noRandInter.rds"))
    
    while (monitorJob("read",path)>0) {
      Sys.sleep(3)
    }
    
    model_full <- readRDS(str_glue("{path}{name}_full.rds"))
    model_noInter <- readRDS(str_glue("{path}{name}_noRandInter.rds"))
    
    # check the interaction in random effect
    BF_randInter <- bayes_factor(model_full,model_noInter)$bf
    BF_list["Rand_Inter"] <- BF_randInter
    
    if (BF_randInter > criterion) {
      model_baseline <- model_full
      noRand <-  NULL
      message(str_glue("Since the BF = {BF_randInter}, We left the interaction in random effect"))
      
    } else {
      
      message(str_glue("Since the BF = {BF_randInter}, We kicked out the interaction in random effect"))
      
      # model without main effect
      saveModel(
        formula = testRandFrom(noRand = within[1]),
        filePath = str_glue("{path}{name}_noRand{str_to_title(within[1])}.rds")
      )
      
      saveModel(
        formula = testRandFrom(noRand = within[2]),
        filePath = str_glue("{path}{name}_noRand{str_to_title(within[2])}.rds")
      )
      
      while (monitorJob("read",path)>0) {Sys.sleep(3)}
      
      
      model_noRandVar1 <- readRDS(str_glue("{path}{name}_noRand{str_to_title(within[1])}.rds"))
      model_noRandVar2 <- readRDS(str_glue("{path}{name}_noRand{str_to_title(within[2])}.rds"))
      
      BF_randVar1 <- bayes_factor(model_noInter,model_noRandVar1)$bf
      BF_randVar2 <- bayes_factor(model_noInter,model_noRandVar2)$bf
      BF_list[str_glue("Rand_{within[1]}")] <- BF_randVar1
      BF_list[str_glue("Rand_{within[2]}")] <- BF_randVar2
      
      # check the main effect in random effect
      # leave both main effects
      if (BF_randVar1 > criterion & BF_randVar2 > criterion) {
        model_baseline <- model_noInter
        noRand <- "inter"
        # leave Var2
      } else if (BF_randVar1 <= criterion & BF_randVar2 > criterion) {
        model_baseline <- model_noRandVar2 # kick out Var1, leave Var2
        noRand <- within[1]
        # leave Var1
      } else if (BF_randVar1 > criterion & BF_randVar2 <= criterion) {
        model_baseline <- model_noRandVar1 # kick out Var2, leave Var1
        noRand <- within[2]
        # no random effect
      } else {
        saveModel(
          formula = testRandFrom(noRand = "all"),
          filePath = str_glue("{path}{name}_noRand.rds"))
        noRand <- "all"
      }
    }
  }
  
  message(str_glue("Start to test the fix effect ..."))
  
  # set the default value of formula for testing fix effect
  testFixFrom <- function(noFix=NULL) {
    do.call(
      genFormula, 
      list(DV = DV, between=between, within = within, ID = ID, bar=bar, noRand=noRand, noFix = noFix))
  }
  
  if (length(fixVar)==1) {
    
    saveModel(
      new_args = args[!names(args)=='prior'],
      formula = testFixFrom(noFix="all"),
      filePath = str_glue("{path}{name}_no{str_to_title(fixVar[1])}.rds"))
    
    while (monitorJob("read",path)>0) {Sys.sleep(3)}
    
    model_noFixVar <- readRDS(str_glue("{path}{name}_no{str_to_title(fixVar[1])}.rds"))
    
    BF_FixVar <- bayes_factor(model_baseline,model_noFixVar)$bf
    BF_list[str_glue("Fix_{fixVar[1]}")] <- BF_FixVar
    
  } else if (length(fixVar)==2) {
    
    saveModel(
      formula = testFixFrom(noFix="inter"),
      filePath = str_glue("{path}{name}_noInter.rds"))
    
    saveModel(
      formula = testFixFrom(noFix=fixVar[1]),
      filePath = str_glue("{path}{name}_no{str_to_title(fixVar[1])}.rds"))
    
    saveModel(
      formula = testFixFrom(noFix=fixVar[2]),
      filePath = str_glue("{path}{name}_no{str_to_title(fixVar[2])}.rds"))
    
    while (monitorJob("read",path)>0) {Sys.sleep(3)}
    
    if(!exists("model_baseline")) model_baseline <- readRDS(str_glue("{path}{name}_noRand.rds"))
    model_noFixInter <- readRDS(str_glue("{path}{name}_noInter.rds"))
    model_noFixVar1 <- readRDS(str_glue("{path}{name}_no{str_to_title(fixVar[1])}.rds"))
    model_noFixVar2 <- readRDS(str_glue("{path}{name}_no{str_to_title(fixVar[2])}.rds"))
    
    BF_fixInter <- bayes_factor(model_baseline,model_noFixInter)$bf
    BF_fixVar1 <- bayes_factor(model_noFixInter,model_noFixVar1)$bf
    BF_fixVar2 <- bayes_factor(model_noFixInter,model_noFixVar2)$bf
    
    BF_list[str_glue("Fix_Inter")] <- BF_fixInter
    BF_list[str_glue("Fix_{fixVar[1]}")] <- BF_fixVar1
    BF_list[str_glue("Fix_{fixVar[2]}")] <- BF_fixVar2
    
  }
  return (BF_list)
}

