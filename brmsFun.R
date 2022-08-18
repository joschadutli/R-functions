
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

load_model <- function( args, formula, data, name, path, reload=FALSE){
  # path
  file_path <- str_glue("{path}{name}.rda")
  # model arguments
  model_args <- append(list(formula = formula, data = data),args)
  if (!file.exists(file_path) | reload){
    # run model with brms
    brm_model <- do.call(brms::brm, model_args)
    save(brm_model, file = file_path, compress = "xz")
  } else {
    # load model from local file
    load(file = file_path)
  }
  return(brm_model)
}


##### one-way/two-way BANOVA #####

brmsBANOVA <- function(data, DV, between=NULL, within=NULL, ID, args, name, path, bar="||", criterion=0.2, reload=FALSE) {
  
  # check the number of variable
  fixVar <- c(between,within)
  if (length(fixVar)==0) stop("You have to input at least one variable")
  
  
  # load function (in case "job" fails to load function)
  load_model = get('load_model', envir = parent.frame())
  genFormula = get("genFormula", envir = parent.frame())
  
  # set the default value for modelling
  runModel <- function(new_args = args, formula, name) {
    do.call(
      load_model,
      list(args = new_args,formula = formula,data = data,path=path,name = name, reload = reload))
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
    
    model_baseline <- runModel(
      formula = testRandFrom(noRand = "all"),
      name = str_glue("{name}_noRand"))
    noRand <- "all"
    
  } else if (length(within)==1) {
    
    # full model
    model_full <- runModel(
      formula = testRandFrom(),
      name = str_glue("{name}_full"))
    
    # model without random effect
    model_noRand <- runModel(
      formula = testRandFrom(noRand = "all"),
      name = str_glue("{name}_noRand"))
    
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
    model_full <- runModel(
      formula = testRandFrom(),
      name = str_glue("{name}_full"))
    
    # model without interaction
    model_noInter <- runModel(
      formula = testRandFrom(noRand = "inter"),
      name = str_glue("{name}_noRandInter"))
    
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
      model_noRandVar1 <- runModel(
        formula = testRandFrom(noRand = within[1]),
        name = str_glue("{name}_noRand{str_to_title(within[1])}")
      )
      
      model_noRandVar2 <- runModel(
        formula = testRandFrom(noRand = within[2]),
        name = str_glue("{name}_noRand{str_to_title(within[2])}")
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
        model_baseline <- runModel(
          formula = testRandFrom(noRand = "all"),
          name = str_glue("{name}_noRand"))
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
    
    model_noFixVar <- runModel(
      new_args = args[!names(args)=='prior'],
      formula = testFixFrom(noFix="all"),
      name = str_glue("{name}_no{str_to_title(fixVar[1])}"))
    
    BF_FixVar <- bayes_factor(model_baseline,model_noFixVar)$bf
    BF_list[str_glue("Fix_{fixVar[1]}")] <- BF_FixVar
    
  } else if (length(fixVar)==2) {
    
    model_noFixInter <- runModel(
      formula = testFixFrom(noFix="inter"),
      name = str_glue("{name}_noInter"))
    
    model_noFixVar1 <- runModel(
      formula = testFixFrom(noFix=fixVar[1]),
      name = str_glue("{name}_no{str_to_title(fixVar[1])}"))
    
    model_noFixVar2 <- runModel(
      formula = testFixFrom(noFix=fixVar[2]),
      name = str_glue("{name}_no{str_to_title(fixVar[2])}"))
    
    BF_fixInter <- bayes_factor(model_baseline,model_noFixInter)$bf
    BF_fixVar1 <- bayes_factor(model_noFixInter,model_noFixVar1)$bf
    BF_fixVar2 <- bayes_factor(model_noFixInter,model_noFixVar2)$bf
    
    BF_list[str_glue("Fix_Inter")] <- BF_fixInter
    BF_list[str_glue("Fix_{fixVar[1]}")] <- BF_fixVar1
    BF_list[str_glue("Fix_{fixVar[2]}")] <- BF_fixVar2
    
  }
  return (BF_list)
}




