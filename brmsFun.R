

run_brm <- function(formula,pars){
  
  # set the default prior
  CauchyScale <- sqrt(2)/4 
  Cauchy <- paste0("cauchy(0,", CauchyScale, ")")
  fixefPrior <- set_prior(Cauchy, class="b")  
  ranefPrior <- set_prior("gamma(1,0.01)", class="sd")
  
  brm_model <- brm(
    formula = formula,
    family = pars$family,
    data = pars$data,
    prior = c(pars$fixefPrior, pars$ranefPrior),
    chains = pars$chains,
    warmup = pars$warmup,
    iter = pars$iter,
    cores = pars$cores,
    control = pars$control,
    save_pars = pars$save_pars
  )
  return(brm_model)
}