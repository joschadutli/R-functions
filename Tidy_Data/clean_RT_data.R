

clean_RT_data <- function(
    .data, 
    RT_var = "RT",
    group, 
    RT_range = c(0.2, 20)) {
  
  results <- .data %>% 
    filter((across({{RT_var}}) > RT_range[1] & across({{RT_var}}) < RT_range[2])) %>% 
    group_by(across({{group}})) %>% 
    filter(abs(scale(across({{RT_var}}))) < 3) %>% 
    ungroup()
  
  return (results)
}
