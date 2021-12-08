

summarySE <- function(.data, DV, between = NULL, within = NULL, ID = NULL, CI = 0.95, DV_prefix=NULL){
  
  ### first aggregate the data on the level of participants
  df <- .data %>% 
    dplyr::group_by(across(c({{ ID }}, {{ between }}, {{ within }}))) %>%
    dplyr::summarise(sub_DV = base::mean({{DV}}, na.rm=T)) %>% #When you have an env-variable that is a character vector
    dplyr::ungroup()
  
  ### adjust the data to compute the within-subject se
  if(!(missing(within))){
    df <- df %>% 
      dplyr::group_by(dplyr::across(c({{ ID }}, {{ between }}))) %>%
      dplyr::mutate(user_mean = base::mean(sub_DV, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by({{ between }}) %>%
      dplyr::mutate(grand_mean = base::mean(sub_DV, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(DV_adjusted = sub_DV - user_mean + grand_mean)
  }
  
  ###aggregate data on level of plot
  df <- df %>% 
    dplyr::group_by(across(c({{ between }}, {{ within }}))) %>%
    dplyr::summarize(
      mean = base::mean(DV_adjusted, na.rm = TRUE),
      n = dplyr::n(),
      se = stats::sd(DV_adjusted, na.rm = TRUE)/base::sqrt(n),
      ci = stats::qt(1-(1-CI)/2, n-1)*se,
    ) %>%
    dplyr::ungroup() %>% 
    rename()
  
  ###add the Morey-correction to the se
  if(!(missing(within))){
    if(!(missing(between))){
      df <- df %>% 
        dplyr::group_by({{ between }}) %>%
        dplyr::mutate(se = se * base::sqrt(dplyr::n()/(dplyr::n()-1))) %>%
        dplyr::ungroup()
    }else{
      df <- df %>% 
        dplyr::mutate(se = se * base::sqrt(dplyr::n()/(dplyr::n()-1)))
    }
    
    ### return df
    return(df)
  }}
