

##### aggregate table #####
summarySE <-
  function(.data, DV, between = NULL, within = NULL, ID = NULL, CI = 0.95) {
    
    ### first aggregate the data on the level of participants
    df <- .data %>%
      dplyr::group_by(dplyr::across(c({{ID}}, {{between}}, {{within}}))) %>%
      dplyr::summarize(sub_DV = base::mean({{DV}}, na.rm = T)) %>% #When you have an env-variable that is a character vector
      dplyr::ungroup()
    
    ### adjust the data to compute the within-subject se
    if (!(is.null(within))) {
      df <- df %>%
        dplyr::group_by(dplyr::across(c({{ID}}, {{between}}))) %>%
        dplyr::mutate(user_mean = base::mean(sub_DV, na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(dplyr::across(c({{between}}))) %>%
        dplyr::mutate(grand_mean = base::mean(sub_DV, na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(DV_adjusted = sub_DV - user_mean + grand_mean)
    } else{
      df <- df %>%
        dplyr::mutate(DV_adjusted = sub_DV)
    }
    
    ### aggregate data on level of plot
    df <- df %>%
      dplyr::group_by(dplyr::across(c({{between}}, {{within}}))) %>%
      dplyr::summarize(
        mean = base::mean(DV_adjusted, na.rm = TRUE),
        n = dplyr::n(),
        se = stats::sd(DV_adjusted, na.rm = TRUE) / base::sqrt(n),
        ci = stats::qt(1 - (1 - CI) / 2, n - 1) * se,
      ) %>%
      dplyr::ungroup()
    
    ### add the Morey-correction to the se
    if (!(is.null(within))) {
      if (!(is.null(between))) {
        df <- df %>%
          dplyr::group_by(dplyr::across(c({{between}}))) %>%
          dplyr::mutate(
            morey_correction = base::sqrt(dplyr::n() / (dplyr::n() - 1)),
            se = se * morey_correction
          ) %>%
          dplyr::ungroup()
      } else{
        df <- df %>%
          dplyr::mutate(
            morey_correction = base::sqrt(dplyr::n() / (dplyr::n() - 1)),
            se = se * morey_correction
          )
      }
      df <- select(df,!morey_correction)
    }
    
    ### return df
    return(df)
  }

##### Default setting for all plots #####

theme_myDefault <- function(fontSize = 10, facet="grey", gridLines=TRUE) {
  
  theme_set(theme_bw())
  
  if (facet == "black") {
    theme_update(
      ### black background for facet panels
      strip.background = element_rect(color="black", fill="gray25", linetype="solid"),
      ### white color of facet labels
      strip.text = element_text(color = "white"))
  }
  
  ### set font size for facet labels
  theme_update(
    strip.text.x = element_text(size = fontSize),
    strip.text.y = element_text(size = fontSize)
  )
  
  ### remove grid lines from panel
  if(!gridLines){
    theme_update(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank())
  }
}

