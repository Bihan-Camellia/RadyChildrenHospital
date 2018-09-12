# staff care need calculation
calcStaff <- function(df){
  smooth <- df$smooth
  smooth <- c(smooth[23:24], smooth)
  staff <- rep(NA, 26)
  
  for(i in 1:length(smooth)){
    staff[i] <-max(smooth[i], smooth[i+1], smooth[i+2])
  }
  return(staff[1:24])
}


make_plot <- function(df, plan = 'staffingPlanH', need = 'staffingNeedH'){
  df %>% ggplot(aes(x = hour)) +
    geom_col(aes_string(y = need), fill = '#3c8dbc') +
    geom_errorbar(aes_string(ymax = plan, ymin = plan), color = 'red', size = 3) +
    # geom_text(aes_string(y = type, label = round(staffingNeedH, 2)), vjust = -0.5, size = 5) +
    theme_bw() +
    labs(x = NULL, y = NULL) +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank()) +
    # scale_fill_brewer(palette = 'Set2') +
    # scale_color_brewer(palette = 'Set2') +
    scale_y_continuous(limits = c(0,12),
                       breaks = seq(0,12,2))
}

make_plot_linear <- function(df, plan = 'staffingPlanH', need = 'staffingNeedH'){
  df %>% ggplot(aes(x = hour)) +
    geom_col(aes_string(y = need), fill = '#3c8dbc') +
    geom_errorbar(aes_string(ymax = plan, ymin = plan), color = 'red', size = 3) +
    # geom_text(aes_string(y = type, label = round(staffingNeedH, 2)), vjust = -0.5, size = 5) +
    theme_bw() +
    labs(x = NULL, y = NULL) +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank()) +
    # scale_fill_brewer(palette = 'Set2') +
    # scale_color_brewer(palette = 'Set2') +
    scale_y_continuous(limits = c(0,12),
                       breaks = seq(0,12,2))
}

get_high_slot <- function(season, weekday){
  if(season == 'High'){
    if(weekday == 'Sun/Mon'){"hhh"} 
    else{"hlh"}
  } else if(season == 'Medium'){
    if(weekday == 'Sun/Mon'){"mhh"} 
    else {"mlh"} 
  } else {
    if(weekday == 'Sun/Mon'){"lhh"} 
    else {"llh"}
  }
}

get_low_slot <- function(season, weekday){
  if(season == 'High'){
    if(weekday == 'Sun/Mon'){"hhl"} 
    else{"hll"}
  } else if(season == 'Medium'){
    if(weekday == 'Sun/Mon'){"mhl"} 
    else {"mll"} 
  } else {
    if(weekday == 'Sun/Mon'){"lhl"} 
    else {"lll"}
  }
}

updateDB <- function(editedValue, pool, tbl){
  editedValue <- editedValue %>% 
    group_by(row, col) %>% 
    filter(value == dplyr::last(value)|is.na(value)) %>% 
    ungroup()
  
  conn <- poolCheckout(pool)
  
  lapply(seq_len(nrow(editedValue)), function(i){
    id = editedValue$row[i]
    col = dbListFields(pool, tbl)[editedValue$col[i]]
    value = editedValue$value[i]
    
    query <- glue_sql('UPDATE {`tbl`} SET {`col`} = {value}
                      WHERE id = {id}', .con = conn)
  })
  
  poolReturn(conn)
  print(editedValue)
  return(invisible())
}




