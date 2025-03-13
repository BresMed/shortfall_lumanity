### LUMANITY NEW

CalcByCycle = function(
  df = df_comp,
  cycle_length_days = cycle_length_days,
  start_age = start_age
  ){
  
  cycle_length_years <- cycle_length_days / 365.25
  cycles_per_year <- 1 / cycle_length_years
  total_cycles <- ceiling(cycles_per_year * (100 - start_age))
  
  df_comp_by_cycle <- as.data.frame(matrix(0, ncol = 1, nrow = total_cycles))
  
  df_comp_by_cycle$age[1] = start_age
  
  for (i in 2:total_cycles){
    df_comp_by_cycle$age[i] = df_comp_by_cycle$age[i-1] + cycle_length_years
  }
  
  start_age_rounded <- round(start_age, digits = 0)
  
  for (i in 1:total_cycles){
    
    age_table_index <- round(df_comp_by_cycle$age[i], digits = 0) - start_age_rounded + 1
    df_comp_by_cycle$age_table_index[i] = age_table_index
    df_comp_by_cycle$utils[i] = df$utils[age_table_index]
    df_comp_by_cycle$lx[i] = df$lx[age_table_index]
    df_comp_by_cycle$dx[i] = df$dx[age_table_index] * cycle_length_years
    df_comp_by_cycle$mx[i] = df$mx[age_table_index]
    df_comp_by_cycle$ex[i] = df$ex[age_table_index]
  }
  
  for(i in 2:nrow(df_comp_by_cycle)){
    df_comp_by_cycle$lx[i] = df_comp_by_cycle$lx[i-1] - df_comp_by_cycle$dx[i-1]
  }

  return(df_comp_by_cycle)
  
  }