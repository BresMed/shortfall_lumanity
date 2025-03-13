# compute QALE
compQale = function(
    ons_df,                   
    prop_female = 0.5, 
    start_age = 50, 
    disc_rate = 0.035,
    utils = "cw",
    cycle_length_days = 365.25,
    disc_adjust = 0
    ){
  
  compQaleInternal = function(
    ons_df,                   
    prop_female = 0.5, 
    start_age = 50, 
    disc_rate = 0.035,
    utils = "cw",
    cycle_length_days = 365.25,
    disc_adjust = 0
    ){
    
    ons_df = ons_df[ons_df$age >= round(start_age, digits = 0),] #Lumanity edit - add rounding function
    ons_df = ons_df[order(ons_df$age),]
    df_female = ons_df[ons_df$sex == "female",c("age",utils,"lx","dx","mx","ex")]
    df_male = ons_df[ons_df$sex == "male",c("age",utils,"lx","dx","mx","ex")]
    
    df_comp = data.frame(
      age = df_female$age,
      age_table_index = df_female$age + 1,
      utils = (1-prop_female) * df_male[,utils]  + prop_female * df_female[,utils],
      lx = (1-prop_female) * df_male$lx  + prop_female * df_female$lx,
      dx = (1-prop_female) * df_male$dx  + prop_female * df_female$dx,
      mx = (1-prop_female) * df_male$mx  + prop_female * df_female$mx,
      ex = (1-prop_female) * df_male$ex  + prop_female * df_female$ex
    )
    
    ##LUMANITY ADD
    
    cycle_length_years <- cycle_length_days / 365.25

    df_comp = CalcByCycle(
      df = df_comp,
      cycle_length_days = cycle_length_days,
      start_age = start_age
    )
    

    # person years in year i
    df_comp$Lx = NA
    for(i in 2:nrow(df_comp)){
      df_comp$Lx[i-1] = df_comp$lx[i] + (0.5 * df_comp$dx[i-1])
    }
    df_comp$Lx[nrow(df_comp)] = (df_comp$lx[nrow(df_comp)]-df_comp$dx[nrow(df_comp)]) + (0.5 * df_comp$dx[nrow(df_comp)])
    
    # person QALYs in year i
    df_comp$Yx = df_comp$utils * df_comp$Lx * cycle_length_years
    
    # apply discounting
    v_disc <- 1/(1+disc_rate)^((0:(length(df_comp$Yx)-1)+disc_adjust)*cycle_length_years)
    df_comp$Yx = df_comp$Yx * v_disc
    
    # remaining person QALYs?
    df_comp$Nx = NA
    df_comp$Nx[nrow(df_comp)] = df_comp$Yx[nrow(df_comp)]
    for(i in nrow(df_comp):2){
      df_comp$Nx[i-1] = df_comp$Yx[i-1] + df_comp$Nx[i]
    }
    
    # Quality adjusted life expectancy 
    df_comp$Qx = df_comp$Nx / df_comp$lx
    
    
    q_factor = sum(df_comp$Yx) / df_comp$Qx[1]
    
    df_comp$qalys_by_year = df_comp$Yx/q_factor 
    df_comp$cumulative_qalys = cumsum(df_comp$qalys_by_year)
    
    # cumulative survival function
    df_comp$S = 1-df_comp$mx
    df_comp$S_cumulativ =  cumprod(df_comp$S)
    df_comp$hrqol = df_comp$utils
    
    df_comp = df_comp[,c("age","hrqol","ex","Qx","S_cumulativ","cumulative_qalys")]
    
    return(df_comp)
    
  }

  
  qale_male = compQaleInternal(
    ons_df = ons_df,                   
    prop_female = 0, 
    start_age = start_age, 
    disc_rate = disc_rate,
    utils = utils,
    cycle_length_days = cycle_length_days,
    disc_adjust = disc_adjust
  )
  qale_female = compQaleInternal(
    ons_df = ons_df,                   
    prop_female = 1, 
    start_age = start_age, 
    disc_rate = disc_rate,
    utils = utils,
    cycle_length_days = cycle_length_days,
    disc_adjust = disc_adjust
  )
  qale_mix = qale_male * (1-prop_female) + qale_female * prop_female
  
  return(qale_mix)
  
}