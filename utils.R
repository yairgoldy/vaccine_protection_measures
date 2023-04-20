


calcualte_measures <- function(dat,analysis,calculate_CI = T){
  
  cohorts <- dat$Cohort %>% levels()
  df <- broom::tidy(analysis) %>%
    filter(str_starts(string = term,"Cohort")) %>% 
    select(Cohort = term, Coef = estimate, std.error) %>%
    add_row(Cohort = cohorts[1], Coef = 0, std.error = 0) %>% 
    mutate(Cohort = str_remove(Cohort,"Cohort"),
           Cohort = factor(Cohort,levels = cohorts)) %>% 
    arrange(Cohort)
  
  absolute_measures <- dat %>% 
    calc_absolute_measures(analysis,calculate_CI = calculate_CI)
  
  relative_measures <- df %>% calc_relative_measures()
  
  df <- df %>% 
    left_join(absolute_measures, by = "Cohort") %>% 
    left_join(relative_measures, by = c("Cohort","Coef", "std.error"))
  # 
  # df <- df %>% 
  #   select(Cohort, `Adjusted Number of Cases` = AR, `Relative VE`, 
  #          `Risk Ratio`,`Protection Risk Ratio`,`Absolute Difference`) %>% 
  #   mutate(across( `Relative VE`:`Absolute Difference`,  function(x){x[1] <- "Reference"; return(x)}))
  # 
  df <- df %>% 
    select(Cohort, `Adjusted Number of Cases` = AR, `Adjusted Difference`,
           `Risk Ratio`,`Protection Risk Ratio`,`Relative VE` 
           ) %>% 
    mutate(across( `Relative VE`:`Adjusted Difference`,  function(x){x[1] <- "Reference"; return(x)}))
  
  
  return(df)
}

calc_relative_measures <- function(df){
  
  #Relative VE
  df <- df %>% 
    mutate(
      est =  round( (1-exp(-Coef))*100,1),
      lower = round( (1-exp(-Coef+1.96*std.error))*100,0),
      upper = round( (1-exp(-Coef-1.96*std.error))*100,0),
      `Relative VE` = paste0(est,"% \\\n[",lower,"%, ",upper,"%]")
    ) 
  
  #Risk Ratio
  df <- df %>% 
    mutate(
      est =  round( exp(-Coef)*1,3),
      lower = round( exp(-Coef-1.96*std.error)*1,2),
      upper = round( exp(-Coef+1.96*std.error)*1,2),
      `Risk Ratio` = paste0(est,"\\\n[",lower,", ",upper,"]")
    ) %>% 
    select(- est,-lower,-upper)
  
  #Protection Risk Ratio
  df <- df %>% 
    mutate(
      est =  round( exp(Coef),1),
      lower = round( exp(Coef-1.96*std.error),0),
      upper = round( exp(Coef+1.96*std.error),0),
      `Protection Risk Ratio` = paste0(est,"\\\n[",lower,", ",upper,"]")
    ) %>% 
    select(- est,-lower,-upper)
  
}











calc_absolute_measures <- function(dat,analysis,B =1000,calculate_CI = T){
  
  cohorts <- dat$Cohort %>% levels()
  est <- adjusted_rates(dat, analysis, cohorts, index=0 ) %>% 
    mutate(
      Cohort = cohorts,
      # `Absolute Difference` = round(AR - AR[1],1)
      `Adjusted Difference` = round(AR - AR[1],1)
    )
  
  if(calculate_CI){
    boot_dat <- tibble(index = 1:B) %>% 
      mutate(res = map(index, function(ind){adjusted_rates(dat,analysis, cohorts, index=ind )})) %>% 
      unnest(cols = c(res))  %>% 
      group_by(index) %>%
      mutate(
        `Adjusted Difference` = AR - AR[1]
      ) %>% 
      ungroup()
    
    
    
    
    CI_est <- boot_dat %>%
      group_by(Cohort) %>% 
      summarise(
        lower = round(quantile(AR,probs=c(0.025),na.rm = TRUE),1),
        upper = round(quantile(AR,probs=c(0.975),na.rm = TRUE),1),
        lowerD = round(quantile(`Adjusted Difference`,probs=c(0.025),na.rm = TRUE),1),
        upperD = round(quantile(`Adjusted Difference`,probs=c(0.975),na.rm = TRUE),1)
      ) %>% 
      ungroup()
    
    est <- est %>% 
      left_join(CI_est,by = c("Cohort")) %>% 
      mutate(
        AR = paste0( round(AR,1),"\\\n[",lower,", ",upper,"]"),
        `Adjusted Difference` = paste0( round(`Adjusted Difference`,1),"\\\n[",lowerD,", ",upperD,"]")
      ) %>% 
      select(Cohort,AR,`Adjusted Difference`)
  }
  return(est)
  
}
adjusted_rates <- function(dat, analysis, cohorts, index= 0){
  
  Sigma <- vcov(analysis)
  Beta <-  coef(analysis)
  Beta <-  replace_na(Beta,0)
  Sigma <-  replace_na(Sigma,0)
  
  
  if(index >=1){
    set.seed(index)
    analysis$coefficients <-  rmvnorm(n = 1,mean = Beta, sigma = Sigma)
  }
  
  adj_rate <- NULL
  for(cohort_i in cohorts){
    ari <- dat %>% 
      mutate(AR = dat %>%
               mutate(Cohort = cohort_i) %>%  
               predict(object = analysis,type = "response"))%>% 
      summarise(AR = 100000*sum(AR)/sum(N_person)) %>% 
      mutate(Cohort = cohort_i)
    adj_rate <- rbind(adj_rate,ari)
  }
  
  if(index == 0) {
    adj_rate <- adj_rate %>% 
      mutate(AR = round(AR,2))
  }
  
  
  return(adj_rate)
}  

