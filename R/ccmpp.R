## CCMPP that spreads net migrants uniformly throughout each period
ccmpp_migr_mid = function(par) {
  n_dim = dim(par$migr)
  n_yrs = n_dim[1]
  n_sex = n_dim[2]
  n_age = n_dim[3]
  
  lab_sex = c("Male", "Female")
  lab_age = c(0:79, "80+")
  
  pop    = array(NA, dim=c(n_yrs, n_sex, n_age), dimnames=list(Year=par$years, Sex=lab_sex, Age=lab_age))
  births = array(NA, dim=c(n_yrs, n_sex),        dimnames=list(Year=par$years, Sex=lab_sex))
  deaths = array(NA, dim=c(n_yrs, n_sex, n_age), dimnames=list(Year=par$years, Sex=lab_sex, Age=lab_age))
  
  pop[1,,] = par$basepop
  btotal = sum(0.5 * (pop[1,2,16:50] + pop[1,2,15:49] * par$Sx[1,2,15:49]) * par$tfr[1] * par$pasfr[1,] / sum(par$pasfr[1,]))
  births[1,1] = btotal * par$srb[1] / (100.0 + par$srb[1])
  births[1,2] = btotal - births[1,1]
  
  for (k in 2:length(par$years)) {
    # ages 1-79 -> 2-80
    mr = 0.5 * (par$migr[k,,2:n_age] + par$migr[k,,1:(n_age-1)])
    deaths[k,,2:n_age] = (pop[k-1,,1:(n_age-1)] + 0.5 * mr) * (1.0 - par$Sx[k,,2:n_age])
    pop[k,,2:n_age] = pop[k-1,,1:(n_age-1)] * par$Sx[k,,2:n_age] + 0.5 * mr * (1.0 + par$Sx[k,,2:n_age])
    
    # ages 80 -> 80+
    mr = 0.5 * (par$migr[k,,n_age-1] + par$migr[k,,n_age])
    deaths[k,,n_age] = deaths[k,,n_age] + (pop[k-1,,n_age] + 0.5 * mr) * (1.0 - par$Sx[k,,n_age+1])
    pop[k,,n_age] = pop[k,,n_age] + pop[k-1,,n_age] * par$Sx[k,,n_age+1] + 0.5 * mr * (1.0 + par$Sx[k,,n_age+1])
    
    # birth to age 0
    mr = 0.5 * par$migr[k,,1]
    btotal = sum(0.5 * (pop[k,2,16:50] + pop[k-1,2,16:50]) * par$tfr[k] * par$pasfr[k,]) / sum(par$pasfr[k,])
    births[k,1] = btotal * par$srb[k] / (100.0 + par$srb[k])
    births[k,2] = btotal - births[k,1]
    deaths[k,,1] = births[k,] * (1.0 - par$Sx[k,,1]) + mr * (2.0 * (1.0 - par$Sx[k,,1])) / 3.0
    pop[k,,1] = births[k,] * par$Sx[k,,1] + mr * (1.0 + 2 * par$Sx[k,,1]) / 3.0
  }
  
  rval = list(pop=pop, births=births, deaths=deaths)
  return(rval)
}

## CCMPP that adds net migrants at the end of each period
ccmpp_migr_mid = function(par) {
  n_dim = dim(par$migr)
  n_yrs = n_dim[1]
  n_sex = n_dim[2]
  n_age = n_dim[3]
  
  lab_sex = c("Male", "Female")
  lab_age = c(0:79, "80+")
  
  pop    = array(NA, dim=c(n_yrs, n_sex, n_age), dimnames=list(Year=par$years, Sex=lab_sex, Age=lab_age))
  births = array(NA, dim=c(n_yrs, n_sex),        dimnames=list(Year=par$years, Sex=lab_sex))
  deaths = array(NA, dim=c(n_yrs, n_sex, n_age), dimnames=list(Year=par$years, Sex=lab_sex, Age=lab_age))
  
  pop[1,,] = par$basepop
  btotal = sum(0.5 * (pop[1,2,16:50] + pop[1,2,15:49] * par$Sx[1,2,15:49]) * par$tfr[1] * par$pasfr[1,] / sum(par$pasfr[1,]))
  births[1,1] = btotal * par$srb[1] / (100.0 + par$srb[1])
  births[1,2] = btotal - births[1,1]

  for (k in 2:length(par$years)) {
    # ages 1-79 -> 2-80
    deaths[k,,2:n_age] = pop[k-1,,1:(n_age-1)] * (1.0 - par$Sx[k,,2:n_age])
    pop[k,,2:n_age] = pop[k-1,,1:(n_age-1)] * par$Sx[k,,2:n_age]
    
    # ages 80 -> 80+
    deaths[k,,n_age] = deaths[k,,n_age] + pop[k-1,,n_age] * (1.0 - par$Sx[k,,n_age+1])
    pop[k,,n_age] = pop[k,,n_age] + pop[k-1,,n_age] * par$Sx[k,,n_age+1]
    
    # birth to age 0
    btotal = sum(0.5 * (pop[k,2,16:50] + pop[k-1,2,16:50]) * par$tfr[k] * par$pasfr[k,]) / sum(par$pasfr[k,])
    births[k,1] = btotal * par$srb[k] / (100.0 + par$srb[k])
    births[k,2] = btotal - births[k,1]
    deaths[k,,1] = births[k,] * (1.0 - par$Sx[k,,1])
    pop[k,,1] = births[k,] * par$Sx[k,,1]
    
    pop[k,,] = pop[k,,] + par$migr[k,,]
  }
  
  rval = list(pop=pop, births=births, deaths=deaths)
  return(rval)
}

#' Calculate a demographic projection
#' 
#' @param par 
demproj = function(par, proj.method, spec.fert=FALSE) {
  years = par$tfr$year
  year_first = min(years)
  year_final = max(years)
  
  n_yrs = length(years)
  n_sex = 2
  n_age = 81
  n_age_fert = 49-15+1
  
  lab_sex = c("Male", "Female")
  lab_age = c(0:79, "80+")
  
  par_list = list(
    years   = years,
    basepop = matrix(par$base.pop$value, nrow=2, ncol=n_age, byrow=TRUE),
    tfr     = par$tfr$value,
    srb     = par$srb$value,
    pasfrs  = matrix(par$pasfrs$value, nrow=n_yrs, ncol=n_age_fert, byrow=TRUE),
    Sx      = aperm(array(par$life.table$Sx, dim=c(n_age+1, n_sex, n_yrs), dimnames=list(Age=c("B", lab_age), Sex=lab_sex, Year=years)), 3:1),
    migr    = aperm(array(par$migr$value, dim=c(n_age, n_sex, n_yrs), dimnames=list(Age=lab_age, Sex=lab_sex, Year=years)), 3:1)
  )
  
  ## Spectrum aggregates PASFRs to five-year age groups, then assumes
  ## age-specific fertility = 20% of the corresponding five-year age group
  ## fertility
  if (spec.fert) { 
    par_list$pasfrs[, 1:5 ] = 0.2 * matrix(rowSums(par_list$pasfrs[, 1:5 ]), nrow=n_yrs, ncol=5) # 15-19
    par_list$pasfrs[, 6:10] = 0.2 * matrix(rowSums(par_list$pasfrs[, 6:10]), nrow=n_yrs, ncol=5) # 20-24
    par_list$pasfrs[,11:15] = 0.2 * matrix(rowSums(par_list$pasfrs[,11:15]), nrow=n_yrs, ncol=5) # 25-29
    par_list$pasfrs[,16:20] = 0.2 * matrix(rowSums(par_list$pasfrs[,16:20]), nrow=n_yrs, ncol=5) # 30-34
    par_list$pasfrs[,21:25] = 0.2 * matrix(rowSums(par_list$pasfrs[,21:25]), nrow=n_yrs, ncol=5) # 35-39
    par_list$pasfrs[,26:30] = 0.2 * matrix(rowSums(par_list$pasfrs[,26:30]), nrow=n_yrs, ncol=5) # 40-44
    par_list$pasfrs[,31:35] = 0.2 * matrix(rowSums(par_list$pasfrs[,31:35]), nrow=n_yrs, ncol=5) # 45-49
  }
  
  rval = proj.method(par_list)
}
