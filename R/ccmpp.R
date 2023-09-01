#' @describeIn demproj CCMPP that spreads net migrants uniformly throughout each period
#' @export
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
  
  if (exists("births", where=par)) {
    btotal = par$births[1]
  } else {
    btotal = sum(0.5 * (pop[1,2,11:55] + pop[1,2,10:54] * par$Sx[1,2,10:54]) * par$tfr[1] * par$pasfr[1,] / sum(par$pasfr[1,]))
  }
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
    if (exists("births", where=par)) {
      btotal = par$births[k]
    } else {
      btotal = sum(0.5 * (pop[k,2,11:55] + pop[k-1,2,11:55]) * par$tfr[k] * par$pasfr[k,]) / sum(par$pasfr[k,])
    }
    births[k,1] = btotal * par$srb[k] / (100.0 + par$srb[k])
    births[k,2] = btotal - births[k,1]
    deaths[k,,1] = births[k,] * (1.0 - par$Sx[k,,1]) + mr * (2.0 * (1.0 - par$Sx[k,,1])) / 3.0
    pop[k,,1] = births[k,] * par$Sx[k,,1] + mr * (1.0 + 2 * par$Sx[k,,1]) / 3.0
  }
  
  rval = list(pop=pop, births=births, deaths=deaths)
  return(rval)
}

#' @describeIn demproj CCMPP that spreads net migrants at the end of each period
#' @export
ccmpp_migr_end = function(par) {
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
  
  if (exists("births", where=par)) {
    btotal = par$births[1]
  } else {
    btotal = sum(0.5 * (pop[1,2,11:55] + pop[1,2,10:54] * par$Sx[1,2,10:54]) * par$tfr[1] * par$pasfr[1,] / sum(par$pasfr[1,]))
  }
  
  births[1,1] = btotal * par$srb[1] / (100.0 + par$srb[1])
  births[1,2] = btotal - births[1,1]
  

  for (k in 2:length(par$years)) {
    # ages 1-79 -> 2-80
    deaths[k,,2:n_age] = pop[k-1,,1:(n_age-1)] * (1.0 - par$Sx[k,,2:n_age])
    pop[k,,2:n_age] = pop[k-1,,1:(n_age-1)] * par$Sx[k,,2:n_age]
    
    # ages 80 -> 80+
    deaths[k,,n_age] = deaths[k,,n_age] + pop[k-1,,n_age] * (1.0 - par$Sx[k,,n_age+1])
    pop[k,,n_age] = pop[k,,n_age] + pop[k-1,,n_age] * par$Sx[k,,n_age+1]
    
    if (exists("births", where=par)) {
      btotal = par$births[k]
    } else {
      btotal = sum(0.5 * (pop[k,2,11:55] + pop[k-1,2,11:55]) * par$tfr[k] * par$pasfr[k,]) / sum(par$pasfr[k,])
    }
    
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
#' @param par demographic inputs as prepared by \code{read_upd}
#' @param proj.method the CCMPP function to use, either \code{ccmpp_migr_mid}
#'   for mid-period net migration or \code{ccmpp_migr_end} for end-period net
#'   migration
#' @param spec.fert If \code{spec.fert=FALSE}, then proportionate age-specific
#'   fertility \code{par$pasfrs} is used as-is. If \code{spec.fert=TRUE},
#'   age-specific fertility is aggregated to five-year age groups, then assumed
#'   uniform within those age groups during model projection as in Spectrum. By
#'   default, \code{spec.fert=TRUE}.
#' @return Returns a list containing
#'  \enumerate{
#'  \item{pop - an array of projected estimates by year, sex (1:male, 2:female) and age. The maximum age corresponds to the open 80+ age group.}
#'  \item{births - an array of births by year and child's sex.}
#'  \item{deaths - an array of projected deaths by year, sex, and age.}
#'  }
#' @details This implementation is intended to replicate the demographic
#'   projection calculations done in the Spectrum software suite, which in turn
#'   is based on methods used by the United Nations Population Division to
#'   produce its World Population Prospects (WPP).
#'
#'   The 2022 WPP revision used end period net migration. Previous WPP revisions
#'   used mid-period net migration. For consistency with WPP, Spectrum used
#'   mid-period net migration prior to Spectrum version 6.19. Since version 6.20
#'   released in 2022, Spectrum has used end-period net migration.
#'
#'   Please note that Spectrum handles net migration inputs differently than
#'   DemProjR. DemProjR uses net migration inputs as specified in par$migr
#'   exactly. To simplify its user interface, net migration in Spectrum is
#'   entered by five-year age groups, then disaggregated to single ages during
#'   projection. This may cause small differences in population projections
#'   between Spectrum and DemProjR.
#'
#' @export
demproj = function(par, proj.method=ccmpp_migr_mid, spec.fert=TRUE) {
  years = par$tfr$year
  year_first = min(years)
  year_final = max(years)
  
  n_yrs = length(years)
  n_sex = 2
  n_age = 81
  
  lab_sex = c("Male", "Female")
  lab_age = c(0:79, "80+")
  
  fert_span = range(par$pasfrs$age)
  if (all(fert_span == c(10,54))) {
    pasfrs = matrix(par$pasfrs$value, nrow=n_yrs, ncol=fert_span[2]-fert_span[1]+1, byrow=TRUE)
  } else if (all(fert_span == c(15,49))) {
    pasfrs = matrix(0.0, nrow=n_yrs, ncol=54-10+1)
    pasfrs[,6:40] = matrix(par$pasfrs$value, nrow=n_yrs, ncol=fert_span[2]-fert_span[1]+1, byrow=TRUE)
  } else {
    stop(sprintf("Unsupported reproductive age range %d-%d, 15-49 or 10-54 expected", fert_span[1], fert_span[2]))
  }
  
  par_list = list(
    years   = years,
    basepop = matrix(par$base.pop$value[par$base.pop$year==year_first], nrow=2, ncol=n_age, byrow=TRUE),
    tfr     = par$tfr$value,
    srb     = par$srb$value,
    pasfrs  = pasfrs,
    Sx      = aperm(array(par$life.table$Sx, dim=c(n_age+1, n_sex, n_yrs), dimnames=list(Age=c("B", lab_age), Sex=lab_sex, Year=years)), 3:1),
    migr    = aperm(array(par$migr$value, dim=c(n_age, n_sex, n_yrs), dimnames=list(Age=lab_age, Sex=lab_sex, Year=years)), 3:1)
  )
  
  ## Check if births are provided as an input
  if (exists("births", where=par)) {
    par_list$births = par$births$value
  }
  
  ## Spectrum flattens age-specific fertility by five year age group
  if (spec.fert) {
    block_num = (54 - 10 + 1) %/% 5 #
    block_mat = Matrix::bdiag(replicate(block_num, matrix(0.2, nrow=5, ncol=5), simplify=FALSE))
    par_list$pasfrs = as.matrix(pasfrs %*% block_mat)
  }
  
  rval = proj.method(par_list)
}
