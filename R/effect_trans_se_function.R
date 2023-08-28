#' Effect Size Transformation and SE Approximation for the Respective Case
#'
#' This function applies the case-dependent effect size transformation and 
#' standard error approximation based on the provided observation d from a 
#' dataframe created via the `join_irf_json()` function.
#'
#' @param d An observation (i.e. a row) from a dataframe obtained using `join_irf_json()`.
#' 
#' @export
effect_trans_se_function <- function (d) {

  # Get specifications of shock and outcome variable
  shock_code <- d$inttype
  dep_code <- d$outcome_var
  cum <- d$cum
  
  case1 <- grepl("log_", dep_code) & grepl("lev_", shock_code) & cum == FALSE
  case2 <- grepl("gr_", dep_code) & grepl("lev_", shock_code) & cum == FALSE
  case3 <- grepl("logdiff_", dep_code) & grepl("lev_", shock_code)  & cum == FALSE
  case4 <- grepl("logdiff_", dep_code) | grepl("gr_", dep_code) & grepl("lev_", shock_code)  & cum == TRUE
  
  # Get y-axis transformation function
  axis_scaling <- get_axis_scale(d)$axis_scaling_function
  # Specify shock size
  shock_size <- get_shock_size(d, study_data)
  # Adjust periodicity of shock_size (if necessary)
  shock_size <- adjust_shock_periodicity(d, shock_size = shock_size)
  # Get confidence level and critical value
  conf_level <- get_conf_level(d)
  crit_val <- get_crit_val(conf_level)
  
  if (dep_code == "rate") {
    
    # If outcome variable is "rate": TO DO: Which transformation in this case???
    warning("rate not yet integrated")
    # PLACEHOLDERs
    d$CI.upper <- NA
    d$mean.effect <- NA
    d$CI.lower <- NA
    d$SE.upper <- NA
    d$SE.lower <- NA
    
  } else {
    
    if (case1 == TRUE | case4 == TRUE) { # CASE 1 or CASE 4
      
      # If case 1: Log-level of response variable and level of shock variable, non-cumulative
      # OR
      # If case 4: Log-difference or growth rate of response variable and level of shock variable, cumulative IRF
      
      # Step 1: Apply axis scaling function 
      d$CI.upper <- axis_scaling(d$CI.upper.raw)
      d$mean.effect <- axis_scaling(d$mean.effect.raw)
      d$CI.lower <- axis_scaling(d$CI.lower.raw)
      # Standardize to effect of 1 percentage point shock
      d$CI.upper <- d$CI.upper / shock_size * 100
      d$mean.effect <- d$mean.effect / shock_size * 100
      d$CI.lower <- d$CI.lower / shock_size * 100
      # Calculate the standard errors
      d$SE.upper <- abs(d$CI.upper - d$mean.effect) / crit_val
      d$SE.lower <- abs(d$CI.lower - d$mean.effect) / crit_val

    } else if (case2 == TRUE | case3 == TRUE) { # CASE 2 or CASE 3
      
      # If case 2: Growth rate of response variable and level of shock variable, non-cumulative
      # OR
      # If case 3: Log-difference of response variable and level of shock variable, non-cumulative
      
      # Step 1: Get all effect sizes and bounds until period h
      h <- d$period
      irf_until_h <- study_data[study_data$key == d$key & 
                                      study_data$model_id == d$model_id & 
                                      study_data$outcome_var == dep_code & 
                                      study_data$period %in% 1:h, c("CI.upper.raw", "mean.effect.raw", "CI.lower.raw")]
      # Step 2: Apply axis-scaling function 
      irf_until_h <- axis_scaling(irf_until_h)
      # Additional adjustment step for case 3
      if (case3 == TRUE) {
        irf_until_h <- (exp( irf_until_h/100 ) - 1) * 100
      }
      # Step 3: Standardize to pp change after a 100 basis points interest rate hike
      if (d$month == TRUE & grepl("_a_", dep_code)) { # If monthly data but annualized growth rate
        
        irf_until_h <- ( (irf_until_h / 100 + 1)^(1/12) - 1 ) * 10000/shock_size
        
      } else if (d$quarter == TRUE & grepl("_a_", dep_code)) { # If quarterly data but annualized growth rate
        
        irf_until_h<-( (irf_until_h / 100 + 1)^(1/4) - 1) * 10000/shock_size
        
      } else { # If data frequency and periodicity align
        irf_until_h <- irf_until_h / shock_size * 100
      }
      # Step 4: Calculate standard errors
      irf_until_h$SE.upper.raw <- abs(irf_until_h$CI.upper.raw - irf_until_h$mean.effect.raw) / crit_val
      irf_until_h$SE.lower.raw <- abs(irf_until_h$CI.lower.raw - irf_until_h$mean.effect.raw) / crit_val
      # Step 5: Calculate cumulative sum of standardized effects, bounds and standard errors up to period h
      d$CI.upper <- sum(irf_until_h$CI.upper.raw)
      d$mean.effect <- sum(irf_until_h$mean.effect.raw)
      d$CI.lower <- sum(irf_until_h$CI.lower.raw)
      d$SE.upper <- sqrt(sum(irf_until_h$SE.upper.raw^2))
      d$SE.lower <- sqrt(sum(irf_until_h$SE.lower.raw^2))
      
    }
    
  }
  
  return(d)
  
}
