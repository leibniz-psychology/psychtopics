	#' trends 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
# Trends for range input
########################

# original code by M. Ponweiser: https://github.com/mponweiser/thesis-LDA-in-R


trends.ab <- function(von, bis, 
                      theta_mean_by_year, 
                      theta_mean_by_year_time, 
                      theta_mean_by_year_ts, years, topic){
  
  colnames(theta_mean_by_year) <- 1:nrow(topic)
  
  # Linear model
  theta_mean_lm <- apply(theta_mean_by_year[von:bis,], 2, function(x) lm(x ~ theta_mean_by_year_time[von:bis]))
  theta_mean_lm_coef <- lapply(theta_mean_lm,function(x) coef(summary(x)))
  theta_mean_lm_coef_sign <- sapply(theta_mean_lm_coef,'[',"theta_mean_by_year_time[von:bis]","Pr(>|t|)")
  theta_mean_lm_coef_slope <- sapply(theta_mean_lm_coef,'[',"theta_mean_by_year_time[von:bis]","Estimate")
  
  # devide in positive and negative slopes
  theta_mean_lm_coef_slope_pos <- theta_mean_lm_coef_slope[theta_mean_lm_coef_slope >= 0]
  theta_mean_lm_coef_slope_neg <- theta_mean_lm_coef_slope[theta_mean_lm_coef_slope < 0]
  
  
  ### hot & cold
  topics_hot <- as.numeric(names(sort(theta_mean_lm_coef_slope_pos, decreasing = TRUE)))
  topics_cold <- as.numeric(names(sort(theta_mean_lm_coef_slope_neg, decreasing = FALSE)))
  
  hot_ts <- ts(theta_mean_by_year_ts[von:bis,topics_hot], start = as.integer(years[von]))
  cold_ts <- ts(theta_mean_by_year_ts[von:bis,topics_cold], start = as.integer(years[von]))
  
  # tables
  terms_hot <- topic[topics_hot,]
  terms_cold <- topic[topics_cold,]
  
  colnames(terms_hot)[1] <- "NR"
  colnames(terms_cold)[1] <- "NR"
  
  
  # results
  results <- list()
  results[1] <- list(terms_hot)
  results[2] <- list(terms_cold)
  results[3] <- list(hot_ts)
  results[4] <- list(cold_ts)
  
  return(results)
  
}