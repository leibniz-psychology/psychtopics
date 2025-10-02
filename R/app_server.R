#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  
  r = reactiveValues()
  
  
  ## update the following line by hand
  r$last_updated = "October 2, 2025"
  
  
  r$n_docs_year <- isolate(readRDS("inst/data/n_docs_year.RDS"))
  r$n_docs_year_0 <- isolate(readRDS("inst/data/n_docs_year_0.RDS"))
  
  n_docs_year_id <- isolate(readRDS("inst/data/n_docs_year_0.RDS")) # topic ids instead of labels
  colnames(n_docs_year_id) <- 1:ncol(n_docs_year_id)
  
  K <- ncol(n_docs_year_id) # number of topics
  k <- K # just in case of different variants in the code
  
  r$years <- isolate(readRDS("inst/data/years.RDS")) # a list of publication years
  years = isolate(r$years)
  n_years <- isolate(length(r$years))
  
  r$n_docs_time <- time(n_docs_year_id) # for trend analysis
  r$n_docs_ts <- ts(n_docs_year_id, start = as.integer(years[1])) # Time-series
  
  
  r$topic <- isolate(readRDS("inst/data/topic.RDS")) # a list of topics and top terms
  r$booster <- isolate(readRDS("inst/data/booster.RDS")) # a table with factors for term boosting in PubPsych.eu

  r$n_doc_year = isolate(readRDS("inst/data/n_docs_year.RDS")) %>% 
    as.table() %>% 
    as.data.frame() %>% 
    dplyr::mutate(
      year = as.numeric(as.character(Var1)),
      label = Var2,
      id = rep(1:k, each = n_years),
      Freq = round(Freq, 2)
    )
  
  r$empirical = isolate(readRDS("inst/data/empirical_year.RDS")) %>% 
    as.table() %>% 
    as.data.frame() %>% 
    dplyr::mutate(
      year = as.numeric(as.character(Var1)),
      label = Var2,
      id = rep(1:k, each = n_years),
      Freq = round(Freq, 2)
    )
  
  r$topic_evo = isolate(readRDS("./inst/data/topic_evo.RDS"))
  r$topic_evo_concatenated = isolate(readRDS("./inst/data/topic_evo_concatenated.RDS"))
  
  # topic_evo with search link
  r$topic_evo_search <- isolate(readRDS("./inst/data/topic_evo_search.RDS"))
  
  
  
  
  ## files for topic evo ----
  
  r$current_year = isolate(max(r$years))
  r$start_year = isolate(min(r$years))
  r$start_evo = isolate(min(as.numeric(colnames(r$topic_evo[[1]]))))
  
  topic_evo_firsts <- isolate(lapply(r$topic_evo, function(x){
    as.numeric(colnames(x)[1])
  }))
  
  topic_evo_lasts <- isolate(lapply(r$topic_evo, function(x){
    as.numeric(colnames(x)[dim(x)[2]])
  }))
  
  r$topic_evo_firsts <- unlist(topic_evo_firsts)
  r$topic_evo_lasts <- unlist(topic_evo_lasts)
  r$new_data <- isolate(readRDS("./inst/data/topic_model_dataset.Rds"))
  r$new_data_year <- isolate(r$new_data %>% dplyr::pull(Year) %>% unique())
  r$new_data_month <- isolate(r$new_data %>% dplyr::pull(Month) %>% unique())
  
  mod_start_server("start", r)
  mod_browse_topics_server("browse", r)
  mod_popular_by_year_server("popular", r)
  #mod_hot_topics_server("hot_topics", r)
  #mod_compare_years_server("compare", r)
  mod_publication_trends_server("publication_trends", r)
  mod_topic_evol_server("topic_evol", r)
  mod_methods_server("methods", r)
  
  router$server(input, output, session)
    
}
