#' helper_functions 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd



router <- shiny.router::make_router(
  route("/", mod_start_ui("start")),
  route("browse-topics", mod_browse_topics_ui("browse")),
  route("popular", mod_popular_by_year_ui("popular")),
  route("hot-topics", mod_hot_topics_ui("hot_topics")),
  #route("compare", mod_compare_years_ui("compare")),
  route("publication-trends", mod_publication_trends_ui("publication_trends")),
  route("topic-evolution", mod_topic_evol_ui("topic_evol")),
  route("methods", mod_methods_ui("methods"))
)




# col_bars <- "#0094c5"
# col_highlight <- "gold"
# colors in tags$style have to be set manually in respective lines

# PubPsych.eu search terms are boosted according to the relations of beta probabilites
# Factors were computed by dividing the beta probabilites of Terms 1-9 by beta of Term 10


createLink <- function(val, boost, topicnum) {
  list <- list()
  for (i in 1:length(val)){
    list[[i]] <- unlist(strsplit(val[i], ", ", fixed = TRUE))
    for (j in 1:9){
      list[[i]][j] <- paste0('"', list[[i]][j], '"%5E', boost[j, topicnum[i]]) # add boost factors for first 9 terms
    }
    list[[i]][10] <- paste0('"', list[[i]][10], '"') # Term 10 is reference, so no boosting
    list[[i]] <- paste0(list[[i]], collapse="+OR+")
    list[[i]] <- gsub("'", "%27", list[[i]])
  }
  val <- unlist(list)
  paste0("<a href='https://pubpsych.zpid.de/pubpsych/Search.action?q=%28%28", 
         val,"%29%29+DB%3DPSYNDEX&stats=TOP' target='_blank' class='btn btn-primary'>Search PSYNDEX</a>")
}


# boosting for evo terms
createLink_evo <- function(val, boost) {
  list <- list()
  
  # use colMeans of booster for evo terms
  # Future update: compute separate booster objects for each year
  booster_means <- round(rowMeans(boost), 2)
  
  for (i in 1:length(val)){
    list[[i]] <- unlist(strsplit(val[i], ", ", fixed = TRUE))
    for (j in 1:9){
     list[[i]][j] <- paste0('"', list[[i]][j], '"%5E', booster_means[j]) # add boost factors for first 9 terms
    }
    list[[i]][10] <- paste0('"', list[[i]][10], '"') # Term 10 is reference, so no boosting
    list[[i]] <- paste0(list[[i]], collapse="+OR+")
    list[[i]] <- gsub("'", "%27", list[[i]])
  }
  val <- unlist(list)
  paste0("<a href='https://pubpsych.zpid.de/pubpsych/Search.action?q=%28%28", 
         val,"%29%29+DB%3DPSYNDEX&stats=TOP' target='_blank' class='btn btn-primary'>Search PSYNDEX</a>")
}



#' Function that adds a new RDS file named topic_evo_concatenated.RDS
#'
#' @param topic_evo_rds the topic_evo.RDS as an object
#' @param directory the directory to save to
#'
#'
#' @examples
#' \dontrun{
#' topic_evo_rds = readRDS("inst/data/topic_evo.RDS")
#' make_topic_evo_concatenated(topic_evo_rds)
#' }
make_topic_evo_concatenated = function(topic_evo_rds, directory = "inst/data/") {
  
  
  make_topic_evo_string = function(each) {
    
    each = as.data.frame(each)
    years = names(each)
    
    get_all_strings = function(year) {
      strings = glue::glue_collapse(each[[year]], sep = ", ")
      glue::glue("{year}: {strings}")
    }
    
    all_strings = sapply(years, get_all_strings)
    glue::glue_collapse(all_strings, sep = "\n")
    
  }
  
  topic_evo_concatenated = sapply(topic_evo_rds, make_topic_evo_string)
  name = "topic_evo_concatenated.RDS"
  saveRDS(topic_evo_concatenated, glue::glue("{directory}/{name}"))
}

## example
# topic_evo_rds = readRDS("inst/data/topic_evo.RDS")
# make_topic_evo_concatenated(topic_evo_rds)

#stringr::str_extract_all(x, "1999.*")
