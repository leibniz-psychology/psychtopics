booster <- readRDS("./inst/data/booster.RDS")
topic_evo <- readRDS("./inst/data/topic_evo.RDS")

createLink_evo <- function(val, boost) {
  list <- list()
  
  # use colMeans of booster for evo terms
  # Future update: compute separate booster objects for each year
  booster_means <- round(rowMeans(booster), 2)
  
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



topic_evo_search <- list()
for (i in 1: length(topic_evo)){
  val <- apply(topic_evo[[i]], 2, paste, collapse = ", ")
  link <- createLink_evo(val, booster)
  topic_evo_search[[i]] <- rbind(topic_evo[[i]], link)
}

saveRDS(topic_evo_search, file = "./inst/data/topic_evo_search.RDS")



## topic evo for psychtopics 2, by Zauad Shahreer

make_topic_evo_concatenated = function(topic_evo_rds, directory = "./") {
  
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
  
  topic_evo_concatenated = sapply(topic_evo, make_topic_evo_string)
  saveRDS(topic_evo_concatenated, file = "./inst/data/topic_evo_concatenated.RDS")
}

make_topic_evo_concatenated(topic_evo)




createLink_OSF <- function(val) {
 list <- list()

 for (i in 1:length(val)){
   list[[i]] <- unlist(strsplit(val[i], ", ", fixed = TRUE))
   for (j in 1:9){
     list[[i]][j] <- paste0('', list[[i]][j], '') # No boosting, just enclosing in quotes
   }
   list[[i]][10] <- paste0('', list[[i]][10], '') # Term 10 is reference
   list[[i]] <- paste0(list[[i]], collapse= " ")
   list[[i]] <- gsub("'", "%27", list[[i]])
 }
 val <- unlist(list)
 paste0("<a href='https://osf.io/search?q=", 
        val,"&resourceType=Registration%2CRegistrationComponent&sort=-dateCreated' target='_blank' class='btn btn-primary'>Search OSF</a>")
}




createLink_PsychArchives <- function(val) {
  list <- list()
  
  for (i in 1:length(val)){
    list[[i]] <- unlist(strsplit(val[i], ", ", fixed = TRUE))
    for (j in 1:9){
      list[[i]][j] <- paste0('"', list[[i]][j], '"') # No boosting, just enclosing in quotes
    }
    list[[i]][10] <- paste0('"', list[[i]][10], '"') # Term 10 is reference
    list[[i]] <- paste0(list[[i]], collapse="+OR+")
    list[[i]] <- gsub("'", "%27", list[[i]])
  }
  val <- unlist(list)
  paste0("<a href='https://www.psycharchives.org/en/browse/?q=", 
         val,"&fq=dcType_keyword%3A%28preregistration%29' target='_blank' class='btn btn-primary'>Search PsychArchives</a>")
}


