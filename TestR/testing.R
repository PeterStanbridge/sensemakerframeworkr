library(jsonlite)
library(tidyverse)

json_header_names <- c("name", "id", "language")
supported_signifier_types <- c("triad", "dyad", "list", "stones", "freetext", "imageselect", "photo", "audio", "uniqueid", "embedded")
shape_slider_types <- c("triad", "dyad", "stones")

json_file <- "R/MPI_Data.json"

fw <- jsonlite::fromJSON(txt = json_file, simplifyVector = TRUE, simplifyDataFrame = TRUE, flatten = FALSE)

header_values <- fw[json_header_names]
signifier_values <- fw[["signifiers"]]
linked_frameworks <- fw[["linked_frameworks"]]$framework

pull_out_definitions(signifier_values, linked_frameworks, header_values)

pull_out_definitions <- function(tsignifier_values, tlinked_frameworks, theader_values) {

  if (1 == 3) {
    tsignifier_values <- signifier_values
    tlinked_frameworks <- linked_frameworks
    theader_values <- header_values
    
    tsignifier_values <- lsignifier_values[[1]] 
    tlinked_frameworks <- llinked_frameworks
    theader_values <- lheader_values
    
  }
  # each signifier in this framework
  for (i in seq_along(tsignifier_values[,"id"])) {
    signifier_type <- tsignifier_values[i, "type"]
    # process only proper signifiers or embedded definitions
    if (signifier_type %in% supported_signifier_types) {
      # if type "embedded" - and role "collector" then pull out the linked framework definition and process that
      if (signifier_type == "embedded") {
        
        this_embedded_role <- tsignifier_values[i,][["content"]][["role"]]
        if (!is.na(this_embedded_role)) {
          if (this_embedded_role == "collector") {
            embedded_id <- tsignifier_values[i, "id"]
            embedded_info <- get_embedded_type(tsignifier_values, embedded_id, tlinked_frameworks)
            embedded_type <- embedded_info[["type"]]
            linked_id <- tsignifier_values[i,][["content"]][["embedded_engagement"]]
            sig_defs_child <- tlinked_frameworks %>% dplyr::filter(id == linked_id)
            lsignifier_values <- sig_defs_child[["signifiers"]]#[[1]]
            # only process the child framework if there are actual signifiers in it (or further embeddings) - it might be a simple message page. 
            if (any(lsignifier_values[[1]][["type"]] %in% supported_signifier_types)) {
              # if the type is linked, then the child definitions will point back up to the current framework as it's parent and will create a new branch. If
              # not, it is "embedded" and the signifiers are simply embedded as though they are in the current level and no child framwork is created. 
              if (embedded_type == "Linked") {
                lheader_values <- as.list(sig_defs_child[json_header_names])
                # add child vertex to the graph

              } else {
                # it is embedded - update the embedded graph and process the child as parent
                lheader_values_temp <- as.list(sig_defs_child[json_header_names])
                
                lheader_values <- theader_values 
              }
              llinked_frameworks <-  sig_defs_child[["linked_frameworks"]][[1]]$framework
            
              # pull out child definition
              pull_out_definitions(lsignifier_values[[1]], llinked_frameworks, lheader_values)
            }
          }
        }
      } else {
        # we have a standard signifier. Process it (adding to the signifier fields)
        # Just ripple this update if the signifier already loaded - will be the case if identical ID used across linked frameworks
        print(paste("processing signifier", tsignifier_values[i,"id"], "of type", tsignifier_values[i,"type"]))

        # }
      }
    }
  }
  
  
}

get_embedded_type <- function(tjson, tembedded_id, tlinked_frameworks) {
  l_json <- tjson %>% dplyr::filter(type == "list")
  list_list <- purrr::map(l_json$content$items, ~ {.x$other_signifier_id})
  list_id <- NULL
  item_id <- NULL
  
  if (tembedded_id %in% as.vector(na.omit(unlist(purrr::map(l_json$content$items, ~ {.x$other_signifier_id}))))) {
    
    for (i in 1:length(list_list)) {
      if (tembedded_id %in% list_list[[i]]) {
        
        # check if the signifiers in the child framework are all non-shape sliders in which case we treat it as embedded as
        # nothing to show in the workbench for the linked framework. 
        linked_id <- tjson[i,][["content"]][["embedded_engagement"]]
        if (!is.na(linked_id)) {
          sig_defs_child <- tlinked_frameworks %>% dplyr::filter(id == linked_id)
          lsignifier_value_types <- sig_defs_child[["signifiers"]][[1]]$type
          if (any(lsignifier_value_types %in% shape_slider_types) == FALSE) {
            return(list(type = "embedded", list = list_id, item = item_id))
          }
        }
        list_id <- l_json[i,"id"]
        item_id <-  l_json[i,"content"][["items"]][[1]][which(tembedded_id == l_json[i,"content"][["items"]][[1]][,"other_signifier_id"]), "id"]
      }
    }
    return(list(type = "Linked", list = list_id, item = item_id))
  }
  return(list(type = "embedded", list = list_id, item = item_id))
}

