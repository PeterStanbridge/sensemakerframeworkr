# NOTE - Fields are public as coders will be able to add their own logic on any
#        thing framework that may not yet be available in the methods. 
# try using base split to pass via a map2 or pmap
#
# N O T E !!!!!
# New fully recursive graph code will be generated with # new graph comment. All old code kept for now for backward compatability commented # old method
#
# The new method philosophy. 
# A header field with the parent project header properties - id, name etc. All methods referring to parent will be via the header property id
# No more parent data structures with the signifiers
# No more linked framework data structures with the signiries
# Just  signifier data structures - type/id and id/type by framework - parent simply being one of them. 
# Will still have the list of linked headers, each with id. 
# Will store the framework structure graph for navigating through the stuctures. 
# Getting linked framework ids is getting all framework ids less the parent. 
# Method for getting children frameworks from parent
# Method for getting parent framework from child. 
#
#' R6 class that represents a framework's signifier definitions.
#'
#' @description
#' The `signifiers` class is the primary class for
#' representing a framework's signifier definition. It will include by
#' default embedded frameworks and if a dashboard, the linked frameworks
#' including definitions of data filtering associated with the dashboard
#' primary framework.
#' The signifiers class is packaged with a large number of helper functions
#' that aid R programmers to work with a framework's capture data.
#'
#' @docType class
#' @format \code{\link{R6Class}} object.
#' @examples
#' # The package vignettes include extensive examples of working with the
#' # signifiers class.
#' library(sensemakerframeworkr)
#' my_fw <- signifiers$new("mydir/projectFramework.json", NULL, NULL, NULL)
#' fw_triads <- self$get_signifier_ids_by_type("triad")
#' triad_01_image <- pt$get_triad_background_image(fw_triads[[1]])
Signifiers <- R6::R6Class("Signifiers",
                          public = list(
                            # New fields for the new fully functional linked framework
                            #' @field frameworks A list of all framework names with framework ids as list ids
                            frameworks = NULL,
                            #' @field framework_graph An igraph graph of the framework definition (showing linked and embedded frameworks if they exist)
                            framework_graph = NULL,
                            #' @field framework_embedded An igraph graph of the embedded frameworks (not linked)
                            framework_embedded = NULL,
                            #' @field polymorphic_definitions Named list giving the R6 classes of the polymorphic signifier definitions. 
                            polymorphic_definitions = NULL,
                            #' @field polymorphic_type_by_ids A named list of the ids defining the polymorphic signifiers, values are types, list names are ids 
                            polymorphic_type_by_ids = NULL,
                            #' @field polymorphic_id_list_by_type A list of lists of polymorphic ids by type type -> list(ids)
                            polymorphic_id_list_by_type = NULL,
                            #' @field polymorphic_ids_by_title A list of  polymorphic ids whose names are the signifier titles
                            polymorphic_ids_by_title = NULL, 
                            # End of new fields
                            #' @field types_by_signifierid Named list giving the signifier type (value) for each signifier ID (name)
                            types_by_signifierid = NULL,
                            #' @field signifierids_by_type Named list giving the signifier ids (value) for each signifier type (name)
                            signifierids_by_type = NULL, 
                            #' @field signifier_definitions Named list giving the R6 class of the signifier definition (value) for each signifier ID (name)
                            signifier_definitions = NULL,
                            #' @field types_with_signifiers Vector giving the signifier types contained in the framework definition
                            types_with_signifiers = NULL,
                            #' @field parent_framework To Depreciate - Named single entry list giving the name of the parent framework (value) with parent id as name. 
                            parent_framework = NULL,
                            #' @field parent_header Properties in the parent framework header. 
                            parent_header = NULL,
                            #' @field signifier_counts_linked_frameworks_type ToDo rename to signifier_counts_type Vector giving the signifier types contained in the framework definition in the parent and linked frameworks
                            signifier_counts_linked_frameworks_type = NULL, 
                            #' @field types_by_signifierid_framework Named list containing the signifier id and signifier type (value name/value pair) for the framework id (name)
                            types_by_signifierid_framework = NULL,
                            #' @field signifierids_by_type_framework List containing the signifier type and ids (value name/values pair) for the framework id (name)
                            signifierids_by_type_framework = NULL,
                            #' @field types_with_signifiers_framework Named list containing a vector of signifier types (value) contained in the framework id (name)
                            types_with_signifiers_framework = NULL, 
                            #' @field linked_other_signifier The other signifier ID for list other options ToDo - this should be deleted but currently used in list apply (but not sure why)
                            linked_other_signifier = NULL,
                            #' @field supported_signifier_types Vector containing all the signifier types supported in the SenseMaker® platform
                            supported_signifier_types = c("triad", "dyad", "list", "stones", "freetext", "imageselect", "photo", "audio", "uniqueid", "embedded"),
                            #' @field signifier_properties Vector containing the property names for the signifier definition main header properties. 
                            signifier_properties = c("title", "tooltip", "allow_na", "fragment", "required", "sticky", "include", "hide"),
                            #' @description
                            #' Create a new `signifiers` object.
                            #' @details
                            #' The json file is parsed into various data structures returned as a set of R6 classes. Package methods provide the R
                            #' programmer with a toolset to work with the SenseMaker® framework capture data.
                            #' @param jsonfilename if using a json file stored locally, the path and file name of the json file to load.
                            #' @param layoutfilename if using a json file stored locally, the path and file name of the layout json file to load. 
                            #' @param parsedjson if using a json file previously loaded and parsed, the parsed file.
                            #' @param parsedlayout if using a json layout file previously loaded and parsed, the parsed file. 
                            #' @param workbenchid if using the platform security, the id for the workbench/dashboard.
                            #' @param token if using the platform securithy, the token to gain access to the json definition.
                            #' @return A new `signifier` R6 class object and fields type by signifier id, signifier ids by type, and
                            #'           types with signifiers.
                            initialize = function(jsonfilename, layoutfilename, parsedjson = NULL, parsedlayout = NULL, workbenchid = NULL,
                                                  token = NULL, poly_data = NULL, poly_data_file = NULL) {
                              sensemakerframework <- private$unpackjson(self, parsedjson, parsedlayout, jsonfilename, layoutfilename, workbenchid, token, poly_data, poly_data_file)
                              
                            },
                            #-----------------------------------------------------------------
                            # Data and Framework Independent Helper Functions
                            # ToDo - place these into the independent helper function package when you gtet to do it
                            #-----------------------------------------------------------------
                            #' @description
                            #' Flatten a list of lists to a single list (removing the top elements (note will do list of list of list)). This is a general helper function that will be moved to the generic helper package.
                            #' @param x The list of lists to flatten. 
                            #' @return
                            #' A list of the lowest level list elements
                            flatten_list = function(x) {
                              private$flattenlist(x)
                            },
                            #-----------------------------------------------------------------
                            # Generic Helper Functions
                            #-----------------------------------------------------------------
                            #' @description
                            #' Get all framework ids
                            #' @param include_names TRUE to return list that includes the framework names. 
                            #' @return A vector of framework ids or list of ids including names. 
                            get_framework_ids = function(include_names = FALSE) {
                              framework_ids <- self$frameworks
                              if (length(framework_ids) == 0) {return(NULL)}
                              if (include_names) {
                                return(framework_ids)
                              }
                              return(names(framework_ids))
                            },
                            #' @description
                            #' Get the signifier types used within the passed framework definition
                            #' @return
                            #' A vector of the signifier types used within the passed framework definition.
                            get_used_signifier_types = function() {
                              # ToDo - with all methods accessing the fields directly, add base methods and call them. 
                              #  e.g see get_signifier_property below where this is done. 
                              return(self$types_with_signifiers)
                            },
                            #' @description
                            #' Get the supported signifier types
                            #' @return
                            #' A vector of the suuported signifier types.
                            get_supported_signifier_types = function() {
                              return(self$supported_signifier_types)
                            },
                            #' @description
                            #' Get the parent definition language
                            #' @return
                            #' The parent language.
                            get_parent_language = function() {
                              return(self$parent_header[["language"]])
                            },
                            #' @description
                            #' Get all the signifier ids contained in the framework definition.
                            #' @return
                            #' A vector of all signifier ids contained in the framework definition.
                            get_all_signifier_ids = function() {
                              return(names(self$types_by_signifierid))
                            },
                            #' @description
                            #' Get the framework signifier ids for a given signifier type.
                            #' @param type The signifier type.
                            #' @param keep_only_include TRUE or FALSE, if TRUE only those flagged with include == TRUE returned. 
                            #' @return A vector of the signifier ids in the framework definition for the passed type.
                            get_signifier_ids_by_type = function(type, keep_only_include = FALSE) {
                              if (length(self$signifierids_by_type[[type]]) == 0) {return(NULL)}
                              ret_list <- self$signifierids_by_type[[type]] 
                              if (keep_only_include) {
                                ret_list <- ret_list %>% purrr::keep(function(x) self$get_signifier_include(x) == TRUE)
                              }
                              return(ret_list)
                            },
                            #-----------------------------------------------------------------
                            # General all Signifier Helper Functions
                            #-----------------------------------------------------------------
                            #' @description
                            #' Get signifier header supported properties.
                            #' @return Vector of signifier header supported properties.
                            get_signifier_supported_header_properties = function() {
                              return(self$signifier_properties)
                            },
                            #' @description
                            #' Get the number of signifiers (count) for a signifier type.
                            #' @param type The signifier type.
                            #' @param keep_only_include TRUE or FALSE, if TRUE only those flagged with include == TRUE returned.
                            #' @return An integer of the number of signifiers defined for the type.
                            get_signifier_count_by_type = function(type, keep_only_include = FALSE) {
                              return(length(self$get_signifier_ids_by_type(type, keep_only_include)))
                            },
                            #' @description
                            #' Get the signifier type belonging to a signifier id.
                            #' @param id The signifier id whose type to retrieve.
                            #' @return A string containing the type of the passed signifier id.
                            get_signifier_type_by_id = function(id) {
                              return(self$types_by_signifierid[[id]])
                            },
                            #' @description
                            #' Get the signifier R6 class for the passed signifier id.
                            #' @param id The signifier id whose type to retrieve.
                            #' @return The R6 class for the passed in signifier id.
                            get_signifier_by_id_R6 = function(id) {
                              return(self$signifier_definitions[[self$get_signifier_type_by_id(id)]][[id]])
                            },
                            #' @description
                            #' Get the signifier property for the passed in signifier id.
                            #' @param id The signifier id whose type to retrieve.
                            #' @param property The property to retrieve ("title"    "tooltip"  "allow_na" "fragment" "required" "sticky"   "include"  "hide")
                            #' @return A string with the title of the passed in signifier id.
                            get_signifier_property = function(id, property) {
                              return(self$get_signifier_by_id_R6(id)$get_property(property))
                            },
                            #' @description
                            #' Get the signifier title for the passed in signifier id.
                            #' @param id The signifier id whose type to retrieve.
                            #' @return A string with the title of the passed in signifier id.
                            get_signifier_title = function(id) {
                              return(self$get_signifier_by_id_R6(id)$get_title())
                            },
                            #' @description
                            #' Get the signifier tooltip for the passed in signifier id.
                            #' @param id The signifier id whose type to retrieve.
                            #' @return A string with the tooltip of the passed in signifier id.
                            get_signifier_tooltip = function(id) {
                              return(self$get_signifier_by_id_R6(id)$get_tooltip())
                            },
                            #' @description
                            #' Get the signifier type of the passed in signifier id.
                            #' @param id The signifier id whose type to retrieve.
                            #' @return A string with the type of the passed in signifier id.
                            get_signifier_type = function(id) {
                              return(self$get_signifier_by_id_R6(id)$get_type())
                            },
                            #' @description
                            #' Get the allow N/a property of the passed in signifier.
                            #' @param id The signifier id whose type to retrieve.
                            #' @return A boolean TRUE if the framework allows the N/A selected for the passed in signifier id, FALSE if not.
                            get_signifier_allow_na = function(id) {
                              return(self$get_signifier_by_id_R6(id)$get_allow_na())
                            },
                            #' @description
                            #' Get the allow is fragment property of the passed in signifier.
                            #' @param id The signifier id whose type to retrieve.
                            #' @return A boolean TRUE if the passed in signifier id is a fragment, FALSE if not.
                            get_signifier_fragment = function(id) {
                              return(self$get_signifier_by_id_R6(id)$get_fragment())
                            },
                            #' @description
                            #' Get the is required property of the passed in signifier.
                            #' @param id The signifier id whose type to retrieve.
                            #' @return A boolean TRUE is the passed in signifier id is a mandatory entry for the respondent, FALSE if not.
                            get_signifier_required = function(id) {
                              return(self$get_signifier_by_id_R6(id)$get_required())
                            },
                            #' @description
                            #' Get the is sticky property of the passed in signifier.
                            #' @param id The signifier id whose type to retrieve.
                            #' @return A boolean TRUE is the passed in signifier id is a demographic one (sticky), FALSE if not.
                            get_signifier_sticky = function(id) {
                              return(self$get_signifier_by_id_R6(id)$get_sticky())
                            },
                            #' @description
                            #'  Get the framework definition content R6 class for the passed signifier id.
                            #' @param id The signifier id whose type to retrieve.
                            #' @return The R6 class for the content portion of the passed in signifier id.
                            get_signifier_content_R6 = function(id) {
                              return(self$get_signifier_by_id_R6(id)$get_content())
                            },
                            #' @description
                            #' Get whether the signifier is currently included in the capture using this framework definition.
                            #' @param id The signifier id whose type to retrieve.
                            #' @return A boolean TRUE if the passed in signifier id is currently used in collector capture, FALSE if not.
                            get_signifier_include = function(id) {
                              return(self$get_signifier_by_id_R6(id)$get_include())
                            },
                            #' @description
                            #' Get whether the signifier is hidden for analytical purposes.
                            #' @param id The signifier id
                            #' @return A boolean TRUE if the signifier is to be hidden from this analytical session.
                            get_signifier_hide = function(id) {
                              return(self$get_signifier_by_id_R6(id)$get_hide())
                            },
                            #' @description
                            #' Change signifier property value
                            #' @param id The signifier id.
                            #' @param value The new value.
                            #' @param property The property to change - e.g. title, allow_na, include required, sticky. Optional, default blank. If blank will take title.
                            #' @param type The type to change "triad", "dyad" etc. Optinal, default blank. If blank the type will be looked up from id.
                            #' @return invisible self
                            change_signifier_property_value = function(id, value, property = "", type = "") {
                              if (type == "type") {return(NULL)} # not allowed to change type
                              if (type == "") {type <- self$get_signifier_type_by_id(id)}
                              if (property == "") {property <- "title"}
                              self$signifier_definitions[[type]][[id]]$set_property(property, value)
                              invisible(self)
                            },
                            #' @description
                            #' Change signifier title value
                            #' @param id The signifier id.
                            #' @param value String. The new value.
                            #' @return invisible self
                            change_signifier_title = function(id, value) {
                              self$change_signifier_property_value(id, value, "title")
                              invisible(self)
                            },
                            #' @description
                            #' Change signifier tooltips value
                            #' @param id The signifier id.
                            #' @param value String. The new value.
                            #' @return invisible self                            
                            change_signifier_tooltip = function(id, value) {
                              self$change_signifier_property_value(id, value, "tooltip")
                              invisible(self)
                            },
                            #' @description
                            #' Change signifier allow_na value
                            #' @param id The signifier id.
                            #' @param value Boolean. The new value.
                            #' @return invisible self
                            change_signifier_allow_na = function(id, value) {
                              self$change_signifier_property_value(id, value, "allow_na")
                              invisible(self)
                            },
                            #' @description
                            #' Change signifier fraagment value
                            #' @param id The signifier id.
                            #' @param value Boolean. The new value.
                            #' @return invisible self
                            change_signifier_fragment = function(id, value) {
                              self$change_signifier_property_value(id, value, "fragment")
                              invisible(self)
                            },
                            #' @description
                            #' Change signifier required value
                            #' @param id The signifier id.
                            #' @param value Boolean. The new value.
                            #' @return invisible self
                            change_signifier_required = function(id, value) {
                              self$change_signifier_property_value(id, value, "required")
                              invisible(self)
                            },
                            #' @description
                            #' Change signifier sticky value
                            #' @param id The signifier id.
                            #' @param value Boolean. The new value.
                            #' @return invisible self
                            change_signifier_sticky = function(id, value) {
                              self$change_signifier_property_value(id, value, "sticky")
                              invisible(self)
                            },
                            #' @description
                            #' Change signifier include value
                            #' @param id The signifier id.
                            #' @param value Boolean. The new value.
                            #' @return invisible self
                            change_signifier_include = function(id, value) {
                              self$change_signifier_property_value(id, value, "include")
                              invisible(self)
                            },
                            #' @description
                            #' Change signifier hide value
                            #' @param id The signifier id.
                            #' @param value Boolean. The new value.
                            #' @return invisible self
                            change_signifier_hide = function(id, value) {
                              self$change_signifier_property_value(id, value, "hide")
                              invisible(self)
                            },
                            #' @description
                            #' export a signifier type header properties to data frame or export ToDo update to multiple properties
                            #' @param ids a vector of ids. Blank means all ids of specified type.
                            #' @param actual_export Boolean, default FALSE, if TRUE csv export otherwise return data frame.
                            #' @param property the property to export Should be valid ("title"    "tooltip"  "allow_na" "fragment" "required" "sticky"   "include"  "hide")
                            #' @param signifier_type The signifier type to export. Default "triad" 
                            #' @param tfw_id A list of one or more framework ids to print properties from 
                            #' @param name_prefix if actual_export TRUE, a prefix to the csv file name. Default blank, default file name only.
                            #' @param keep_only_include TRUE or FALSE, if TRUE only those flagged with include == TRUE returned. 
                            #' @return df of export or invisible self
                            export_signifier_properties = function(ids = "", actual_export = FALSE, property = "title", signifier_type = "triad", tfw_id = "", name_prefix = "", keep_only_include = FALSE) {
                              if (all((ids == "") == TRUE)) {
                                ids <- self$get_signifier_ids_by_type(signifier_type, keep_only_include)
                              } 
                              if (all(tfw_id != "")) {
                                if (all(tfw_id %in% self$get_framework_ids())) {
                                  ret_list <- unlist(purrr::map(ids, function(x) purrr::map(tfw_id, function(y) ifelse(x %in% self$get_linked_framework_ids_by_type(y, signifier_type), x, NA))), recursive = TRUE)
                                  ids <- ret_list[which(!is.na(ret_list))]
                                }
                              }
                              ret_ids <- vector("list", length = length(ids))
                              names(ret_ids) <- ids
                             
                              text_vals <- purrr::imap(ret_ids, ~ self$get_signifier_property(.y, property))
                              export_df <- data.frame(id = ids, text = unname(unlist(text_vals)))
                              colnames(export_df) <- c("id", property)
                              if (actual_export) {
                                write.csv(export_df, paste0(ifelse(trimws(name_prefix) == "", "", paste0(name_prefix, "_")), signifier_type, "_Export", self$get_parent_framework_id(), "_.csv"), row.names = FALSE)
                              } else {
                                return(export_df)
                              }
                            },
                            #' @description Export all signifier header properties for all signifiers in the framework. 
                            #' @param ids A list of one or more signifier ids to include in the report. If blank, all ids. 
                            #' @param actual_export Boolean, default FALSE, if TRUE csv export otherwise return data frame.
                            #' @param signifier_types A list of one or more signifier types to include in the report. If blank, all types. 
                            #' @param name_prefix if actual_export TRUE, a prefix to the csv file name. Default blank, default file name only.
                            #' @return df of export or invisible self 
                            export_signifier_header_properties = function(ids = "", actual_export = FALSE, signifier_types = "", name_prefix = ""){
                             # - todo - just use the ids, sig types and fw_id and loop through creating the data frame - and forget the crazy stuff
                              my_list <- self$signifier_definitions %>% purrr::discard((.) == "No Entries")
                              out <- NULL
                               # passed in signifier ids
                              if (all((ids == "") == TRUE)) {
                                  ids <- self$get_all_signifier_ids()
                              } 
  
                              # passed in signifier types
                              if (all(signifier_types == "") == TRUE) {
                                signifier_types <- self$get_used_signifier_types()
                              }
                              
                              for (sig_type in signifier_types) {
                                
                                sig_values <- my_list[[sig_type]]
                                sig_values_ids <- names(sig_values[which(names(sig_values) %in% ids)])
                                
                                for (sig_id in sig_values_ids) {
                                  # turn the data into a data frame row and bind to out
                                  out <- dplyr::bind_rows(out, base::as.data.frame(base::as.list(sig_values[[sig_id]])[c("type", "id", self$get_signifier_supported_header_properties())]))
                                }
                                
                              }
                              
                              if (actual_export) {
                                write.csv(out, paste0(ifelse(trimws(name_prefix) == "", "", paste0(name_prefix, "_")),  "All_Signifier_Header_Properties_Export_", self$get_parent_framework_id(), "_.csv"), row.names = FALSE)
                              } else {
                                return(out)
                              }
                            },
                            #' @description
                            #' import a signifier header property to apply to signifier definition. ToDo update to multiple properties
                            #' @param df The name of a csv file or data frame to apply.
                            import_signifier_properties = function(df) {
                              if (is.data.frame(df)) {
                                data_df <- df
                              } else {
                                data_df <- read.csv(file = df, check.names = FALSE, stringsAsFactors = FALSE)
                              }
                              purrr::walk2(data_df[[1]], data_df[[2]], ~ self$change_signifier_property_value(.x, .y, property = colnames(data_df)[[2]]))
                            },
                            #' @description
                            #' Change signifier content property value
                            #' @param id The signifier id.
                            #' @param value The new value.
                            #' @param property the property to update. Should be valid for the signifier type.
                            #' @param type The type to change "triad", "dyad" etc. Optinal, default blank. If blank the type will be looked up from id.
                            #' @return invisible self
                            change_signifier_content_proprty_value = function(id, value, property = "", type = "") {
                              if (type == "type") {return(NULL)} # not allowed to change type
                              if (type == "") {type <- self$get_signifier_type_by_id(id)}
                              if (property == "") {property <- "title"}
                              self$signifier_definitions[[type]][[id]][["content"]][[property]] <- value
                              invisible(self)
                            },
                            #' @description
                            #' Change signifier content label value - the labels are used in triads and dyads
                            #' @param id The signifier id.
                            #' @param label The label being updated (values are "top", "left" and "right" ("top" only used in triads))
                            #' @param value The new value.
                            #' @param property the property to update. Should be valid for the signifier type. Optional, if blank, title used.
                            #' @param type The type to change "triad" or "dyad" etc. Optinal, default blank. If blank the type will be looked up from id.
                            #' @return invisible self
                            change_signifier_content_label_value = function(id, label, value, property = "", type = "") {
                              if (type == "type") {return(NULL)} # not allowed to change type
                              if (type == "") {type <- self$get_signifier_type_by_id(id)}
                              if (property == "") {property <- "text"}
                              label <- paste0(label, "_anchor")
                              self$signifier_definitions[[type]][[id]][["content"]][["labels"]][[label]][[property]] <- value
                              invisible(self)
                            },
                            #-----------------------------------------------------------------
                            # Meta abstracated Helper Functions for sliders and lists and columns
                            #-----------------------------------------------------------------
                            #' @description
                            #' Get an anchor property value for the "triad" or "dyad" specified by the id.
                            #' @param id The triad or dyad signifier id whose type to retrieve.
                            #' @param anchor_id Optional. Default "". The id of the anchor to be retrieved. either anchor_id or anchor must be supplied.anchor_id takes preference if both supplied.
                            #' @param anchor Optional, Default "". The anchor to return field from. Anchor is either "left", "right" for dyad/triads and "top" for triads. Either anchor_id or anchor must be supplied.
                            #' @param field Optoinal. Default "text" for the text property. Other allowable values "id", "image", "show_image", "show_label"
                            #' @param type Optional. Default "". Type is "triad" or "dyad". If not supplied, it is looked up based on the id parameter.
                            #' @return A text string of value of the field parameter for the triad or dyad specified.
                            get_anchor_value = function(id, anchor_id = "", anchor = "", field = "text", type = "") {
                              if (type == "") {
                                type <- self$get_signifier_type_by_id(id)
                              }
                              return(do.call(eval(parse(text = paste0("self$get_", type, "_anchor", ifelse(anchor_id == "", "_R6", "_by_id_R6")))), list(id, ifelse(anchor_id == "", anchor, anchor_id)))[[field]])
                            },
                            #' @description
                            #' Get the anchor ids for the triad or dyad specified by the id.
                            #' @param id The signifier id to retrieve anchor ids.
                            #' @param delist Default FALSE If TRUE return the list of ids as an unnamed vector otherwise as a named list ("top"/triads and "left" and "right" for dyads/triads)
                            #' @param type Default "". Optional. The type of signifier, "triad" or "dyad". If blank, type determined by lookup.
                            #' @return A character list/vector of anchor ids.
                            get_anchor_ids = function(id, delist = FALSE, type = "") {
                              if (type == "") {
                                type <- self$get_signifier_type_by_id(id)
                              }
                              if (delist) {
                                return(unname(unlist(do.call(eval(parse(text = paste0("self$get_", type, "_anchor_ids"))), list(id)))))
                              } else {
                                return(do.call(eval(parse(text = paste0("self$get_", type, "_anchor_ids"))), list(id)))
                              }
                            },
                            #' @description
                            #' Get the anchor column names for the compositional content - i.e. Top/Left/Right for triads and Left/Right for dyads.
                            #' @param id The signifier id to retrieve anchor ids.
                            #' @param type Default "". Optional. The type of signifier, "triad" or "dyad". If blank, type determined by lookup.
                            #' @param delist Default FALSE, If TRUE then only the list contents is given (the actual column names) otherwise the ids as headers included. 
                            #' @return A character vector of the data column names for the compositional values for the dyad or triad.
                            get_anchor_compositional_column_names = function(id, type = "", delist = FALSE) {
                              if (type == "") {
                                type <- self$get_signifier_type_by_id(id)
                              }
                              if (delist) {
                                return(unname(unlist(do.call(eval(parse(text = paste0("self$get_", type, "_compositional_column_names"))), list(id)))))
                              } else {
                                return(do.call(eval(parse(text = paste0("self$get_", type, "_compositional_column_names"))), list(id)))
                              }
                            },
                            #' @description
                            #' Get an anchor R6 class instance for the supplied triad or dyad anchor.
                            #' @param id The signifier id to retrieve anchor ids.
                            #' @param anchor_id Optional. Default "". The id of the anchor to be retrieved. One of anchor_id or anchor must be supplied.
                            #' @param anchor Optional, Default "". Optional. The anchor name to return. Values "top" (triad), "left", "right" (triad and dyad)
                            #' @param type Default "". Optional. The type of signifier, "triad" or "dyad". If blank, type determined by lookup.
                            #' @return An R6 class instance of the anchor requested.
                            get_anchor_R6 = function(id, anchor_id = "", anchor = "", type = "") {
                              if (type == "") {
                                type <- self$get_signifier_type_by_id(id)
                              }
                              return(do.call(eval(parse(text = paste0("self$get_", type, "_anchor", ifelse(anchor_id == "", "_R6", "_by_id_R6")))), list(id, ifelse(anchor_id == "", anchor, anchor_id))))
                            },
                            #' @description
                            #' Get a list of list item ids for passed list signifier id. Currently only lists supported but expected imageselect and others to follow.
                            #' @param id The signifier id to retrieve anchor ids.
                            #' @param type Default "". Optional. The type of signifier, "list" only allowable value. If not provided, value obtained via lookup.
                            #' @return A character vector of the list item ids.
                            get_item_ids = function(id, type = "") {
                              if (type == "") {
                                type <- self$get_signifier_type_by_id(id)
                              }
                              return(do.call(eval(parse(text = paste0("self$get_", type, "_items_ids"))), list(id)))
                            },
                            #-----------------------------------------------------------------
                            # Parent Framework Helper Functions
                            # These functions enable processing of the patent framework for linked frameworks
                            # identifiction of their signifiers and signifier types.
                            # These are not used for the full array of helper functions - to
                            # use the full helper functions use the linked framework helper functions
                            # to give you an id or set of ids.
                            #-----------------------------------------------------------------
                            #' @description
                            #' Get parent id
                            #' @return A character string of the parent fraework id
                            get_parent_id = function() {
                              return(names(self$parent_framework))
                            },
                            #' @description
                            #' Get parent id
                            #' @return A character string of the parent fraework id
                            get_parent_name = function() {
                              return(unlist(unname(self$parent_framework)))
                            },
                            #' @description
                            #' Get parent signifier ids
                            #' @param include_types Boolean TRUE if the signifier types to be included or FALSE (default) for just the ids. 
                            #' @return A vector or list of the signifier ids of the parent framework with their types if requested. 
                            get_parent_signifier_ids = function(include_types = FALSE) {
                              if (include_types) {
                                return(self$types_by_signifierid_framework[[self$get_parent_framework_id()]])
                              }
                              return(names(self$types_by_signifierid_framework[[self$get_parent_framework_id()]]))
                            },
                            #' @description
                            #' Get parent framework signifier ids by type
                            #' @param type The signifier type.
                            #' @return A vector of signifier ids.
                            get_parent_framework_signifier_ids_by_type = function(type = NULL) {
                              if (is.null(type)) {
                                return(self$get_linked_framework_ids_by_type(self$get_parent_framework_id()))
                              }
                              return(self$get_linked_framework_ids_by_type(self$get_parent_framework_id(), type = type))
                            },
                            #' @description
                            #' Get parent framework triad signifier ids
                            #' @return A vector of triad signifier ids.
                            get_parent_framework_triad_ids = function() {
                              return(self$get_linked_framework_ids_by_type(self$get_parent_framework_id(), type = "triad"))
                            },
                            #' @description
                            #' Get parent framework dyad signifier ids
                            #' @return A vector of dyad signifier ids.
                            get_parent_framework_dyad_ids = function() {
                              return(self$get_linked_framework_ids_by_type(self$get_parent_framework_id(), type = "dyad"))
                            },
                            #' @description
                            #' Get parent framework list signifier ids
                            #' @return A vector of list signifier ids.
                            get_parent_framework_list_ids = function() {
                              return(self$get_linked_framework_ids_by_type(self$get_parent_framework_id(), type = "list"))
                            },
                            #' @description
                            #' Get parent framework stones signifier ids
                            #' @return A vector of stones signifier ids.
                            get_parent_framework_stones_ids = function() {
                              return(self$get_linked_framework_ids_by_type(self$get_parent_framework_id(), type = "stones"))
                            },
                            #' @description
                            #' Get parent framework freetext signifier ids
                            #' @return A vector of freetext signifier ids.
                            get_parent_framework_freetext_ids = function() {
                              return(self$get_linked_framework_ids_by_type(self$get_parent_framework_id(), type = "freetext"))
                            },
                            #' @description
                            #' Get parent framework imageselect signifier ids
                            #' @return A vector of imageselect signifier ids.
                            get_parent_framework_imageselect_ids = function() {
                              return(self$get_linked_framework_ids_by_type(self$get_parent_framework_id(), type = "imageselect"))
                            },
                            #' @description
                            #' Get parent framework audio signifier ids
                            #' @return A vector of audio signifier ids.
                            get_parent_framework_audio_ids = function() {
                              return(self$get_linked_framework_ids_by_type(self$get_parent_framework_id(), type = "audio"))
                            },
                            #' @description
                            #' Get parent framework photo signifier ids
                            #' @return A vector of photo signifier ids.
                            get_parent_framework_photo_ids = function() {
                              return(self$get_linked_framework_ids_by_type(self$get_parent_framework_id(), type = "photo"))
                            },
                            #' @description
                            #' Get parent framework uniqueid signifier ids
                            #' @return A vector of uniqueid signifier ids.(should only return one)
                            get_parent_framework_uniqueid_ids = function() {
                              return(self$get_linked_framework_ids_by_type(self$get_parent_framework_id(), type = "uniaueid"))
                            },
                            #' @description
                            #' Get parent framework type by signifier id
                            #' @param sig_id The signifier id
                            #' @return A character string of the signifier type.
                            get_parent_framework_type_by_signifierid = function(sig_id) {
                              return(self$types_by_signifierid_framework[[self$get_parent_framework_id()]][[sig_id]])
                            },
                            #' @description
                            #' Get linked framework signifier count by framework/type
                            #' @param fw_id The linked framework id
                            #' @param type The signifier type
                            #' @return An integer with the signifier count.
                            get_parent_framework_count_by_type = function(fw_id, type) {
                              return(self$signifier_counts_linked_frameworks_type[[fw_id]][[type]])
                            },
                            #' @description
                            #' Get the signifier ids for all single select lists for the parent framework
                            #' @return A vector of signifier ids.
                            get_parent_single_select_list_ids = function() {
                              return(names(self$signifier_definitions[["list"]] %>%
                                             purrr::keep(names(.) %in% self$get_parent_framework_list_ids()) %>%
                                             private$get_max_responses() %>%
                                             purrr::keep((.) == 1)))
                            },
                            #' @description
                            #' Get the signifier ids for all multi select lists for the parent framework
                            #' @return A vector of signifier ids.
                            get_parent_multi_select_list_ids = function() {
                              my_ret <- names(self$signifier_definitions[["list"]] %>%
                                                purrr::keep(names(.) %in% self$get_parent_framework_list_ids()) %>%
                                                private$get_max_responses() %>%
                                                purrr::keep((.) > 1))
                              if (length(my_ret) == 0) {
                                return(NULL)
                              } else {return(my_ret)}
                            },
                            #' @description
                            #' Remove a signifier definition
                            #' @param tid the signifier id to remove
                            #' @param tfw_id The framework IDs of the signifier to remove, if blank, code will find and use all occurrences 
                            #' @param ttype the signifier type to remove. Optional, if blank, looked up.
                            #' @return invisible self                         
                            remove_signifier_definition = function(tid, tfw_id = "", ttype = "") {
                              if (ttype == "") {ttype <- self$get_signifier_type_by_id(tid)}
                              if (any(tfw_id == "") == TRUE) {tfw_id <- self$get_framework_for_id(tid, ttype)}
                              purrr::walk(tfw_id, ~ {private$remove_signifier_reference(tid, fw_id, ttype)})
                              invisible(self)
                            },
                            #-----------------------------------------------------------------
                            # linked Framework Helper Functions (includes parent when you have the framework id for parent)
                            # These functions enable processing of the linked frameworks and
                            # identifiction of their signifiers and signifier types.
                            # These are not used for the full array of helper functions - to
                            # use the full helper functions use the linked framework helper functions
                            # to give you an id or set of ids.
                            #-----------------------------------------------------------------
                            #' @description
                            #' Is a linked framework
                            #' @return Boolean TRUE if the framework is a linked framework otherwise FALSE
                            is_linked_framework = function() {
                              if (is.null(self$get_linked_framework_ids())) {return(FALSE)}
                              return(TRUE)
                            },
                            #' @description
                            #' Get linked framework count
                            #' @return An integer of the number of linked frameworks
                            get_linked_framework_count = function() {
                              return(length(self$get_linked_framework_ids()))
                            },
                            #' @description
                            #' Get parent framework id
                            #' @return Parent fraework id
                            get_parent_framework_id = function() {
                              return(names(self$parent_framework))
                            },
                            #' Get linked framework parent name
                            #' @return Parent name
                            get_parent_framework_name = function() {
                              return(unname(unlist(self$parent_framework)))
                            },
                            #' @description
                            #' Get linked framework ids
                            #' @param include_names TRUE to return list that includes the framework names. 
                            #' @return A vector of linked framework ids or list of ids including names. 
                            get_linked_framework_ids = function(include_names = FALSE) {
                              framework_ids <- self$frameworks[-which(names(self$frameworks) == self$get_parent_framework_id())]
                              if (length(framework_ids) == 0) {return(NULL)}
                              if (include_names) {
                                return(framework_ids)
                              }
                              return(names(framework_ids))
                            },
                            #' @description
                            #' Get linked framework names
                            #' @param fw_id The framework ID to return. Default NULL, include all linked frameworks. 
                            #' @param include_ids Default FALSE, do not include ids with names. TRUE to include the ids with the name. 
                            #' @return A vector of linked framework names
                            get_linked_framework_names = function(fw_id = NULL, include_ids = FALSE) {
                              frameworks <- self$get_linked_framework_ids(include_names = TRUE)
                              if (!is.null(fw_id)) {
                                frameworks <- frameworks[fw_id]
                              }
                              list_names <- unname(unlist((frameworks)))
                              if (include_ids) {
                                ret_list <- as.list(names(frameworks))
                                names(ret_list) <- list_names
                                return(ret_list)
                              }
                              return(list_names)
                            },
                            #' @description
                            #' Get linked framework list
                            #' @return A named list of linked frameworks- names ids, values names.
                            get_linked_framework_list = function() {
                              return(self$get_linked_framework_ids(include_names = TRUE))
                            },
                            #' @description
                            #' Get linked framework list widget.
                            #' @param include_names Default TRUE, shows the framework titles as list names otherwise IDs only. 
                            #' @return A named list of linked frameworks IDs, with framework titles as names (if include_names TRUE)
                            get_linked_framework_mcq_list = function(include_names = TRUE) {
                              ret_list <- self$get_linked_framework_ids()
                              if (include_names) { 
                                names(ret_list) <- self$get_linked_framework_names()
                              }
                              return(ret_list)
                            },
                            #' @description
                            #' Get list ids that are used to branch linked frameworks (only one from primary). .
                            #' @return A vector of list ids used to enable linked framework branch selection. .
                            get_linked_framework_selection_lists = function() {
                              if (!self$is_linked_framework()) {return(NULL)} 
                              return(unique(igraph::V(self$framework_graph)$list)[-which(unique(igraph::V(self$framework_graph)$list) == "Top")])
                            },
                            #' @description
                            #' Get the MCQ list id that selects the linked framework passed.
                            #' @param fw_id The linked framework id.
                            #' @return The list id
                            get_linked_framework_mcq_list_id = function(fw_id) {
                              return(igraph::V(self$framework_graph)$list[[which(fw_id == igraph::V(self$framework_graph)$id)]])
                            },
                            #' @description
                            #' Get the MCQ list item id that selects the linked framework passed
                            #' @param fw_id The linked framework id.
                            #' @return The list item id
                            get_linked_framework_mcq_list_item_id = function(fw_id) {
                              return(igraph::V(self$framework_graph)$item[[which(fw_id == igraph::V(self$framework_graph)$id)]])
                              #return(self$list_ids_by_other[[self$linked_framework_other_sig[[fw_id]]]])
                            },
                            #' @description
                            #' Get the MCQ list and list item id that selects the linked framework passed
                            #' @param fw_id The linked framework id.
                            #' @return The list item id
                            get_linked_framework_mcq_list_item_id_list = function(fw_id) {
                              temp_list <- list(self$get_linked_framework_mcq_list_item_id(fw_id))
                              names(temp_list) <- self$get_linked_framework_mcq_list_id(fw_id)
                              return(temp_list)
                            },
                            #' @description
                            #' Get linked framework signifier ids by type
                            #' @param fw_id The linked framework id.
                            #' @param type The signifier type.
                            #' @return A vector of signifier ids.
                            get_linked_framework_ids_by_type = function(fw_id, type = NULL) {
                              if (is.null(type)) {
                                return(self$signifierids_by_type_framework[[fw_id]])
                              } else {
                                return(self$signifierids_by_type_framework[[fw_id]][[type]])
                              }
                            },
                            #' @description
                            #' Get linked framework triad signifier ids
                            #' @param fw_id The linked framework id.
                            #' @return A vector of triad signifier ids.
                            get_linked_framework_triad_ids = function(fw_id) {
                              return(self$signifierids_by_type_framework[[fw_id]][["triad"]])
                            },
                            #' @description
                            #' Get linked framework dyad signifier ids
                            #' @param fw_id The linked framework id.
                            #' @return A vector of dyad signifier ids.
                            get_linked_framework_dyad_ids = function(fw_id) {
                              return(self$signifierids_by_type_framework[[fw_id]][["dyad"]])
                            },
                            #' @description
                            #' Get linked framework list signifier ids
                            #' @param fw_id The linked framework id.
                            #' @return A vector of list signifier ids.
                            get_linked_framework_list_ids = function(fw_id) {
                              return(self$signifierids_by_type_framework[[fw_id]][["list"]])
                            },
                            #' @description
                            #' Get linked framework list demographics signifier ids
                            #' @param fw_id The linked framework id.
                            #' @return A vector of list signifier ids.
                            get_linked_framework_list_demographics_ids = function(fw_id) {
                                list_ids <- self$get_linked_framework_list_ids(fw_id)
                                ret_list <- vector("list", length = length(list_ids))
                                names(ret_list) <- list_ids
                                dem_list <- purrr::imap(ret_list, ~ self$get_signifier_sticky(.y)) %>% purrr::keep(. == TRUE)
                                return(names(dem_list))
                            },
                            #' @description
                            #' Get linked framework stones signifier ids
                            #' @param fw_id The linked framework id.
                            #' @return A vector of stones signifier ids.
                            get_linked_framework_stones_ids = function(fw_id) {
                              return(self$signifierids_by_type_framework[[fw_id]][["stones"]])
                            },
                            #' @description
                            #' Get linked framework freetext signifier ids
                            #' @param fw_id The linked framework id.
                            #' @return A vector of freetext signifier ids.
                            get_linked_framework_freetext_ids = function(fw_id) {
                              return(self$signifierids_by_type_framework[[fw_id]][["freetext"]])
                            },
                            #' @description
                            #' Get linked framework imageselect signifier ids
                            #' @param fw_id The linked framework id.
                            #' @return A vector of imageselect signifier ids.
                            get_linked_framework_imageselect_ids = function(fw_id) {
                              return(self$signifierids_by_type_framework[[fw_id]][["imageselect"]])
                            },
                            #' @description
                            #' Get linked framework audio signifier ids
                            #' @param fw_id The linked framework id.
                            #' @return A vector of audio signifier ids.
                            get_linked_framework_audio_ids = function(fw_id) {
                              return(self$signifierids_by_type_framework[[fw_id]][["audio"]])
                            },
                            #' @description
                            #' Get linked framework photo signifier ids
                            #' @param fw_id The linked framework id.
                            #' @return A vector of photo signifier ids.
                            get_linked_framework_photo_ids = function(fw_id) {
                              return(self$signifierids_by_type_framework[[fw_id]][["photo"]])
                            },
                            #' @description
                            #' Get linked framework uniqueid signifier ids ToDo - not sure on what this was suppose to do
                            #' @param fw_id The linked framework id.
                            #' @return A vector of uniqueid signifier ids.(should only return one)
                            get_linked_framework_uniqueid_ids = function(fw_id) {
                              return(self$signifierids_by_type_framework[[fw_id]][["uniqueid"]])
                            },
                            #' @description
                            #' Get linked framework signifier type by signifier id
                            #' @param fw_id The linked framework id.
                            #' @param sig_id The signifier id
                            #' @return A character string of the signifier type.
                            get_linked_framework_type_by_signifierid = function(fw_id, sig_id) {
                              return(self$types_by_signifierid_framework[[fw_id]][[sig_id]])
                            },
                            #' @description
                            #' Get linked framework signifier count by type
                            #' @param fw_id The linked framework id.
                            #' @param type The signifier type
                            #' @return An integer with the signifier count.
                            get_linked_framework_count_by_type = function(fw_id, type) {
                              return(self$signifier_counts_linked_frameworks_type[[fw_id]] %>% dplyr::filter(types == type) %>% .$n)
                            },
                            #' @description
                            #' Get used signifier types by framework
                            #' @param fw_id The linked framework id.
                            #' @return A vector with the signifier types used in the linked framework.
                            get_linked_framework_used_signifier_types = function(fw_id) {
                              return(self$types_with_signifiers_framework[[fw_id]])
                            },
                            #' @description
                            #' Get the signifier ids for all single select lists for a linked framework
                            #' @param fw_id the linked framework id
                            #' @return A vector of signifier ids.
                            get_linked_single_select_list_ids = function(fw_id) {
                              my_ret <- names(self$signifier_definitions[["list"]] %>%
                                                purrr::keep(names(.) %in% self$get_linked_framework_list_ids(fw_id)) %>%
                                                private$get_max_responses() %>%
                                                purrr::keep((.) == 1))
                              if (length(my_ret) == 0) {
                                return(NULL)
                              } else {return(my_ret)}
                            },
                            #' @description
                            #' Get the signifier ids for all multi select lists for a linked framework
                            #' @param fw_id the linked framework id
                            #' @return A vector of signifier ids.
                            get_linked_multi_select_list_ids = function(fw_id) {
                              # ToDo - combine with previous - repeated code. 
                              my_ret <- names(self$signifier_definitions[["list"]] %>%
                                                purrr::keep(names(.) %in% self$get_linked_framework_list_ids(fw_id)) %>%
                                                private$get_max_responses() %>%
                                                purrr::keep((.) > 1))
                              if (length(my_ret) == 0) {
                                return(NULL)
                              } else {return(my_ret)}
                            },
                            # 
                            #' @description
                            #' Get the framework IDs for a given signifier id (and optionally type) (signifier library can contain multiple frameworks for an id)
                            #' @param tsig_id the signifier id
                            #' @param ttype the signifier type
                            #' @return A vector containing The framework ids
                            get_framework_for_id = function(tsig_id, ttype = "") {
                              if (ttype == "") {
                                ttype <- self$get_signifier_type_by_id(id = tsig_id)
                              }
                              return(unlist(purrr::map(self$get_framework_ids(), private$check_framework_for_id, ttype, tsig_id)))
                            },
                            
                            #-----------------------------------------------------------------
                            # List Helper Functions
                            #-----------------------------------------------------------------
                            #' @description
                            #' Get list count
                            #' @return An integer of the number of list occurences
                            get_list_count = function() {
                              return(self$get_signifier_count_by_type("list"))
                            },
                            #' @description
                            #' Get list ids
                            #' @param keep_only_include TRUE or FALSE, if TRUE only those flagged with include == TRUE returned.
                            #' @return A vector of the framework list ids
                            get_list_ids = function(keep_only_include = FALSE) {
                              return(self$get_signifier_ids_by_type("list", keep_only_include))
                            },
                            #' @description
                            #' Get list of list titles with list ids as headers
                            #' @param delist Whether to delist returned list (no ids as headers)
                            #' @param keep_only_include TRUE or FALSE, if TRUE only those flagged with include == TRUE returned.
                            #' @return A vector of the framework list titles if delist otherwise list of titles with ids as names
                            get_list_titles = function(delist = FALSE, keep_only_include = FALSE) {
                              ret_list <- purrr::map(self$get_signifier_ids_by_type("list", keep_only_include), ~{self$get_signifier_title(.x)})
                              if (delist) {return(unlist(ret_list))}
                              names(ret_list) <- self$get_signifier_ids_by_type("list", keep_only_include)
                              return(ret_list)
                            },
                            #' @description
                            #' Get list demographic ids (sticky = TRUE)
                            #' @return A vector of ids of demographic lists
                            get_list_demographics_ids = function() {
                              list_ids <- self$get_list_ids()
                              ret_list <- vector("list", length = length(list_ids))
                              names(ret_list) <- list_ids
                              dem_list <- purrr::imap(ret_list, ~ self$get_signifier_sticky(.y)) %>% purrr::keep(. == TRUE)
                              return(names(dem_list))
                            },
                            #' @description
                            #' Get ids of multi-select lists
                            #' @return A vector of the list ids that are  multi-select
                            get_multiselect_list_ids = function() {
                              return(self$get_list_ids()[which(unlist(purrr::map(self$get_list_ids(), ~{self$get_list_max_responses(.x)})) >1)])
                            },
                            #' @description
                            #' Get all colunm names of all  multi-select lists
                            #' @return A vector of the column names of all multiselect lists
                            get_all_multiselect_list_column_names = function() {
                              return(unlist(purrr::map(self$get_multiselect_list_ids(), ~{self$get_list_column_names(.x)})))
                            },
                            #' @description
                            #' Get the maximum allowable selections for a list signifier
                            #' @param id The signifier id of the list whose maximum responses is to be retrieved.
                            #' @return A positive integer value of the maximum number of allowable selections for the list signifier id passed in.
                            get_list_max_responses = function(id) {
                              return(self$get_signifier_content_R6(id)[["max_responses"]])
                            },
                            #' @description
                            #' Get the minimum allowable selections for a list signifier
                            #' @param id The signifier id of the list whose minimum responses is to be retrieved.
                            #' @return A positive integer value of the minimum number of allowable selections for the list signifier id passed in.
                            get_list_min_responses = function(id) {
                              return(self$get_signifier_content_R6(id)[["min_responses"]])
                            },
                            #' @description
                            #' Get the number of items defined for the  for passed list.
                            #' @param id The signifier id of the list whose other number of items is to be retrieved.
                            #' @return An integer value of the number of items defined for the passed list.
                            get_list_num_items = function(id) {
                              return(self$get_signifier_content_R6(id)[["num_items"]])
                            },
                            #' @description
                            #' Get a list of the R6 class instances for each list item in the passed list.
                            #' @param id The signifier id of the list whose R6 class instances of each list item to be retrieved.
                            #' @return A list of R6 class instances of each list item for the passed list.
                            get_list_items_R6 = function(id) {
                              return(self$get_signifier_content_R6(id)[["items"]])
                            },
                            #' @description
                            #' Get a vector of the item ids for the passed list.
                            #' @param id The signifier id of the list whose item ids to be returned.
                            #' @return A vector of list item ids for the passed list.
                            get_list_items_ids = function(id) {
                              return(names(self$get_signifier_content_R6(id)[["items"]]))
                            },
                            #' @description
                            #' Get a vector of the data column names for the passed list.
                            #' @param id The signifier id of the list whose data column names to be returned.
                            #' @return A vector of list column names for the passed list. Single value for single select list Multiple values for multi-select list.
                            get_list_column_names = function(id) {
                              if (!(id %in% self$get_all_signifier_ids())) {return(NULL)}
                              if (self$get_signifier_type_by_id(id) != "list") {return(NULL)}
                              if (self$get_list_max_responses(id) == 1) {return(id)}
                              return(paste0(id, "_", self$get_list_items_ids(id)))
                            },
                            #' @description
                            #' Get a vector of the list item titles for the passed list.
                            #' @param id The signifier id of the list whose titles to be returned.
                            #' @return A vector of list titles for the passed list.
                            get_list_items_titles = function(id) {
                              return(unname(unlist(purrr::map(self$get_signifier_content_R6(id)$items, ~{.x$title}))))
                            },
                            #' @description
                            #' Deduplicate list item titles (required for workbench multi-select MCQs.
                            #' @param id The signifier id to be deduplicated. Default NULL, all MCQs, multi or single.
                            #' @param multi_select_only Default TRUE, only process the multi-select mcqs (important for workbench)
                            #' @param append Default "A_" each duplication will be appended with this strung plus an integer of the repeat. 
                            #' @param prefix_suffix Default "prefix", append value at the start of the title otherwise "suffix" to append at the end. 
                            #' @return A vector of list titles for the passed list.
                            dedup_list_item_titles = function(id = NULL, multi_select_only = TRUE, append = "A_", prefix_suffix = "prefix") {
                              if (!is.null(id)) {
                                temp_list <- id
                              } else {
                                if (multi_select_only) {
                                  temp_list <- self$get_multiselect_list_ids()
                                } else {
                                  temp_list <- self$get_list_ids()
                                }
                              }
                              
                              for (list_id in temp_list) {
                                item_ids <- self$get_list_items_ids(list_id)
                                item_titles <- self$get_list_items_titles(list_id)
                                which_dup <- which(item_titles ==   item_titles[which(duplicated(item_titles) == TRUE)])
                                if (length(which_dup) > 1) {
                                  for (j in seq_along(which_dup)) {
                                    if (j > 1) {
                                      new_append <- paste0(append, j)
                                      if (prefix_suffix == "prefix") {
                                        new_title <- paste0(new_append, "_", item_titles[[which_dup[[j]]]])
                                      } else {
                                        new_title <- paste0(item_titles[[which_dup[[j]]]], "_", new_append)
                                      }
                                      self$signifier_definitions$list[[list_id]]$content$items[[item_ids[[which_dup[[j]]]]]]$title <- new_title
                                    }
                                  }
                                }
                              }
                            },
                            #' @description
                            #' Get a named list of the list item ids (values) and titles (names) for the passed list. Used for shiny dropdown lists.
                            #' @param id The signifier id of the list whose ids and titles to be returned.
                            #' @return A named list of list ids (values) and titles (names) for the passed list.
                            get_list_items_mcq_list = function(id) {
                              vals  <- purrr::map(self$get_signifier_content_R6(id)$items, ~{.x$title})
                              name_vals <- unname(unlist(vals))
                              id_vals <- names(vals)
                              names(id_vals) <- name_vals
                              return(id_vals)
                            },
                            #' @description
                            #' Get a vector of the data column names for the passed list named with the item titles.
                            #' @param id The signifier id of the list whose data column names to be returned.
                            #' @return A vector of list column names for the passed list. Single value for single select list Multiple values for multi-select list with title as names.
                            get_list_column_mcq_names = function(id) {
                              if (!(id %in% self$get_all_signifier_ids())) {return(NULL)}
                              if (self$get_signifier_type_by_id(id) != "list") {return(NULL)}
                              if (self$get_list_max_responses(id) == 1) {
                                ret_id <- as.list(id)
                                names(ret_id) <- self$get_signifier_title(id)
                                return(ret_id)
                              }
                              col_names <- as.list(paste0(id, "_", self$get_list_items_ids(id)))
                              vals  <- purrr::map(self$get_signifier_content_R6(id)$items, ~{.x$title})
                              names(col_names) <- unname(unlist(vals))
                              return(col_names)
                            },
                            #' @description
                            #' Get the R6 class instance of the passed list and list item.
                            #' @param sig_id The signifier id of the list whose list item R6 class instance to be returned.
                            #' @param item_id The signifier item id of the list whose list item R6 class instance to be returned.
                            #' @return An R6 class instance of the list item requested.
                            get_list_item_R6 = function(sig_id, item_id) {
                              return(self$get_list_items_R6(sig_id)[[item_id]])
                            },
                            #' @description
                            #' Get the title of a list item
                            #' @param sig_id The signifier id of the list whose list item title to be returned.
                            #' @param item_id The signifier item id of the list whose list item item title to be returned.
                            #' @return A string containing the list item title.
                            get_list_item_title = function(sig_id, item_id) {
                              return(self$get_list_item_R6(sig_id, item_id)[["title"]])
                            },
                            #' @description
                            #' Get the tooltip of a list item
                            #' @param sig_id The signifier id of the list whose list item tooltip to be returned.
                            #' @param item_id The signifier item id of the list whose list item item tooltip to be returned.
                            #' @return A string containing the list item tooltip
                            get_list_item_tooltip = function(sig_id, item_id) {
                              return(self$get_list_items_R6(sig_id)[[item_id]][["tooltip"]])
                            },
                            #' @description
                            #' Get the is visible of a list item
                            #' @param sig_id The signifier id of the list whose list item "is visible" to be returned.
                            #' @param item_id The signifier item id of the list whose list item item "is visible" to be returned.
                            #' @return A Boolean indicating whether the anchor is visible (TRUE) or not (FALSE)
                            get_list_item_visible = function(sig_id, item_id) {
                              return(self$get_list_items_R6(sig_id)[[item_id]][["visible"]])
                            },
                            #' @description
                            #' Get the is other_signifier_id of a list item
                            #' @param sig_id The signifier id of the list whose list item "is visible" to be returned.
                            #' @param item_id The signifier item id of the list whose list item item "is visible" to be returned.
                            #' @return A GUID of the other_signifier_id
                            get_list_item_other_signifier_id = function(sig_id, item_id) {
                              return(self$get_list_items_R6(sig_id)[[item_id]][["other_signifier_id"]])
                            },
                            #' @description
                            #' Get the is image of a list item
                            #' @param sig_id The signifier id of the list whose list item image URL to be returned.
                            #' @param item_id The signifier item id of the list whose list item item image URL to be returned.
                            #' @return A Boolean indicating whether the anchor is visible.
                            get_list_item_image = function(sig_id, item_id) {
                              return(self$get_list_items_R6(sig_id)[[item_id]][["image"]])
                            },
                            #' @description
                            #' Get the signifier ids for all single select lists
                            #' @return A vector of signifier ids.
                            get_single_select_list_ids = function() {
                              my_ret <- names(self$signifier_definitions[["list"]] %>%
                                                private$get_max_responses() %>%
                                                purrr::keep((.) == 1))
                              if (length(my_ret) == 0) {
                                return(NULL)
                              } else {return(my_ret)}
                            },
                            #                         We already have this as get_multiselect_
                            #                          #' @description
                            #                          #' Get the signifier ids for all multiple select select lists
                            #                          #' @return A vector of signifier ids.
                            #                          get_multi_select_list_ids = function() {
                            #                           my_ret <- names(self$signifier_definitions[["list"]] %>%
                            #                                              private$get_max_responses() %>%
                            #                                              purrr::keep((.) > 1))
                            #                            if (length(my_ret) == 0) {
                            #                              return(NULL)
                            #                            } else {return(my_ret)}
                            #                         },
                            #' @description
                            #' Get the list ids that have an other freetext signifier id
                            #' @param list_ids a vector of list ids to check. Default blank for all list ids
                            #' @param as_named_list a boolean. Default FALSE. If TRUE named list returned with names the list ids.
                            #' @return A vector or named list of the other signifier ids. 
                            get_list_other_ids = function(list_ids = "", as_named_list = FALSE) {
                              # old fashioned way for now
                              other_id_list <- NULL
                              if (all(list_ids == "")) {
                                list_ids <- self$get_list_ids()
                              }
                              for (list_id in list_ids) {
                                for (item_id in self$get_list_items_ids(list_id)) {
                                  other_id <- self$get_list_item_other_signifier_id(sig_id = list_id, item_id = item_id)
                                  if (!is.na(other_id)) {
                                    other_type <- self$get_signifier_type_by_id(other_id)
                                    if (!is.null(other_type)) {
                                      if (other_type == "freetext") {
                                        temp_list <- list(list_id)
                                        names(temp_list) <- other_id
                                        other_id_list <- append(other_id_list, temp_list)
                                      }
                                    }
                                  }
                                }
                              }
                              if (is.null(other_id_list)) {return(NULL)}
                              if (as_named_list) {
                                return(other_id_list)
                              } else {
                                return(names(other_id_list))
                              }
                            },
                            #' @description
                            #' get list whose names are the list IDs from other item ids that are others
                            #' @param other_ids a vector of list ids to check. Default blank for all list ids
                            #' @param as_named_list a boolean. Default FALSE. If TRUE named list returned with names the list ids.
                            #' @return A vector or named list of results. 
                            get_list_ids_by_other = function(other_ids = "", as_named_list = FALSE) {
                              if (all((other_ids == ""))) {
                                ret_list <- unname(unlist(self$get_list_other_ids(as_named_list = TRUE)))
                                if (as_named_list) {
                                   result_list <- as.list(names(self$get_list_other_ids(as_named_list = TRUE)))
                                   names(result_list) <- ret_list
                                   ret_list <- result_list
                                }
                              } else {
                                ret_list <- unname(unlist(self$get_list_other_ids(list_ids = other_ids, as_named_list = TRUE)))
                                if (as_named_list) {
                                  result_list <- as.list(names(self$get_list_other_ids(list_ids = other_ids, as_named_list = TRUE)))
                                  names(result_list) <- ret_list
                                  ret_list <- result_list
                                }
                              }
                             return(ret_list)
                            },
                            #' @description
                            #' get list whose names are the  frameworkids and values the linked framework MCQ ids determining linked frameworks.
                            #' @param list_ids a vector of list ids to check. Default blank for all list ids
                            #' @param as_named_list a boolean. Default FALSE. If TRUE named list returned with names the list ids.
                            #' @return A vector or named list of results. 
                            get_linked_fw_list = function(list_ids = "", as_named_list = TRUE) {
                              if (all(list_ids == "")) {
                                list_ids <- self$get_linked_framework_ids()
                              } 
                              temp_list <- as.list(purrr::map(list_ids, ~ {self$get_linked_framework_mcq_list_id(.x)}))
                              if (as_named_list) {
                                names(temp_list) <- list_ids
                                return(temp_list)
                              } else {
                                return(unname(unlist(temp_list)))
                              }
                            },
                            #' @description
                            #' Update the list content properties
                            #' @param id the signifier id
                            #' @param property the property to update (valid values "max_responses", "min_responses", "random_order")
                            #' @param value the new value.
                            #' @return invisible self
                            update_list_content_property = function(id, property, value) {
                              # property must belong to a list
                              assertive::assert_is_identical_to_true(all(property %in% c("max_responses", "min_responses")), severity = "stop")
                              self$change_signifier_content_proprty_value(id, value, property, type = "list")
                              invisible(self)
                            },
                            #' @description
                            #' Update the list content item properties
                            #' @param sig_id the signifier id
                            #' @param item_id the item id
                            #' @param property the property to update (valid values image", "title", "tooltip", "visible", "other_signifier_id")
                            #' @param value the new value.
                            #' @return invisible self
                            update_list_content_item_property = function(sig_id, item_id, property, value) {
                              # property must belong to a list
                              assertive::assert_is_identical_to_true(all(property %in% c("image", "title", "tooltip", "visible", "other_signifier_id")), severity = "stop")
                              self$signifier_definitions[["list"]][[sig_id]][["content"]][["items"]][[item_id]][[property]] <- value
                              invisible(self)
                            },
                            #' @description
                            #' Update the list item title
                            #' @param sig_id the signifier id
                            #' @param item_id The item id
                            #' @param property The property to update
                            #' @param value the new value.
                            #' @return invisible self
                            update_list_content_item_title = function(sig_id, item_id, value) {
                              self$signifier_definitions[["list"]][[sig_id]][["content"]][["items"]][[item_id]][["title"]] <- value
                              invisible(self)
                            },
                            #' @description
                            #' Export list and liste item titles
                            #' @param ids List of list ids to export. Default blank, all lists 
                            #' @param actual_export or return data frame. Default FALSE - return data frame. 
                            #' @param name_prefix prefix to put on the csv file name if actual_export TRUE. 
                            #' @return Invisible self if actual export otherwise a data frame of the triad anchor ids and title. 
                            export_list_titles = function(ids = "", actual_export = FALSE, name_prefix = "") {
                              if (all((ids == "") == TRUE)) {
                                list_ids <- self$get_list_ids()
                              }
                              ret_list <- vector("list", length = length(list_ids))
                              names(ret_list) <- list_ids
                              ret_calc_list <- purrr::imap_dfr(ret_list, private$build_list_export)
                              if (actual_export) {
                                write.csv(x = ret_calc_list, file = paste0(ifelse(trimws(name_prefix) == "", "", paste0(name_prefix, "_")), "ListItemExport_", self$get_parent_framework_name(), "_.csv"), row.names = FALSE)
                                return(invisible(self))
                              } else {
                                return(ret_calc_list)
                              }
                            },
                            #' @description
                            #' import triad and triad anchor titles from csv file
                            #' @param df A dataframe or csv file name containing the import data. 
                            #' @return Invisible self if actual export otherwise a data frame of the triad anchor ids and title. 
                            import_list_titles = function(df) {
                              if (is.data.frame(df)) {
                                data_df <- df
                              } else {
                                data_df <- read.csv(file = df, check.names = FALSE, stringsAsFactors = FALSE)
                              }
                              my_result <-  private$apply_list_conent_update(data_df)
                              return(invisible(self))
                            },
                            #-----------------------------------------------------------------
                            # Triad Helper Functions
                            #-----------------------------------------------------------------
                            #' @description
                            #' Get triad count
                            #' @return An integer of the number of triad occurences
                            get_triad_count = function() {
                              return(self$get_signifier_count_by_type("triad"))
                            },
                            #' @description
                            #' Get triad ids
                            #' @param keep_only_include TRUE or FALSE, if TRUE only those flagged with include == TRUE returned.
                            #' @return A vector of the framework triad ids
                            get_triad_ids = function(keep_only_include = FALSE) {
                              return(self$get_signifier_ids_by_type("triad", keep_only_include))
                            },
                            #' @description
                            #' Get the triad labels R6 class instance. 
                            #' @param id The signifier id of the list whose list item image URL to be returned.
                            #' @return R6 class instance of the triad labels. 
                            get_triad_labels_R6 = function(id) {
                              return(self$get_signifier_content_R6(id)$labels)
                            },
                            #' @description
                            #' Get the pointer image of the passed triad.
                            #' @param id The signifier id of the list whose tryad item pointer image to be returned.
                            #' @return A string of the image URL for the triad impage pointer.
                            get_triad_pointer_image = function(id) {
                              return(self$get_signifier_content_R6(id)$pointer_image)
                            },
                            #' @description
                            #' Get the background image of the passed triad.
                            #' @param id The signifier id of the list whose tryad item background image to be returned.
                            #' @return A string of the image URL for the triad background image.
                            get_triad_background_image = function(id) {
                              return(self$get_signifier_content_R6(id)$background_image)
                            },
                            #' @description
                            #' Get the triad anchor IDs of the passed triad.
                            #' @param id  The signifier id of the triad whose anchor ids are returned.
                            #' @param delist Default FALSE If TRUE return the list of ids as an unnamed vector otherwise as a named list.
                            #' @return A vector or named list of the anchor ids of the passted in triad id.
                            get_triad_anchor_ids = function(id, delist = FALSE) {
                              if (delist) {
                                return(unname(unlist(list(top = self$get_triad_labels_R6(id)[["top_anchor"]]$id, left = self$get_triad_labels_R6(id)[["left_anchor"]]$id, right = self$get_triad_labels_R6(id)[["right_anchor"]]$id))))
                              } else {
                                return(list(top = self$get_triad_labels_R6(id)[["top_anchor"]]$id, left = self$get_triad_labels_R6(id)[["left_anchor"]]$id, right = self$get_triad_labels_R6(id)[["right_anchor"]]$id))
                              }
                            },
                            #' @description
                            #' Get the triad anchor texts of the passed triad.
                            #' @param id  The signifier id of the triad whose anchor ids are returned.
                            #' @param delist Default FALSE If TRUE return the list of ids as an unnamed vector otherwise as a named list.
                            #' @param label_or_id If delist FALSE, return list names to be the "top", "left", "right" labels or the anchor ids. "label" or "id"
                            #' @return A vector or named list of the anchor texts of the passted in triad id.
                            get_triad_anchor_texts = function(id, delist = FALSE, label_or_id = "label") {
                              top_text <- private$removeHTML(self$get_triad_labels_R6(id)[["top_anchor"]]$text)
                              left_text <- private$removeHTML(self$get_triad_labels_R6(id)[["left_anchor"]]$text)
                              right_text <- private$removeHTML(self$get_triad_labels_R6(id)[["right_anchor"]]$text)
                              if (delist) {
                                return(c(top_text, left_text, right_text))
                                #return(unname(unlist(list(top = top_text, left = left_text, right = right_text))))
                              } else {
                                if (label_or_id == "label") {
                                  return(list(top = top_text, left = left_text, right = right_text))
                                } else {
                                  ret_list <- list(top_text, left_text, right_text)
                                  names(ret_list) <- c(self$get_triad_labels_R6(id)[["top_anchor"]]$id, self$get_triad_labels_R6(id)[["left_anchor"]]$id, self$get_triad_labels_R6(id)[["right_anchor"]]$id)
                                  return(ret_list)
                                }
                              }
                            },
                            #' @description
                            #' Get a triad anchor R6 object instance for passed triad signifier id.
                            #' @param sig_id The signifier id of the list whose triad anchor R6 class instance to be returned.
                            #' @param anchor_id Triad anchor id.
                            #' @return An R6 class instasnce of the triad anchor for the passed triad and anchor ids.
                            get_triad_anchor_by_id_R6 = function(sig_id, anchor_id) {
                              return(self$get_triad_labels_R6(sig_id)[[which(self$get_triad_anchor_ids(sig_id) == anchor_id)[[1]]]])
                            },
                            #' @description
                            #' Get a triad anchor R6 object instance for passed triad id and anchor.
                            #' @param id The signifier id of the list whose triad anchor R6 class instance to be returned.
                            #' @param anchor The anchor to return the R6 class instance ("top", "left", "right".
                            #' @return An R6 class instance of the triad anchor.
                            get_triad_anchor_R6 = function(id, anchor) {
                              return(self$get_triad_labels_R6(id)[[paste0(anchor, "_anchor")]])
                            },
                            #' @description
                            #' Get a triad anchor id for the passed triad id and anchor.
                            #' @param id The signifier id of the list whose anchor id to be returned.
                            #' @param anchor The anchor to return the id for ("top", "left", "right".
                            #' @return A string containing the triad anchor id.
                            get_triad_anchor_id = function(id, anchor) {
                              return(self$get_triad_labels_R6(id)[[paste0(anchor, "_anchor")]]$id)
                            },
                            #' @description
                            #' Get the left triad anchor R6 object instance for passed triad signifier id.
                            #' @param id The triad id.
                            #' @return An R6 class instasnce of the left triad anchor for the passed triad id.
                            get_triad_left_anchor_R6 = function(id) {
                              return(self$get_triad_anchor_R6(id, "left"))
                            },
                            #' @description
                            #' Get the top triad anchor R6 object instance for passed triad signifier id.
                            #' @param id The triad id.
                            #' @return An R6 class instasnce of the top triad anchor for the passed triad id.
                            get_triad_top_anchor_R6 = function(id) {
                              return(self$get_triad_anchor_R6(id, "top"))
                            },
                            #' @description
                            #' Get the right triad anchor R6 object instance for passed triad signifier id.
                            #' @param id The triad id.
                            #' @return An R6 class instasnce of the right triad anchor for the passed triad id.
                            get_triad_right_anchor_R6 = function(id) {
                              return(self$get_triad_anchor_R6(id, "right"))
                            },
                            #' @description
                            #' Get triad anchor text for passed in triad id and anchor id.
                            #' @param sig_id The triad id.
                            #' @param anchor_id The anchor id
                            #' @return Character string containing the triad anchor text.
                            get_triad_anchor_text_by_id = function(sig_id, anchor_id) {
                              return(self$get_triad_anchor_by_id_R6(sig_id, anchor_id)$text)
                            },
                            #' @description
                            #' Get triad anchor show image for passed in triad id and anchor id.
                            #' @param sig_id The triad id.
                            #' @param anchor_id The anchor id
                            #' @return Boolean containing the triad anchor show image
                            get_triad_anchor_show_image_by_id = function(sig_id, anchor_id) {
                              return(self$get_triad_anchor_by_id_R6(sig_id, anchor_id)$show_image)
                            },
                            #' @description
                            #' Get triad anchor image URL for passed in triad id and anchor id.
                            #' @param sig_id The triad id.
                            #' @param anchor_id The anchor id
                            #' @return String containing the triad image URL
                            get_triad_anchor_image_by_id = function(sig_id, anchor_id) {
                              return(self$get_triad_anchor_by_id_R6(sig_id, anchor_id)$image)
                            },
                            #' @description
                            #' Get triad anchor show label for passed in triad id and anchor id.
                            #' @param sig_id The triad id.
                            #' @param anchor_id The anchor id
                            #' @return Boolean containing the triad anchor show label
                            get_triad_anchor_show_label_by_id = function(sig_id, anchor_id) {
                              return(self$get_triad_anchor_by_id_R6(sig_id, anchor_id)$show_label)
                            },
                            #' @description
                            #' Get triad anchor text for passed in triad id and anchor.
                            #' @param id The triad id.
                            #' @param anchor The anchor ("top", "left", "right")
                            #' @return Character string containing the triad anchor text.
                            get_triad_anchor_text_by_anchor = function(id, anchor) {
                              return(self$get_triad_anchor_R6(id, anchor)$text)
                            },
                            #' @description
                            #' Get triad anchor show image for passed in triad id and anchor.
                            #' @param id The triad id.
                            #' @param anchor The anchor ("top", "left", "right")
                            #' @return Boolean containing the triad anchor show image
                            get_triad_anchor_show_image_by_anchor = function(id, anchor) {
                              return(self$get_triad_anchor_R6(id, anchor)$show_image)
                            },
                            #' @description
                            #' Get triad anchor image URL for passed in triad id and anchor.
                            #' @param id The triad id.
                            #' @param anchor The anchor ("top", "left", "right")
                            #' @return String containing the triad image URL
                            get_triad_anchor_image_by_anchor = function(id, anchor) {
                              return(self$get_triad_anchor_R6(id, anchor)$image)
                            },
                            #' @description
                            #' Get triad anchor show label for passed in triad id and anchor.
                            #' @param id The triad id.
                            #' @param anchor The anchor ("top", "left", "right")
                            #' @return Boolean containing the triad anchor show label
                            get_triad_anchor_show_label_by_anchor = function(id, anchor) {
                              return(self$get_triad_anchor_R6(id, anchor)$show_label)
                            },
                            #' @description
                            #' Get triad top anchor text for passed in triad id.
                            #' @param id The triad id.
                            #' @return Character string containing the triad anchor text.
                            get_triad_top_anchor_text = function(id) {
                              return(self$get_triad_anchor_R6(id, "top")$text)
                            },
                            #' @description
                            #' Get triad top anchor show image for passed in triad id.
                            #' @param id The triad id.
                            #' @return Boolean containing the triad show image.
                            get_triad_top_anchor_show_image = function(id) {
                              return(self$get_triad_anchor_R6(id, "top")$show_image)
                            },
                            #' @description
                            #' Get triad top anchor image for passed in triad id.
                            #' @param id The triad id.
                            #' @return Character string containing the triad image URL.
                            get_triad_top_anchor_image = function(id) {
                              return(self$get_triad_anchor_R6(id, "top")$image)
                            },
                            #' @description
                            #' Get triad top anchor show label for passed in triad id.
                            #' @param id The triad id.
                            #' @return Boolean containing the triad show label
                            get_triad_top_anchor_show_label = function(id) {
                              return(self$get_triad_anchor_R6(id, "top")$show_label)
                            },
                            #' @description
                            #' Get triad left anchor text for passed in triad id.
                            #' @param id The triad id.
                            #' @return Character string containing the triad  left anchor text.
                            get_triad_left_anchor_text = function(id) {
                              return(self$get_triad_anchor_R6(id, "left")$text)
                            },
                            #' @description
                            #' Get triad left anchor show image for passed in triad id.
                            #' @param id The triad id.
                            #' @return Boolean containing the triad show image.
                            get_triad_left_anchor_show_image = function(id) {
                              return(self$get_triad_anchor_R6(id, "left")$show_image)
                            },
                            #' @description
                            #' Get triad left anchor image for passed in triad id.
                            #' @param id The triad id.
                            #' @return Character string containing the triad image URL
                            get_triad_left_anchor_image = function(id) {
                              return(self$get_triad_anchor_R6(id, "left")$image)
                            },
                            #' @description
                            #' Get triad left anchor show label for passed in triad id.
                            #' @param id The triad id.
                            #' @return Boolean containing the triad show label
                            get_triad_left_anchor_show_label = function(id) {
                              return(self$get_triad_anchor_R6(id, "left")$show_label)
                            },
                            #' @description
                            #' Get triad right anchor text for passed in triad id.
                            #' @param id The triad id.
                            #' @return Character string containing the triad  left anchor text.
                            get_triad_right_anchor_text = function(id) {
                              return(self$get_triad_anchor_R6(id, "right")$text)
                            },
                            #' @description
                            #' Get triad right anchor show image for passed in triad id.
                            #' @param id The triad id.
                            #' @return Boolean containing the triad show image.
                            get_triad_right_anchor_show_image = function(id) {
                              return(self$get_triad_anchor_R6(id, "right")$show_image)
                            },
                            #' @description
                            #' Get triad right anchor show image for passed in triad id.
                            #' @param id The triad id.
                            #' @return Boolean containing the triad show image.
                            get_triad_right_anchor_image = function(id) {
                              return(self$get_triad_anchor_R6(id, "right")$image)
                            },
                            #' @description
                            #' Get triad left anchor show label for passed in triad id.
                            #' @param id The triad id.
                            #' @return Boolean containing the triad show label
                            get_triad_right_anchor_show_label = function(id) {
                              return(self$get_triad_anchor_R6(id, "right")$show_label)
                            },
                            #' @description
                            #' Get triad top anchor id for passed in triad id.
                            #' @param id The triad id.
                            #' @return String of the top anchor id
                            get_triad_top_anchor_id = function(id) {
                              return(self$get_triad_anchor_id(id, "top"))
                            },
                            #' @description
                            #' Get triad left anchor id for passed in triad id.
                            #' @param id The triad id.
                            #' @return String of the left anchor id
                            get_triad_left_anchor_id = function(id) {
                              return(self$get_triad_anchor_id(id, "left"))
                            },
                            #' @description
                            #' Get triad right anchor id for passed in triad id.
                            #' @param id The triad id.
                            #' @return String of the right anchor id
                            get_triad_right_anchor_id = function(id) {
                              return(self$get_triad_anchor_id(id, "right"))
                            },
                            #' @description
                            #' Get data column name for passed in triad id and column.
                            #' @param id The triad id.
                            #' @param column The column to return. ("top", "left", "right", "x", "y", "X", "Y")
                            #' @return Character string of the column name
                            get_triad_column_name = function(id, column) {
                              return(paste0(id, ifelse(column %in% c("x", "y", "X", "Y"), "", "_"), ifelse(column %in% c("x", "y", "X", "Y"), toupper(column), self$get_triad_anchor_id(id, column))))
                            },
                            #' @description
                            #' Get data X column name for passed in triad id.
                            #' @param id The triad id.
                            #' @return Character string of the X column name
                            get_triad_x_column_name = function(id) {
                              return(self$get_triad_column_name(id, "x"))
                            },
                            #' @description
                            #' Get data Y column name for passed in triad id.
                            #' @param id The triad id.
                            #' @return Character string of the Y column name
                            get_triad_y_column_name = function(id) {
                              return(self$get_triad_column_name(id, "y"))
                            },
                            #' @description
                            #' Get data X and Y column name for passed in triad id.
                            #' @param id The triad id.
                            #' @param delist Default FALSE. If TRUE return unnamed vector. 
                            #' @return List of X and Y
                            get_triad_x_y_column_names = function(id, delist = FALSE) {
                              col_names <- c(self$get_triad_column_name(id, "x"), self$get_triad_column_name(id, "y"))
                              if (delist) {return(col_names)}
                              names(col_names) <- c("x", "y")
                              return(col_names)
                            },
                            
                            #' @description
                            #' Get data top column name for passed in triad id.
                            #' @param id The triad id.
                            #' @return Character string of the top column name
                            get_triad_top_column_name = function(id) {
                              return(self$get_triad_column_name(id, "top"))
                            },
                            #' @description
                            #' Get data left column name for passed in triad id.
                            #' @param id The triad id.
                            #' @return Character string of the left column name
                            get_triad_left_column_name = function(id) {
                              return(self$get_triad_column_name(id, "left"))
                            },
                            #' @description
                            #' Get data right column name for passed in triad id.
                            #' @param id The triad id.
                            #' @return Character string of the right column name
                            get_triad_right_column_name = function(id) {
                              return(self$get_triad_column_name(id, "right"))
                            },
                            #' @description
                            #' Get data N/A column name for passed in triad id.
                            #' @param id The triad id.
                            #' @return Character string of the N/A column name
                            get_triad_na_column_name = function(id) {
                              if (self$get_signifier_allow_na(id)) {
                                return(paste0(id, "_NA"))
                              }
                              return(NULL)
                            },
                            #' @description
                            #' Get the compositional anchor data column names for a given triad. Thus "top" "left" and "right" column names only.
                            #' @param id  Triad id.
                            #' @param delist Default FALSE. If FALSE return as unnamed vector otherwise as a named ("top", "left", "right") list.
                            #' @return An unnamed vector or named list of triad compositional anchor data column names.
                            get_triad_compositional_column_names = function(id, delist = FALSE) {
                              if (self$get_signifier_type_by_id(id) != "triad") {return(NULL)}
                              if (delist) {
                                return(c(self$get_triad_top_column_name(id), self$get_triad_left_column_name(id), self$get_triad_right_column_name(id)))
                              } else {
                                return(list(top = self$get_triad_top_column_name(id), left = self$get_triad_left_column_name(id), right = self$get_triad_right_column_name(id)))
                              }
                            },
                            #' @description
                            #' Get the anchor data column names for a given triad. Thus "top" "left"  "right" and N/A if applicable.
                            #' @param id  Triad id.
                            #' @param delist Default FALSE. If FALSE return as unnamed vector otherwise as a named ("top", "left", "right", "na") list.
                            #' @return A named or unnamed vector of triad anchor data column names.
                            get_triad_anchor_column_names = function(id, delist = FALSE) {
                              cols <- c(self$get_triad_top_column_name(id), self$get_triad_left_column_name(id), self$get_triad_right_column_name(id))
                              names(cols) <- c("top", "left", "right")
                              if (self$get_signifier_allow_na(id)) {
                                cols <- c(cols, self$get_triad_na_column_name(id))
                                names(cols) <- c("top", "left", "right", "na")
                              }
                              if (delist) {
                                return(unname(cols))
                              }
                              return(cols)
                            },
                            #' @description
                            #' Get the data column names for a given triad. Thus "x", "y", top" "left"  "right" and N/A if applicable.
                            #' @param id  Triad id.
                            #' @param delist Default FALSE. If FALSE return as unnamed vector otherwise as a named ("top", "left", "right", "na") list.
                            #' @return A named or unnamed vector of triad  data column names.
                            get_triad_all_column_names = function(id, delist = FALSE) {
                              cols <- c(self$get_triad_x_column_name(id), self$get_triad_y_column_name(id), self$get_triad_top_column_name(id), self$get_triad_left_column_name(id), self$get_triad_right_column_name(id))
                              names(cols) <- c("x", "y", "top", "left", "right")
                              if (self$get_signifier_allow_na(id)) {
                                cols <- c(cols, self$get_triad_na_column_name(id))
                                names(cols) <- c("x", "y", "top", "left", "right", "na")
                              }
                              if(delist) {
                                return(unname(cols))
                              }
                              return(cols)
                            }, 
                            #' @description
                            #' update triad label top value
                            #' @param id The signifier id.
                            #' @param value The new value.
                            #' @param property the property to update. Should be valid for the signifier type. Optional, if blank, title used.
                            #' @return invisible self
                            update_triad_top_label_value = function(id, value, property = "") {
                              if (property == "") {property <- "text"}
                              self$signifier_definitions[["triad"]][[id]][["content"]][["labels"]][["top_anchor"]][[property]] <- value
                              invisible(self)
                            },
                            #' @description
                            #' update triad label left value
                            #' @param id The signifier id.
                            #' @param value The new value.
                            #' @param property the property to update. Should be valid for the signifier type. Optional, if blank, title used.
                            #' @return invisible self
                            update_triad_left_label_value = function(id, value, property = "") {
                              if (property == "") {property <- "text"}
                              self$signifier_definitions[["triad"]][[id]][["content"]][["labels"]][["left_anchor"]][[property]] <- value
                              invisible(self)
                            },
                            #' @description
                            #' update triad label right value
                            #' @param id The signifier id.
                            #' @param value The new value.
                            #' @param property the property to update. Should be valid for the signifier type. Optional, if blank, title used.
                            #' @return invisible self
                            update_triad_right_label_value = function(id, value, property = "") {
                              if (property == "") {property <- "text"}
                              self$signifier_definitions[["triad"]][[id]][["content"]][["labels"]][["right_anchor"]][[property]] <- value
                              invisible(self)
                            },
                            #' @description
                            #' Export triad and triad anchor titles
                            #' @param ids List of triad ids to export. Default blank, all triads. 
                            #' @param actual_export or return data frame. Default FALSE - return data frame. 
                            #' @param name_prefix prefix to put on the csv file name if actual_export TRUE. 
                            #' @return Invisible self if actual export otherwise a data frame of the triad anchor ids and title. 
                            export_triad_titles = function(ids = "", actual_export = FALSE, name_prefix = "") {
                              if (all((ids == "") == TRUE)) {
                                triad_ids <- self$get_triad_ids()
                              }
                              ret_list <- vector("list", length = length(triad_ids))
                              names(ret_list) <- triad_ids
                              ret_calc_list <- purrr::imap_dfr(ret_list, private$build_triad_export)
                              if (actual_export) {
                                write.csv(x = ret_calc_list, file = paste0(ifelse(trimws(name_prefix) == "", "", paste0(name_prefix, "_")), "TriadAnchorExport_", self$get_parent_framework_name(), "_.csv"), row.names = FALSE)
                                return(invisible(self))
                              } else {
                                return(ret_calc_list)
                              }
                            },
                            #' @description
                            #' import triad and triad anchor titles from csv file
                            #' @param df data frame or csv file name of the triad data to import. 
                            #' @return Invisible self if actual export otherwise a data frame of the triad anchor ids and title. 
                            import_triad_titles = function(df) {
                              if (is.data.frame(df)) {
                                data_df <- df
                              } else {
                                data_df <- read.csv(file = df, check.names = FALSE, stringsAsFactors = FALSE)
                              }
                              my_result <-  private$apply_triad_conent_update(data_df)
                              return(invisible(self))
                            },
                            #-----------------------------------------------------------------
                            # Dyad Helper Functions
                            #-----------------------------------------------------------------
                            #' @description
                            #' Get dyad count
                            #' @return An integer of the number of dyad occurences
                            get_dyad_count = function() {
                              return(self$get_signifier_count_by_type("dyad"))
                            },
                            #' @description
                            #' Get dyad ids
                            #' @param keep_only_include TRUE or FALSE, if TRUE only those flagged with include == TRUE returned.
                            #' @return A vector of the framework dyad ids
                            get_dyad_ids = function(keep_only_include = FALSE) {
                              return(self$get_signifier_ids_by_type("dyad", keep_only_include))
                            },
                            #' @description
                            #' Get dyad labels for id.
                            #' @param id The dyad id.
                            #' @return An R6 class instance of the dyad anchors
                            get_dyad_labels_R6 = function(id) {
                              return(self$get_signifier_content_R6(id)$labels)
                            },
                            #' @description
                            #' Get dyad pointer image URL for id.
                            #' @param id The dyad id.
                            #' @return A character string of the pointer image URL
                            get_dyad_pointer_image = function(id) {
                              return(self$get_signifier_content_R6(id)$pointer_image)
                            },
                            #' @description
                            #' Get dyad background image URL for id.
                            #' @param id The dyad id.
                            #' @return A character string of the background image URL
                            get_dyad_background_image = function(id) {
                              return(self$get_signifier_content_R6(id)$background_image)
                            },
                            #' @description
                            #' Get dyad anchor ids for id.
                            #' @param id The dyad id.
                            #' @param delist Default FALSE. If FALSE return as unnamed vector otherwise as a named list ("left", "right", "na").
                            #' @return An unamed vector or named list of the dyad anchor ids
                            get_dyad_anchor_ids = function(id, delist = FALSE) {
                              if(delist) {
                                return(c(self$get_dyad_labels_R6(id)[["left_anchor"]]$id,  self$get_dyad_labels_R6(id)[["right_anchor"]]$id))
                              } else {
                                return(list(left = self$get_dyad_labels_R6(id)[["left_anchor"]]$id, right = self$get_dyad_labels_R6(id)[["right_anchor"]]$id))
                              }
                            },
                            #' @description
                            #' Get the dyad anchor texts of the passed dyad
                            #' @param id  The signifier id of the dyad whose anchor ids are returned.
                            #' @param delist Default FALSE If TRUE return the list of ids as an unnamed vector otherwise as a named list.
                            #' @param label_or_id If delist FALSE, return list names to be the  "left", "right" labels or the anchor ids. "label" or "id"
                            #' @return A vector or named list of the anchor texts of the passted in dyad id.
                            get_dyad_anchor_texts = function(id, delist = FALSE, label_or_id = "label") {
                              left_text <- private$removeHTML(self$get_dyad_labels_R6(id)[["left_anchor"]]$text)
                              right_text <- private$removeHTML(self$get_dyad_labels_R6(id)[["right_anchor"]]$text)
                              if (delist) {
                                return(c(left_text, right_text))
                                #return(unname(unlist(list(top = top_text, left = left_text, right = right_text))))
                              } else {
                                if (label_or_id == "label") {
                                  return(list(left = left_text, right = right_text))
                                } else {
                                  ret_list <- list(left_text, right_text)
                                  names(ret_list) <- c(self$get_dyad_labels_R6(id)[["left_anchor"]]$id, self$get_dyad_labels_R6(id)[["right_anchor"]]$id)
                                  return(ret_list)
                                }
                              }
                            },
                            #' @description
                            #' Get dyad anchor id for anchor ("left", "right")
                            #' @param id The dyad id.
                            #' @param anchor Anchor to retrieve anchor id ("left", "right").
                            #' @return A character string containing the dyad anchor id
                            get_dyad_anchor_id = function(id, anchor) {
                              return(self$get_dyad_labels_R6(id)[[paste0(anchor, "_anchor")]]$id)
                            },
                            #' @description
                            #' Get dyad anchor R6 class instance for dyad and anchor id
                            #' @param sig_id The dyad id.
                            #' @param anchor_id Anchor id.
                            #' @return An R6 class instance of the dyad anchor
                            get_dyad_anchor_by_id_R6 = function(sig_id, anchor_id) {
                              return(self$get_dyad_labels_R6(sig_id)[[which(self$get_dyad_anchor_ids(sig_id) == anchor_id)[[1]]]])
                            },
                            #' @description
                            #' Get dyad anchor R6 class instance for dyad and anchor ("left"), "right")
                            #' @param id The dyad id.
                            #' @param anchor The anchor ("left", "right")
                            #' @return An R6 class instance of the dyad anchor
                            get_dyad_anchor_R6 = function(id, anchor) {
                              return(self$get_dyad_labels_R6(id)[[paste0(anchor, "_anchor")]])
                            },
                            #' @description
                            #' Get dyad anchor R6 class instance for dyad left anchor
                            #' @param id The dyad id.
                            #' @return An R6 class instance of the dyad left anchor
                            get_dyad_left_anchor_R6 = function(id) {
                              return(self$get_dyad_anchor_R6(id, "left"))
                            },
                            #' @description
                            #' Get dyad anchor R6 class instance for dyad right anchor
                            #' @param id The dyad id.
                            #' @return An R6 class instance of the dyad right anchor
                            get_dyad_right_anchor_R6 = function(id) {
                              return(self$get_dyad_anchor_R6(id, "right"))
                            },
                            #' @description
                            #' Get dyad anchor text for dyad and anchor id.
                            #' @param sig_id The dyad id.
                            #' @param anchor_id Anchor id.
                            #' @return Character string of dyad anchor text.
                            get_dyad_anchor_text_by_id = function(sig_id, anchor_id) {
                              return(self$get_dyad_anchor_by_id_R6(sig_id, anchor_id)$text)
                            },
                            #' @description
                            #' Get dyad anchor show image for dyad and anchor id.
                            #' @param sig_id The dyad id.
                            #' @param anchor_id Anchor id.
                            #' @return Boolean of dyad anchor show image
                            get_dyad_anchor_show_image_by_id = function(sig_id, anchor_id) {
                              return(self$get_dyad_anchor_by_id_R6(sig_id, anchor_id)$show_image)
                            },
                            #' @description
                            #' Get dyad anchor image for dyad and anchor id.
                            #' @param sig_id The dyad id.
                            #' @param anchor_id Anchor id.
                            #' @return Chracter string of dyad anchor image
                            get_dyad_anchor_image_by_id = function(sig_id, anchor_id) {
                              return(self$get_dyad_anchor_by_id_R6(sig_id, anchor_id)$image)
                            },
                            #' @description
                            #' Get dyad anchor show label for dyad and anchor id.
                            #' @param sig_id The dyad id.
                            #' @param anchor_id Anchor id.
                            #' @return Boolean of dyad anchor show label
                            get_dyad_anchor_show_label_by_id = function(sig_id, anchor_id) {
                              return(self$get_dyad_anchor_by_id_R6(sig_id, anchor_id)$show_label)
                            },
                            #' @description
                            #' Get dyad anchor text for dyad and anchor.
                            #' @param id The dyad id.
                            #' @param anchor Anchor ("left", "right").
                            #' @return Character string of dyad anchor text.
                            get_dyad_anchor_text_by_anchor = function(id, anchor) {
                              return(self$get_dyad_anchor_R6(id, anchor)$text)
                            },
                            #' @description
                            #' Get dyad anchor show image for dyad and anchor.
                            #' @param id The dyad id.
                            #' @param anchor Anchor ("left", "right").
                            #' @return Boolean of dyad anchor show image
                            get_dyad_anchor_show_image_by_anchor = function(id, anchor) {
                              return(self$get_dyad_anchor_R6(id, anchor)$show_image)
                            },
                            #' @description
                            #' Get dyad anchor  image for dyad and anchor.
                            #' @param id The dyad id.
                            #' @param anchor Anchor ("left", "right").
                            #' @return Character string of dyad anchor image URL
                            get_dyad_anchor_image_by_anchor = function(id, anchor) {
                              return(self$get_dyad_anchor_R6(id, anchor)$image)
                            },
                            #' @description
                            #' Get dyad anchor show label for dyad and anchor.
                            #' @param id The dyad id.
                            #' @param anchor Anchor ("left", "right").
                            #' @return Boolean of dyad anchor show label
                            get_dyad_anchor_show_label_by_anchor = function(id, anchor) {
                              return(self$get_dyad_anchor_R6(id, anchor)$show_label)
                            },
                            #' @description
                            #' Get dyad left anchor text for dyad.
                            #' @param id The dyad id.
                            #' @return Character string of dyad left anchor text
                            get_dyad_left_anchor_text = function(id) {
                              return(self$get_dyad_anchor_R6(id, "left")$text)
                            },
                            #' @description
                            #' Get dyad left anchor show image for dyad.
                            #' @param id The dyad id.
                            #' @return Boolean of dyad left anchor show image
                            get_dyad_left_anchor_show_image = function(id) {
                              return(self$get_dyad_anchor_R6(id, "left")$show_image)
                            },
                            #' @description
                            #' Get dyad left anchor image for dyad.
                            #' @param id The dyad id.
                            #' @return Character string of dyad left anchor image URL
                            get_dyad_left_anchor_image = function(id) {
                              return(self$get_dyad_anchor_R6(id, "left")$image)
                            },
                            #' @description
                            #' Get dyad left anchor show label for dyad.
                            #' @param id The dyad id.
                            #' @return Boolean of dyad left anchor show label
                            get_dyad_left_anchor_show_label = function(id) {
                              return(self$get_dyad_anchor_R6(id, "left")$show_label)
                            },
                            #' @description
                            #' Get dyad right anchor text for dyad.
                            #' @param id The dyad id.
                            #' @return Character string of dyad right anchor text
                            get_dyad_right_anchor_text = function(id) {
                              return(self$get_dyad_anchor_R6(id, "right")$text)
                            },
                            #' @description
                            #' Get dyad right anchor show image for dyad.
                            #' @param id The dyad id.
                            #' @return Boolean of dyad right anchor show image
                            get_dyad_right_anchor_show_image = function(id) {
                              return(self$get_dyad_anchor_R6(id, "right")$show_image)
                            },
                            #' @description
                            #' Get dyad right anchor image for dyad.
                            #' @param id The dyad id.
                            #' @return Character string of dyad right anchor image URL
                            get_dyad_right_anchor_image = function(id) {
                              return(self$get_dyad_anchor_R6(id, "right")$image)
                            },
                            #' @description
                            #' Get dyad right anchor show label for dyad.
                            #' @param id The dyad id.
                            #' @return Boolean of dyad right anchor show label
                            get_dyad_right_anchor_show_label = function(id) {
                              return(self$get_dyad_anchor_R6(id, "right")$show_label)
                            },
                            #' @description
                            #' Get dyad left anchor id for dyad.
                            #' @param id The dyad id.
                            #' @return Character string of dyad left anchor id
                            get_dyad_left_anchor_id = function(id) {
                              return(self$get_dyad_anchor_id(id, "left"))
                            },
                            #' @description
                            #' Get dyad right anchor id for dyad.
                            #' @param id The dyad id.
                            #' @return Character string of dyad right anchor id
                            get_dyad_right_anchor_id = function(id) {
                              return(self$get_dyad_anchor_id(id, "right"))
                            },
                            #' @description
                            #' Get dyad anchor ids by dyad.
                            #' @param id The dyad id.
                            #' @param delist Default FALSE. If FALSE return as unnamed vector otherwise as a named list ("left", "right").
                            get_dyad_all_anchor_ids = function(id, delist = FALSE) {
                              if (delist) {
                                return(c(self$get_dyad_left_anchor_id(id), self$get_dyad_right_anchor_id(id)))
                              } else {
                                return(list(left = self$get_dyad_left_anchor_id(id), right = self$get_dyad_right_anchor_id(id)))
                              }
                            },
                            #' @description
                            #' Get dyad column name for dyad and anchor.
                            #' @param id The dyad id.
                            #' @param column Column to retrieve ("x", "X", left", "right").
                            #' @return Character string of the dyad column name
                            get_dyad_column_name = function(id, column) {
                              return(paste0(id, ifelse(column %in% c("x", "X"), "", "_"), ifelse(column %in% c("x", "X"), toupper(column), self$get_dyad_anchor_id(id, column))))
                            },
                            #' @description
                            #' Get dyad x column name for dyad.
                            #' @param id The dyad id.
                            #' @return Character string of the dyad x column name
                            get_dyad_x_column_name = function(id) {
                              return(self$get_dyad_column_name(id, "x"))
                            },
                            #' @description
                            #' Get dyad left column name for dyad.
                            #' @param id The dyad id.
                            #' @return Character string of the dyad left column name
                            get_dyad_left_column_name = function(id) {
                              return(self$get_dyad_column_name(id, "left"))
                            },
                            #' @description
                            #' Get dyad right column name for dyad.
                            #' @param id The dyad id.
                            #' @return Character string of the dyad right column name
                            get_dyad_right_column_name = function(id) {
                              return(self$get_dyad_column_name(id, "right"))
                            },
                            #' @description
                            #' Get dyad N/A column name for dyad.
                            #' @param id The dyad id.
                            #' @return Character string of the dyad N/A column name
                            get_dyad_na_column_name = function(id) {
                              if (self$get_signifier_allow_na(id)) {
                                return(paste0(id, "_NA"))
                              }
                              return(NULL)
                            },
                            #' @description
                            #' Get as percent column name for dyad. This is the x column as a value between 0 and 100
                            #' @param id The dyad id.
                            #' @return Character string of the dyad as percent column name
                            get_dyad_aspercent_x_column_name = function(id) {
                              return(paste0(id, "XR"))
                            },
                            #' @description
                            #' Get dyad compositional anchor column names by dyad (i.e. left and right).
                            #' @param id The dyad id.
                            #' @param delist Default FALSE. If FALSE return as unnamed vector otherwise as a named list ("left", "right").
                            #' @return An unamed vector or named list of the dyad compositional anchor column names
                            get_dyad_compositional_column_names = function(id, delist = TRUE) {
                              if (self$get_signifier_type_by_id(id) != "dyad") {return(NULL)}
                              if (delist) {
                                return(c(self$get_dyad_left_column_name(id), self$get_dyad_right_column_name(id)))
                              } else {
                                return(list(left = self$get_dyad_left_column_name(id), right = self$get_dyad_right_column_name(id)))
                              }
                            },
                            #' @description
                            #' Get dyad  anchor column names by dyad (i.e. left and right and N/A if applicable).
                            #' @param id The dyad id.
                            #' @param delist Default FALSE. If FALSE return as unnamed vector otherwise as a named list ("left", "right").
                            #' @return An unamed vector or named list of the dyad compositional anchor column names
                            # anchor column names, but this one can have the NA
                            get_dyad_anchor_column_names = function(id, delist = TRUE) {
                              cols <- c(left = self$get_dyad_left_column_name(id), right = self$get_dyad_right_column_name(id))
                              if (self$get_signifier_allow_na(id)) {
                                cols <- c(cols, na = self$get_dyad_na_column_name(id))
                              }
                              if (delist) {
                                return(unname(cols))
                              } else {
                                return(as.list(cols))
                              }
                            },
                            #' @description
                            #' Get all the dyad  anchor column names by dyad (i.e. x, left and right and N/A if applicable).
                            #' @param id The dyad id.
                            #' @param delist Default FALSE. If FALSE return as unnamed vector otherwise as a named list ("left", "right").
                            #' @return An unamed vector or named list of all the dyad  anchor column names
                            get_dyad_all_column_names = function(id, delist = TRUE) {
                              cols <- c(x = self$get_dyad_x_column_name(id), left = self$get_dyad_left_column_name(id), right = self$get_dyad_right_column_name(id))
                              if (self$get_signifier_allow_na(id)) {
                                cols <- c(cols, na = self$get_dyad_na_column_name(id))
                              }
                              if (delist) {
                                return(unname(cols))
                              } else {
                                return(as.list(cols))
                              }
                            },
                            #' @description
                            #' update dyad label left value
                            #' @param id The signifier id.
                            #' @param value The new value.
                            #' @param property the property to update. Should be valid for the signifier type. Optional, if blank, title used.
                            #' @return invisible self
                            update_dyad_left_label_value = function(id, value, property = "") {
                              if (property == "") {property <- "text"}
                              self$signifier_definitions[["dyad"]][[id]][["content"]][["labels"]][["left_anchor"]][[property]] <- value
                              invisible(self)
                            },
                            #' @description
                            #' update dyad label right value
                            #' @param id The signifier id.
                            #' @param value The new value.
                            #' @param property the property to update. Should be valid for the signifier type. Optional, if blank, title used.
                            #' @return invisible self
                            update_dyad_right_label_value = function(id, value, property = "") {
                              if (property == "") {property <- "text"}
                              self$signifier_definitions[["dyad"]][[id]][["content"]][["labels"]][["right_anchor"]][[property]] <- value
                              invisible(self)
                            },
                            #' @description
                            #' Export dyad and dyad anchor titles
                            #' @param ids List of dyad ids to export. Default blank, all dyads 
                            #' @param actual_export or return data frame. Default FALSE - return data frame. 
                            #' @param name_prefix prefix to put on the csv file name if actual_export TRUE. 
                            #' @return Invisible self if actual export otherwise a data frame of the dyad anchor ids and title. 
                            export_dyad_titles = function(ids = "", actual_export = FALSE, name_prefix = "") {
                              if (all((ids == "") == TRUE)) {
                                dyad_ids <- self$get_dyad_ids()
                              }
                              ret_list <- vector("list", length = length(dyad_ids))
                              names(ret_list) <- dyad_ids
                              ret_calc_list <- purrr::imap_dfr(ret_list, private$build_dyad_export)
                              if (actual_export) {
                                write.csv(x = ret_calc_list, file = paste0(ifelse(trimws(name_prefix) == "", "", paste0(name_prefix, "_")), "DyadAnchorExport_", self$get_parent_framework_name(), "_.csv"), row.names = FALSE)
                                return(invisible(self))
                              } else {
                                return(ret_calc_list)
                              }
                            },
                            #' @description
                            #' import dyad and dyad anchor titles from csv file
                            #' @param df The name of the csv file to import or a data frame to import. 
                            #' @return Invisible self if actual export otherwise a data frame of the triad anchor ids and title. 
                            import_dyad_titles = function(df) {
                              if (is.data.frame(df)) {
                                data_df <- df
                              } else {
                                data_df <- read.csv(file = df, check.names = FALSE, stringsAsFactors = FALSE)
                              }
                              my_result <-  private$apply_dyad_conent_update(data_df)
                              return(invisible(self))
                            },
                            
                            #-----------------------------------------------------------------
                            # Stone Helper Functions
                            #-----------------------------------------------------------------
                            #' @description
                            #' Get stones count
                            #' @return An integer of the number of stones occurences
                            get_stones_count = function() {
                              return(self$get_signifier_count_by_type("stones"))
                            },
                            #' @description
                            #' Get stones ids
                            #' @param keep_only_include TRUE or FALSE, if TRUE only those flagged with include == TRUE returned.
                            #' @return A vector of the framework stones ids
                            get_stones_ids = function(keep_only_include = FALSE) {
                              return(self$get_signifier_ids_by_type("stones", keep_only_include))
                            },
                            #' @description
                            #' Get stones background image url.
                            #' @param id The stones id.
                            #' @return Character string of background image URL
                            get_stones_background_image = function(id) {
                              return(self$get_signifier_content_R6(id)$background_image)
                            },
                            #' @description
                            #' Get stones axes as a list of R6 class instance objects.
                            #' @param id The stones id.
                            #' @return List of stones axis R6 class instance objects.
                            get_stones_axis_R6 = function(id) {
                              return(self$get_signifier_content_R6(id)$axis)
                            },
                            #' @description
                            #' Get stones axis as a R6 class instance object.
                            #' @param id The stones id.
                            #' @param axis The stone axis to return ("x" or "y")
                            #' @return An R6 class instance object of the returned axis.
                            get_stones_axis_by_axis_R6 = function(id, axis) {
                              return(self$get_stones_axis_R6(id)[[axis]])
                            },
                            #' @description
                            #' Get stones x axis as a R6 class instance object.
                            #' @param id The stones id.
                            #' @return An R6 class instance object of the x axis.
                            get_stones_x_axis_R6 = function(id) {
                              return(self$get_stones_axis_by_axis_R6(id, "x"))
                            },
                            #' @description
                            #' Get stones y axis as a R6 class instance object.
                            #' @param id The stones id.
                            #' @return An R6 class instance object of the y axis.
                            get_stones_y_axis_R6 = function(id) {
                              return(self$get_stones_axis_by_axis_R6(id, "y"))
                            },
                            #' @description
                            #' Get stones axis name by axis.
                            #' @param id The stones id.
                            #' @param axis The stone axis ("x" or "y")
                            #' @return Character string of returned axis name.
                            get_stones_axis_name_by_axis = function(id, axis) {
                              return(self$get_stones_axis_by_axis_R6(id, axis)$name)
                            },
                            #' @description
                            #' Get stones axis end label by axis.
                            #' @param id The stones id.
                            #' @param axis The stone axis ("x" or "y")
                            #' @return Character string of returned axis end label
                            get_stones_axis_end_label_by_axis = function(id, axis) {
                              return(self$get_stones_axis_by_axis_R6(id, axis)$end_label)
                            },
                            #' @description
                            #' Get stones axis start label by axis.
                            #' @param id The stones id.
                            #' @param axis The stone axis ("x" or "y")
                            #' @return Character string of returned axis start label
                            get_stones_axis_start_label_by_axis = function(id, axis) {
                              return(self$get_stones_axis_by_axis_R6(id, axis)$start_label)
                            },
                            #' @description
                            #' Get stones x axis name.
                            #' @param id The stones id.
                            #' @return Character string of returned x axis name
                            get_stones_xaxis_name = function(id) {
                              return(self$get_stones_axis_name_by_axis(id, "x"))
                            },
                            #' @description
                            #' Get stones x axis end label
                            #' @param id The stones id.
                            #' @return Character string of returned x axis end label
                            get_stones_xaxis_end_label = function(id) {
                              return(self$get_stones_axis_end_label_by_axis(id, "x"))
                            },
                            #' @description
                            #' Get stones x axis start label
                            #' @param id The stones id.
                            #' @return Character string of returned x axis start label
                            get_stones_xaxis_start_label = function(id) {
                              return(self$get_stones_axis_start_label_by_axis(id, "x"))
                            },
                            #' @description
                            #' Get stones y axis name.
                            #' @param id The stones id.
                            #' @return Character string of returned y axis name
                            get_stones_yaxis_name = function(id) {
                              return(self$get_stones_axis_name_by_axis(id, "y"))
                            },
                            #' @description
                            #' Get stones y axis end label
                            #' @param id The stones id.
                            #' @return Character string of returned y axis end label
                            get_stones_yaxis_end_label = function(id) {
                              return(self$get_stones_axis_end_label_by_axis(id, "y"))
                            },
                            #' @description
                            #' Get stones y axis start label
                            #' @param id The stones id.
                            #' @return Character string of returned y axis start label
                            get_stones_yaxis_start_label = function(id) {
                              return(self$get_stones_axis_start_label_by_axis(id, "y"))
                            },
                            #' @description
                            #' Get stones stones as a list of R6 class instances.
                            #' @param id The stones id.
                            #' @return List of stones stones as R6 class instances.
                            get_stones_stones_R6 = function(id) {
                              return(self$get_signifier_content_R6(id)$stones)
                            },
                            #' @description
                            #' Get stones stones ids.
                            #' @param id The stones id.
                            #' @return vector of stones stones ids.
                            get_stones_items_ids = function(id) {
                              return(names(self$get_stones_stones_R6(id)))
                            },
                            #' @description
                            #' Get all property values for a stones stones.
                            #' @param id The stones id.
                            #' @param property The property to return.
                            #' @param delist Default FALSE, reuturn list with stone ids as names, otherwise property values as vector
                            #' @return vector of stones stones property values or list of values with ids as names
                            get_stones_items_property = function(id, property, delist = FALSE) {
                              ret_ids <- names(self$get_stones_stones_R6(id))
                              ret_data <- vector("list", length = length(ret_ids))
                              ret_values <- purrr::imap(ret_data, private$get_R6_property, "title", id)
                              if (delist) {
                                return(unname(unlist(ret_values)))
                              } else {
                                names(ret_values) <- ret_ids
                                return(ret_values)
                              }
                            },
                            #' @description
                            #' Get all title values for a stones stones.
                            #' @param stones_id The stones id.
                            #' @param stone_id The id of the individual stone. Default NULL to include all individual stones. 
                            #' @param delist Default FALSE, reuturn list with stone ids as names, otherwise property values as vector
                            #' @return vector of stones stones title values or list of values with ids as names
                            get_stones_items_title = function(stones_id, stone_id = NULL, delist = FALSE) {
                              if (is.null(stone_id)) {
                                return(self$get_stones_items_property(stones_id, "title", delist))
                              } else {
                                ret_list <- self$get_stones_items_property(stones_id, "title", FALSE)
                                if (!delist) {
                                  return(ret_list[stone_id])
                                } else {
                                  return(ret_list[[stone_id]])
                                }
                              }
                            },
                            #' @description
                            #' Get stones stone by id
                            #' @param sig_id The stones id.
                            #' @param stone_id The stones stone id
                            #' @return A stones stone as an R6 class instance.
                            get_stones_stone_by_id_R6 = function(sig_id, stone_id) {
                              return(self$get_stones_stones_R6(sig_id)[[stone_id]])
                            },
                            #' @description
                            #' Get stones stone title by id
                            #' @param sig_id The stones id.
                            #' @param stone_id The stones stone id
                            #' @return Character string of the stone title.
                            get_stones_stone_title_by_id = function(sig_id, stone_id) {
                              return(self$get_stones_stone_by_id_R6(sig_id, stone_id)$title)
                            },
                            #' @description
                            #' Get stones stone tooltip by id
                            #' @param sig_id The stones id.
                            #' @param stone_id The stones stone id
                            #' @return Character string of the stone tooltip
                            get_stones_stone_tooltip_by_id = function(sig_id, stone_id) {
                              return(self$get_stones_stone_by_id_R6(sig_id, stone_id)$tooltip)
                            },
                            #' @description
                            #' Get stones stone image URL by id
                            #' @param sig_id The stones id.
                            #' @param stone_id The stones stone id
                            #' @return Character string of the stone image URL
                            get_stones_stone_image_by_id = function(sig_id, stone_id) {
                              return(self$get_stones_stone_by_id_R6(sig_id, stone_id)$image)
                            },
                            #' @description
                            #' Get stones stone visible by id
                            #' @param sig_id The stones id.
                            #' @param stone_id The stones stone id
                            #' @return Boolean of the stone Visible
                            get_stones_stone_visible_by_id = function(sig_id, stone_id) {
                              return(self$get_stones_stone_by_id_R6(sig_id, stone_id)$visible)
                            },
                            #' @description
                            #' Get stones stones data column names by id
                            #' @param id The stones id.
                            #' @return Vector of column names for the stones stones
                            get_stones_compositional_column_names = function(id) {
                              return(unlist(purrr::imap(self$get_stones_items_ids(id), private$append_stone_columns, id), recursive = TRUE))
                              
                            },
                            #' @description
                            #' Get stones stone data column names by id
                            #' @param sig_id The stones id.
                            #' @param stone_id The stones stone id
                            #' @return Vector of column names for the stones stones
                            get_stones_stone_compositional_column_names = function(sig_id, stone_id) {
                              return(private$append_stone_columns(sig_id, 1, stone_id) )
                            },
                            #' @description
                            #' Update stone axis property
                            #' @param id The stones id.
                            #' @param axis The stones axis ("x" or "y")
                            #' @param property The property to update (values "end_label, "start_label", "name")
                            #' @param value the updated value
                            #' @return invisible self
                            Update_stones_axis_property = function(id, axis, property, value) {
                              self$signifier_definitions[["stones"]][[id]][["content"]][["axis"]][[axis]][[property]] <- value
                            },
                            #' @description
                            #' Update stone x axis property
                            #' @param id The stones id.
                            #' @param property The property to update (values "end_label, "start_label", "name")
                            #' @param value the updated value
                            #' @return invisible self
                            Update_stones_x_axis_property = function(id,  property, value) {
                              self$signifier_definitions[["stones"]][[id]][["content"]][["axis"]][["x"]][[property]] <- value
                            },
                            #' @description
                            #' Update stone y axis property
                            #' @param id The stones id.
                            #' @param property The property to update (values "end_label, "start_label", "name")
                            #' @param value the updated value
                            #' @return invisible self
                            Update_stones_y_axis_property = function(id,  property, value) {
                              self$signifier_definitions[["stones"]][[id]][["content"]][["axis"]][["y"]][[property]] <- value
                            },
                            #' @description
                            #' Update stone axis property
                            #' @param sig_id The stones signifier id.
                            #' @param stone_id The id of the stone
                            #' @param property The property to update (values "end_label, "start_label", "name")
                            #' @param value the updated value
                            #' @return invisible self
                            Update_stones_stone_property = function(sig_id, stone_id, property, value) {
                              self$signifier_definitions[["stones"]][[sig_id]][["content"]][["stones"]][[stone_id]][[property]] <- value
                            },
                            #' @description
                            #' Export stone titles
                            #' @param ids List of stone ids to export. Default blank, all freetexts 
                            #' @param actual_export or return data frame. Default FALSE - return data frame. 
                            #' @param name_prefix prefix to put on the csv file name if actual_export TRUE. 
                            #' @return Invisible self if actual export otherwise a data frame of the freetext ids and titles. 
                            export_stones_titles = function(ids = "", actual_export = FALSE, name_prefix = "") {
                              if (all((ids == "") == TRUE)) {
                                stones_ids <- self$get_stones_ids()
                              }
                              ret_list <- vector("list", length = length(stones_ids))
                              names(ret_list) <- stones_ids
                              ret_calc_list <- purrr::imap_dfr(ret_list, private$build_stones_export)
                              if (actual_export) {
                                write.csv(x = ret_calc_list, file = paste0(ifelse(trimws(name_prefix) == "", "", paste0(name_prefix, "_")), "StonesTitleExport_", self$get_parent_framework_name(), "_.csv"), row.names = FALSE)
                                return(invisible(self))
                              } else {
                                return(ret_calc_list)
                              }
                            },
                            #' @description
                            #' import stones titles from csv file
                            #' @param df data frame or csv file name of the stones data to import. 
                            #' @return Invisible self if actual export otherwise a data frame of the stones ids and title. 
                            import_stones_titles = function(df) {
                              if (is.data.frame(df)) {
                                data_df <- df
                              } else {
                                data_df <- read.csv(file = df, check.names = FALSE, stringsAsFactors = FALSE)
                              }
                              my_result <-  private$apply_stones_conent_update(data_df)
                              return(invisible(self))
                            },
                            #-----------------------------------------------------------------
                            # Freetext Helper Functions
                            #-----------------------------------------------------------------
                            #' @description
                            #' Get freetext count
                            #' @return An integer of the number of freetext occurences
                            get_freetext_count = function() {
                              return(self$get_signifier_count_by_type("freetext"))
                            },
                            #' @description
                            #' Get freetext ids
                            #' @param keep_only_include TRUE or FALSE, if TRUE only those flagged with include == TRUE returned.
                            #' @return A vector of the framework freetext ids
                            get_freetext_ids = function(keep_only_include = FALSE) {
                              return(self$get_signifier_ids_by_type("freetext", keep_only_include))
                            },
                            #' @description
                            #' Get freetext fragments - those ids that are fragments. 
                            #' @return A vector of signifier ids for fragment free text signifier definitions
                            get_freetext_fragments = function() {
                              return(purrr::keep(self$get_freetext_ids(), ~ self$get_signifier_fragment(.x) == TRUE))
                            },
                            #' @description
                            #' Get freetext default value.
                            #' @param id The freetext id.
                            #' @return A character strong of the default value
                            get_freetext_default = function(id) {
                              return(self$get_signifier_content_R6(id)$default)
                            },
                            #' @description
                            #' Whether the freetext allows multi-line input.
                            #' @param id The freetext id.
                            #' @return Boolean specifying whether the freetext is multiline input
                            get_freetext_multiline = function(id) {
                              return(self$get_signifier_content_R6(id)$multiline)
                            },
                            #' @description
                            #' Export freetext titles
                            #' @param ids List of freetext ids to export. Default blank, all freetexts 
                            #' @param actual_export or return data frame. Default FALSE - return data frame. 
                            #' @param name_prefix prefix to put on the csv file name if actual_export TRUE. 
                            #' @return Invisible self if actual export otherwise a data frame of the freetext ids and titles. 
                            export_freetext_titles = function(ids = "", actual_export = FALSE, name_prefix = "") {
                              if (all((ids == "") == TRUE)) {
                                freetext_ids <- self$get_freetext_ids()
                              }
                              ret_list <- vector("list", length = length(freetext_ids))
                              names(ret_list) <- freetext_ids
                              ret_calc_list <- purrr::imap_dfr(ret_list, private$build_freetext_export)
                              if (actual_export) {
                                write.csv(x = ret_calc_list, file = paste0(ifelse(trimws(name_prefix) == "", "", paste0(name_prefix, "_")), "FreeTextTitleExport_", self$get_parent_framework_name(), "_.csv"), row.names = FALSE)
                                return(invisible(self))
                              } else {
                                return(ret_calc_list)
                              }
                            },
                            #' @description
                            #' import freetext titles from csv file
                            #' @param df data frame or csv file name of the freetext data to import. 
                            #' @return Invisible self if actual export otherwise a data frame of the freetext ids and title. 
                            import_freetext_titles = function(df) {
                              if (is.data.frame(df)) {
                                data_df <- df
                              } else {
                                data_df <- read.csv(file = df, check.names = FALSE, stringsAsFactors = FALSE)
                              }
                              my_result <-  private$apply_freetext_conent_update(data_df)
                              return(invisible(self))
                            },
                            #-----------------------------------------------------------------
                            # imageselect Helper Functions
                            #-----------------------------------------------------------------
                            #' @description
                            #' Get imageselect count
                            #' @return An integer of the number of imageselect occurences
                            get_imageselect_count = function() {
                              return(self$get_signifier_count_by_type("imageselect"))
                            },
                            #' @description
                            #' Get imageselect ids
                            #' @param keep_only_include TRUE or FALSE, if TRUE only those flagged with include == TRUE returned.
                            #' @return A vector of the framework imageselect ids
                            get_imageselect_ids = function(keep_only_include = FALSE) {
                              return(self$get_signifier_ids_by_type("imageselect", keep_only_include))
                            },
                            #' @description
                            #' Get the imageselect items.
                            #' @param id The imageselect id.
                            #' @return List of URLs to the image select item image.
                            get_imageselect_items = function(id) {
                              return(unname(unlist(purrr::map(self$get_signifier_content_R6(id)$items, ~{.x$default}))))
                            },
                            #-----------------------------------------------------------------
                            # photo Helper Functions
                            #-----------------------------------------------------------------
                            #' @description
                            #' Get photo count
                            #' @return An integer of the number of photo occurences
                            get_photo_count = function() {
                              return(self$get_signifier_count_by_type("photo"))
                            },
                            #' @description
                            #' Get photo ids
                            #' @param keep_only_include TRUE or FALSE, if TRUE only those flagged with include == TRUE returned.
                            #' @return A vector of the framework photo ids
                            get_photo_ids = function(keep_only_include = FALSE) {
                              return(self$get_signifier_ids_by_type("photo", keep_only_include))
                            },
                            #-----------------------------------------------------------------
                            # audio Helper Functions
                            #-----------------------------------------------------------------
                            #' @description
                            #' Get audio count
                            #' @return An integer of the number of audio occurences
                            get_audio_count = function() {
                              return(self$get_signifier_count_by_type("audio"))
                            },
                            #' @description
                            #' Get audio ids
                            #' @param keep_only_include TRUE or FALSE, if TRUE only those flagged with include == TRUE returned.
                            #' @return A vector of the framework audio ids
                            get_audio_ids = function(keep_only_include = FALSE) {
                              return(self$get_signifier_ids_by_type("audio", keep_only_include))
                            },
                            #-----------------------------------------------------------------
                            # uniqueid Helper Functions
                            #-----------------------------------------------------------------
                            #' @description
                            #' Get uniqueid count
                            #' @return An integer of the number of uniqueid occurences (should only return 1)
                            get_uniqueid_count = function() {
                              return(self$get_signifier_count_by_type("list"))
                            },
                            #' @description
                            #' Get uniqueid ids
                            #' @param keep_only_include TRUE or FALSE, if TRUE only those flagged with include == TRUE returned.
                            #' @return A vector of the framework uniqueid ids
                            get_uniqueid_ids = function(keep_only_include = FALSE) {
                              return(self$get_signifier_ids_by_type("uniqueid", keep_only_include))
                            },
                            #-----------------------------------------------------------------
                            # polymorphic signifier Helper Functions
                            #-----------------------------------------------------------------
                            #' @description
                            #' Get signifier ids of defined polymorphic signifiers
                            #' @return Vector of signifier ids
                            get_poly_sig_ids = function() {
                              names(fw$polymorphic_definitions)
                            },
                            #' @description
                            #' Get signifier type for polymorphic signifier id
                            #' @param id Polymorphic signifier id.
                            #' @return Signifier type of the id
                            get_poly_type_by_id = function(id) {
                              return(self$polymorphic_type_by_ids[[id]])
                            },
                            #' @description
                            #' Get the ids of the polymorphic to signifiers for a given polymorphic id
                            #' @param id Polymorphic signifier id.
                            #' @return ids of those signifiers polymorphic to the one passed
                            get_poly_sig_to_ids = function(id) {
                              return(self$polymorphic_definitions[[id]][["polymorphic_to"]][, "id"])
                            },
                            #' @description
                            #' Get the full polymorphic to information for a given polymorphic signifier id and polymorphic to id
                            #' @param sig_id Polymorphic signifier id.
                            #' @param poly_id Polymorphic to signifier id.
                            #' @return dataframe row of polymorphic to information record
                            get_poly_sig_to_by_id = function(sig_id, poly_id) {
                              return(self$polymorphic_definitions[[sig_id]][["polymorphic_to"]] %>% dplyr::filter(id == poly_id))
                            },
                            #' @description
                            #' Get the full polymorphic to data column names for a given polymorphic signifier id and polymorphic to id
                            #' @param sig_id Polymorphic signifier id.
                            #' @param poly_id Polymorphic to signifier id.
                            #' @return Vector of the data column names for the polymorphic id and polymorphic to id 
                            get_poly_sig_to_cols_by_id = function(sig_id, poly_id) {
                              temp_data <- self$polymorphic_definitions[[sig_id]][["polymorphic_to"]] %>% dplyr::filter(id == poly_id)
                              print(temp_data)
                              if (self$get_poly_type_by_id(sig_id) == "triad") {
                                return(paste0(temp_data[1, "id"], "_", temp_data[1, c("top", "left", "right")]))
                              } else {
                                return(paste0(temp_data[1, "id"], "_", temp_data[1, c("left", "right")]))
                              }
                            },
                            #' @description
                            #' Get the top polymorphic id for signifier id and polymorphic to id
                            #' @param sig_id Polymorphic signifier id.
                            #' @param poly_id Polymorphic to signifier id.
                            #' @return The polymorphic to top id 
                            get_poly_sig_to_top_by_id = function(sig_id, poly_id) {
                              return((self$polymorphic_definitions[[sig_id]][["polymorphic_to"]] %>% dplyr::filter(id == poly_id))[1, "top"])
                            },
                            #' @description
                            #' Get the left polymorphic id for signifier id and polymorphic to id
                            #' @param sig_id Polymorphic signifier id.
                            #' @param poly_id Polymorphic to signifier id.
                            #' @return The polymorphic to left id
                            get_poly_sig_to_left_by_id = function(sig_id, poly_id) {
                              return((self$polymorphic_definitions[[sig_id]][["polymorphic_to"]] %>% dplyr::filter(id == poly_id))[1, "left"])
                            },
                            #' @description
                            #' Get the right polymorphic id for signifier id and polymorphic to id
                            #' @param sig_id Polymorphic signifier id.
                            #' @param poly_id Polymorphic to signifier id.
                            #' @return The polymorphic to right id
                            get_poly_sig_to_right_by_id = function(sig_id, poly_id) {
                              return((self$polymorphic_definitions[[sig_id]][["polymorphic_to"]] %>% dplyr::filter(id == poly_id))[1, "right"])
                            },
                            #' @description
                            #' Get the anchor polymorphic id for signifier id and polymorphic to id
                            #' @param sig_id Polymorphic signifier id.
                            #' @param poly_id Polymorphic to signifier id.
                            #' @param anchor "left", "top", "right" anchor to return (defaults to "left").
                            #' @return The polymorphic to top id
                            get_poly_sig_to_anchort_by_id = function(sig_id, poly_id, anchor = "left") {
                              return((self$polymorphic_definitions[[sig_id]][["polymorphic_to"]] %>% dplyr::filter(id == poly_id))[1, anchor])
                            },
                            #=================================================================
                            # Creation methods
                            #=================================================================
                            #' @description
                            #' Add a freetext signifier definition to the parent definition
                            #' @param title - the freetext signifier title
                            #' @param tooltip - the freetext signifier tooltip
                            #' @param allow_na - whether the freetext signifier allows N/A
                            #' @param fragment - whether the freetext signifier is a fragment entry
                            #' @param required - whether the freetext signifier is mandatory
                            #' @param sticky - whether the freetext signifier is a sticky
                            #' @param multiline - whether the freetext signifier entry is multi-line
                            #' @param include - whether the freetext signifier is included in the capture
                            #' @param default - the freetext signifier default value
                            #' @param theader -  a 3 elment named list with "name", "id" and "language" as list names. NULL will take the parent framework to add
                            #' @param id - the freetext signifier id - if blank or NULL, id is calculated automatically
                            #' @param load Whether the added signifier is the initial load or a subsequent load (adding a new signifier after the initial load from the json)
                            #' @return self
                            add_freetext = function(title, tooltip, allow_na, fragment, required, sticky, multiline, include = TRUE, default = "", theader = NULL, id = "", load = "subsequent") {
                              # get the signifier definition entry being processed
                              if (id == "") {
                                id <- uuid::UUIDgenerate(use.time = FALSE, n = 1)
                              }
                       
                              if (is.null(theader)) {
                                theader <- list(name = self$get_parent_name(), id = self$get_parent_id(), language = self$get_parent_language())
                              } else {
                                if (theader[["id"]] == self$get_parent_id()) {
                                  if (theader[["name"]] == "") {
                                    theader[["name"]] == self$get_parent_name()
                                  }
                                  if (theader[["language"]] == "") {
                                    theader[["language"]] == self$get_parent_language()
                                  }
                                } else {
                                  if (theader[["name"]] == "") {
                                    theader[["name"]] == self$get_linked_framework_names(fw_id = theader[["id"]])
                                  }
                                }
                              }
                              
                              # Add the signifier definition
                              content <- private$freetext_content_definition_R6()$new(default = default, multiline = multiline)
                              definition <- private$signifier_definition_R6()$new(id = id, type = "freetext", title = title, tooltip = tooltip, allow_na = allow_na,
                                                                                  fragment = fragment, required = required, sticky = sticky, content = content, include = TRUE)
                              add_list <- list(definition)
                              names(add_list) <- id
                              
                              self$signifier_definitions[["freetext"]] <- append(self$signifier_definitions[["freetext"]], add_list)
                              # Ripple update to the other fields
                              private$ripple_update(id, "freetext", theader[["id"]], load)
                              return(id)
                            },
                            #' @description
                            #' Add a image select signifier definition to the parent definition
                            #' @param title - the imageselect signifier title
                            #' @param tooltip - the imageselect signifier tooltip
                            #' @param allow_na - whether the imageselect signifier allows N/A
                            #' @param fragment - whether the imageselect signifier is a fragment entry
                            #' @param required - whether the imageselect signifier is mandatory
                            #' @param sticky - whether the imageselect signifier is a sticky
                            #' @param items - a vector of imageselect items  (path/file) to add (or data frame with imageselect path/file)
                            #' @param theader -  a 3 elment named list with "name", "id" and "language" as list names. NULL will take the parent framework to add
                            #' @param id - the freetext signifier id - if blank or NULL, id is calculated automatically
                            #' @param load Whether the added signifier is the initial load or a subsequent load (adding a new signifier after the initial load from the json)
                            #' @return self
                            add_imageselect = function(title, tooltip, allow_na, fragment, required, sticky, items, theader = NULL, id = "", load = "subsequent") {
                              # get the signifier definition entry being processed
                              if (id == "") {
                                id <- uuid::UUIDgenerate(use.time = FALSE, n = 1)
                              }
                              if (is.null(theader)) {
                                theader <- list(name = self$get_parent_name(), id = self$get_parent_id(), language = self$get_parent_language())
                              } else {
                                if (theader[["id"]] == self$get_parent_id()) {
                                  if (theader[["name"]] == "") {
                                    theader[["name"]] == self$get_parent_name()
                                  }
                                  if (theader[["language"]] == "") {
                                    theader[["language"]] == self$get_parent_language()
                                  }
                                } else {
                                  if (theader[["name"]] == "") {
                                    theader[["name"]] == self$get_linked_framework_names(fw_id = theader[["id"]])
                                  }
                                }
                              }
                              # Add the signifier definition
                              if (!is.data.frame(items)) {
                                items <- data.frame(default = items)
                                #names(items) <- "default"
                              } else {
                                names(items) <- "default"
                              }
                              result_item <- vector("list", length = nrow(items))
                              names(result_item) <- 1:length(result_item)
                              list_items <-  purrr::imap(result_item, private$build_imageselect_item, items)
                              content <- private$imageselect_content_definition_R6()$new(items = list_items, num_items = length(list_items))
                              definition <- private$signifier_definition_R6()$new(id = id, type = "imageselect", title = title, tooltip = tooltip, allow_na = allow_na,
                                                                                  fragment = fragment, required = required, sticky = sticky, content = content, include = TRUE)
                              add_list <- list(definition)
                              names(add_list) <- id
                              self$signifier_definitions[["imageselect"]] <- append(self$signifier_definitions[["imageselect"]], add_list)
                              # Ripple update to the other fields
                              private$ripple_update(id, "imageselect", theader[["id"]], load)
                              return(id)
                            },
                            #' @description
                            #' Add an audio signifier definition to the parent definition
                            #' @param title - the audio signifier title
                            #' @param tooltip - the audio signifier tooltip
                            #' @param allow_na - whether the audio signifier allows N/A
                            #' @param fragment - whether the audio signifier is a fragment entry
                            #' @param required - whether the audio signifier is mandatory
                            #' @param sticky - whether the audio signifier is a sticky#' 
                            #' @param theader -  a 3 elment named list with "name", "id" and "language" as list names. NULL will take the parent framework to add
                            #' @param id - the freetext signifier id - if blank or NULL, id is calculated automatically
                            #' @param load Whether the added signifier is the initial load or a subsequent load (adding a new signifier after the initial load from the json)
                            #' @return self
                            add_audio = function(title, tooltip, allow_na, fragment, required, sticky, theader = NULL, id = "", load = "subsequent") {
                              # get the signifier definition entry being processed
                              if (id == "") {
                                id <- uuid::UUIDgenerate(use.time = FALSE, n = 1)
                              }
                              if (is.null(theader)) {
                                theader <- list(name = self$get_parent_name(), id = self$get_parent_id(), language = self$get_parent_language())
                              } else {
                                if (theader[["id"]] == self$get_parent_id()) {
                                  if (theader[["name"]] == "") {
                                    theader[["name"]] == self$get_parent_name()
                                  }
                                  if (theader[["language"]] == "") {
                                    theader[["language"]] == self$get_parent_language()
                                  }
                                } else {
                                  if (theader[["name"]] == "") {
                                    theader[["name"]] == self$get_linked_framework_names(fw_id = theader[["id"]])
                                  }
                                }
                              }
                              definition <- private$signifier_definition_R6()$new(id = id, type = "audio", title = title, tooltip = tooltip, allow_na = allow_na,
                                                                                  fragment = fragment, required = required, sticky = sticky, content = NULL, include = TRUE)
                              add_list <- list(definition)
                              names(add_list) <- id
                              self$signifier_definitions[["audio"]] <- append(self$signifier_definitions[["audio"]], add_list)
                              # Ripple update to the other fields
                              private$ripple_update(id, "audio", theader[["id"]], load)
                              return(id)
                            },
                            #' @description
                            #' Add an photo signifier definition to the parent definition
                            #' @param title - the photo signifier title
                            #' @param tooltip - the photo signifier tooltip
                            #' @param allow_na - whether the photo signifier allows N/A
                            #' @param fragment - whether the photo signifier is a fragment entry
                            #' @param required - whether the photo signifier is mandatory
                            #' @param sticky - whether the photo signifier is a sticky
                            #' @param theader -  a 3 elment named list with "name", "id" and "language" as list names. NULL will take the parent framework to add
                            #' @param id - the freetext signifier id - if blank or NULL, id is calculated automatically
                            #' @param load Whether the added signifier is the initial load or a subsequent load (adding a new signifier after the initial load from the json)
                            #' @return self
                            add_photo = function(title, tooltip, allow_na, fragment, required, sticky, theader = NULL, id = "", load = "subsequent") {
                              # get the signifier definition entry being processed
                              if (id == "") {
                                id <- uuid::UUIDgenerate(use.time = FALSE, n = 1)
                              }
                              if (is.null(theader)) {
                                theader <- list(name = self$get_parent_name(), id = self$get_parent_id(), language = self$get_parent_language())
                              } else {
                                if (theader[["id"]] == self$get_parent_id()) {
                                  if (theader[["name"]] == "") {
                                    theader[["name"]] == self$get_parent_name()
                                  }
                                  if (theader[["language"]] == "") {
                                    theader[["language"]] == self$get_parent_language()
                                  }
                                } else {
                                  if (theader[["name"]] == "") {
                                    theader[["name"]] == self$get_linked_framework_names(fw_id = theader[["id"]])
                                  }
                                }
                              }
                              definition <- private$signifier_definition_R6()$new(id = id, type = "photo", title = title, tooltip = tooltip, allow_na = allow_na,
                                                                                  fragment = fragment, required = required, sticky = sticky, content = NULL, include = TRUE)
                              add_list <- list(definition)
                              names(add_list) <- id
                              self$signifier_definitions[["photo"]] <- append(self$signifier_definitions[["photo"]], add_list)
                              # Ripple update to the other fields
                              private$ripple_update(id, "photo", theader[["id"]], load)
                              return(id)
                            },
                            #' @description
                            #' Add an list signifier definition to the parent definition
                            #' @param title - the list signifier title
                            #' @param tooltip - the list signifier tooltip
                            #' @param allow_na - whether the list signifier allows N/A
                            #' @param fragment - whether the list signifier is a fragment entry
                            #' @param required - whether the list signifier is mandatory
                            #' @param sticky - whether the list signifier is a sticky
                            #' @param items - data frame of the list items with columns id, title, tooltip, visible, other_signifier_id blank ids will be generated.
                            #' @param max_responses - integer of the maximum responses for the list. 
                            #' @param min_responses - inteter of the minimum responses for the list. 
                            #' @param other_item_id - The signifier level other item id. 
                            #' @param other_signifier_id - The signifier ID if there is an "other" text box (not supporting dynamic creation of embedded)
                            #' @param theader -  a 3 elment named list with "name", "id" and "language" as list names. NULL will take the parent framework to add
                            #' @param id - the freetext signifier id - if blank or NULL, id is calculated automatically
                            #' @param load Whether the added signifier is the initial load or a subsequent load (adding a new signifier after the initial load from the json)
                            #' @return self
                            add_list = function(title, tooltip, allow_na, fragment, required, sticky, items, max_responses, min_responses, other_item_id, other_signifier_id, theader = NULL, id = "", load = "subsequent") {
                              
                              # items must be  data frame
                              assertive::assert_is_data.frame(x = items, severity = "stop")
                              # number of columns of items is to be 5
                              assertive::assert_all_are_equal_to(ncol(items), 5,
                                                                 severity = getOption("assertive.severity", "stop"))
                              # column names of items correct
                              assertive::assert_is_identical_to_true(all(c("id", "title", "tooltip", "visible", "other_signifier_id") %in% colnames(items)), severity = "stop")
                              # Any NA column names - assign an id
                              items[["id"]] <- unlist(purrr::map(items[["id"]], function(.x) {ifelse(is.na(.x), uuid::UUIDgenerate(use.time = FALSE, n = 1), .x)}))
                              # get the signifier definition entry being processed
                              if (id == "") {
                                id <- uuid::UUIDgenerate(use.time = FALSE, n = 1)
                              }
                              if (is.null(theader)) {
                                theader <- list(name = self$get_parent_name(), id = self$get_parent_id(), language = self$get_parent_language())
                              } else {
                                if (theader[["id"]] == self$get_parent_id()) {
                                  if (theader[["name"]] == "") {
                                    theader[["name"]] == self$get_parent_name()
                                  }
                                  if (theader[["language"]] == "") {
                                    theader[["language"]] == self$get_parent_language()
                                  }
                                } else {
                                  if (theader[["name"]] == "") {
                                    theader[["name"]] == self$get_linked_framework_names(fw_id = theader[["id"]])
                                  }
                                }
                              }
                              result_item <- vector("list", length = nrow(items))
                              names(result_item) <- items$id
                              list_items <-  purrr::imap(result_item, private$build_list_item , items)
                              content <- private$item_content_definition_R6()$new(items = list_items, num_items = length(list_items), max_responses = max_responses, min_responses = min_responses, other_item_id = other_item_id, other_signifier_id = other_signifier_id)
                              definition <- private$signifier_definition_R6()$new(id = id, type = "list", title = title, tooltip = tooltip, allow_na = allow_na,
                                                                                  fragment = fragment, required = required, sticky = sticky, content = content, include = TRUE)
                              add_list <- list(definition)
                              names(add_list) <- id
                              self$signifier_definitions[["list"]] <- append(self$signifier_definitions[["list"]], add_list)
                              # Ripple update to the other fields
                              private$ripple_update(id, "list", theader[["id"]], load)
                              return(id)
                            },
                            #' @description
                            #' Add an triad signifier definition to the parent definition
                            #' @param title - the triad signifier title
                            #' @param tooltip - the list signifier tooltip
                            #' @param allow_na - whether the list signifier allows N/A
                            #' @param fragment - whether the list signifier is a fragment entry
                            #' @param required - whether the list signifier is mandatory
                            #' @param sticky - whether the list signifier is a sticky
                            #' @param labels - a 3 row dataframe with top, left, right anchor definitions. dataframe columns id, text, image, show_image and show_label
                            #' @param pointer_image - url to the triad pointer image file
                            #' @param background_image - url to the triad background image
                            #' @param theader -  a 3 elment named list with "name", "id" and "language" as list names. NULL will take the parent framework to add
                            #' @param id - the freetext signifier id - if blank or NULL, id is calculated automatically
                            #' @param load Whether the added signifier is the initial load or a subsequent load (adding a new signifier after the initial load from the json)
                            #' @return self
                            add_triad = function(title, tooltip, allow_na, fragment, required, sticky, labels, pointer_image, background_image, theader = NULL, id = "", load = "subsequent") {
                              # labels must be  data frame
                              assertive::assert_is_data.frame(x = labels, severity = "stop")
                              # number of columns of labels is to be 5
                              assertive::assert_all_are_equal_to(ncol(labels), 5,
                                                                 severity = getOption("assertive.severity", "stop"))
                              # number of rows of labels is to be 3
                              assertive::assert_all_are_equal_to(nrow(labels), 3,
                                                                 severity = getOption("assertive.severity", "stop"))
                              # column names of items correct
                              assertive::assert_is_identical_to_true(all(c("id", "text", "image", "show_image", "show_label") %in% colnames(labels)), severity = "stop")
                              # Any NA column names - assign an id
                              labels[["id"]] <- unlist(purrr::map(labels[["id"]], function(.x) {ifelse(is.na(.x), uuid::UUIDgenerate(use.time = FALSE, n = 1), .x)}))
                              # get the signifier definition entry being processed
                              if (id == "") {
                                id <- uuid::UUIDgenerate(use.time = FALSE, n = 1)
                              }
                              if (is.null(theader)) {
                                theader <- list(name = self$get_parent_name(), id = self$get_parent_id(), language = self$get_parent_language())
                              } else {
                                if (theader[["id"]] == self$get_parent_id()) {
                                  if (theader[["name"]] == "") {
                                    theader[["name"]] == self$get_parent_name()
                                  }
                                  if (theader[["language"]] == "") {
                                    theader[["language"]] == self$get_parent_language()
                                  }
                                } else {
                                  if (theader[["name"]] == "") {
                                    theader[["name"]] == self$get_linked_framework_names(fw_id = theader[["id"]])
                                  }
                                }
                              }
                              top_anchor  <-  private$label_definition_R6()$new(id = labels[1,"id"], text =
                                                                                  labels[1,"text"], show_image = labels[1, "show_image"],
                                                                                show_label =  labels[1, "show_label"],
                                                                                image = labels[1, "image"])
                              left_anchor  <-  private$label_definition_R6()$new(id = labels[2,"id"], text =
                                                                                   labels[2,"text"], show_image = labels[2, "show_image"],
                                                                                 show_label =  labels[2, "show_label"],
                                                                                 image = labels[2, "image"])
                              right_anchor  <-  private$label_definition_R6()$new(id = labels[3,"id"], text =
                                                                                    labels[3,"text"], show_image = labels[3, "show_image"],
                                                                                  show_label =  labels[3, "show_label"],
                                                                                  image = labels[3, "image"])
                              labels <- list(top_anchor = top_anchor, left_anchor = left_anchor,
                                             right_anchor = right_anchor)
                              content <- private$slider_content_definition_R6()$new(labels = labels, pointer_image = pointer_image,  background_image = background_image)
                              definition  <- private$signifier_definition_R6()$new(id = id, type = "triad", title = title, tooltip = tooltip, allow_na = allow_na,
                                                                                   fragment = fragment, required = required, sticky = sticky, content = content, include = TRUE)
                              add_list <- list(definition)
                              names(add_list) <- id
                              self$signifier_definitions[["triad"]] <- append(self$signifier_definitions[["triad"]], add_list)
                              # Ripple update to the other fields
                              # id, "triad", link_type, linked_framework_id, load, tcurrent_framework_id
                              private$ripple_update(id, "triad", theader[["id"]], load)
                              return(id)
                            },
                            #' @description
                            #' Add an dyad signifier definition to the parent definition
                            #' @param title - the dyad signifier title
                            #' @param tooltip - the list signifier tooltip
                            #' @param allow_na - whether the list signifier allows N/A
                            #' @param fragment - whether the list signifier is a fragment entry
                            #' @param required - whether the list signifier is mandatory
                            #' @param sticky - whether the list signifier is a sticky
                            #' @param labels - a 3 row dataframe with left, right anchor definitions. dataframe columns id, text, image, show_image and show_label
                            #' @param pointer_image - url to the dyad pointer image file
                            #' @param background_image - url to the dyad background image
                            #' @param theader -  a 3 elment named list with "name", "id" and "language" as list names. NULL will take the parent framework to add
                            #' @param id - the freetext signifier id - if blank or NULL, id is calculated automatically
                            #' @param load Whether the added signifier is the initial load or a subsequent load (adding a new signifier after the initial load from the json)
                            #' @return self
                            add_dyad = function(title, tooltip, allow_na, fragment, required, sticky, labels, pointer_image, background_image, theader = NULL, id = "", load = "subsequent") {
                              # labels must be  data frame
                              assertive::assert_is_data.frame(x = labels, severity = "stop")
                              # number of columns of labels is to be 5
                              assertive::assert_all_are_equal_to(ncol(labels), 5,
                                                                 severity = getOption("assertive.severity", "stop"))
                              # number of rows of labels is to be 2
                              assertive::assert_all_are_equal_to(nrow(labels), 2,
                                                                 severity = getOption("assertive.severity", "stop"))
                              # column names of items correct
                              assertive::assert_is_identical_to_true(all(c("id", "text", "image", "show_image", "show_label") %in% colnames(labels)), severity = "stop")
                              # Any NA column names - assign an id
                              labels[["id"]] <- unlist(purrr::map(labels[["id"]], function(.x) {ifelse(is.na(.x), uuid::UUIDgenerate(use.time = FALSE, n = 1), .x)}))
                              # get the signifier definition entry being processed
                              if (id == "") {
                                id <- uuid::UUIDgenerate(use.time = FALSE, n = 1)
                              }
                              if (is.null(theader)) {
                                theader <- list(name = self$get_parent_name(), id = self$get_parent_id(), language = self$get_parent_language())
                              } else {
                                if (theader[["id"]] == self$get_parent_id()) {
                                  if (theader[["name"]] == "") {
                                    theader[["name"]] == self$get_parent_name()
                                  }
                                  if (theader[["language"]] == "") {
                                    theader[["language"]] == self$get_parent_language()
                                  }
                                } else {
                                  if (theader[["name"]] == "") {
                                    theader[["name"]] == self$get_linked_framework_names(fw_id = theader[["id"]])
                                  }
                                }
                              }
                              left_anchor  <-  private$label_definition_R6()$new(id = labels[1,"id"], text =
                                                                                   labels[1,"text"], show_image = labels[1, "show_image"],
                                                                                 show_label =  labels[1, "show_label"],
                                                                                 image = labels[1, "image"])
                              right_anchor  <-  private$label_definition_R6()$new(id = labels[2,"id"], text =
                                                                                    labels[2,"text"], show_image = labels[2, "show_image"],
                                                                                  show_label =  labels[2, "show_label"],
                                                                                  image = labels[2, "image"])
                              labels <- list(left_anchor = left_anchor,
                                             right_anchor = right_anchor)
                              content <- private$slider_content_definition_R6()$new(labels = labels, pointer_image = pointer_image,  background_image = background_image)
                              definition  <- private$signifier_definition_R6()$new(id = id, type = "dyad", title = title, tooltip = tooltip, allow_na = allow_na,
                                                                                   fragment = fragment, required = required, sticky = sticky, content = content, include = TRUE)
                              add_list <- list(definition)
                              names(add_list) <- id
                              self$signifier_definitions[["dyad"]] <- append(self$signifier_definitions[["dyad"]], add_list)
                              # Ripple update to the other fields
                              private$ripple_update(id, "dyad", theader[["id"]], load)
                              return(id)
                            },
                            #' @description
                            #' Add an stones signifier definition to the parent definition
                            #' @param title - the stones signifier title
                            #' @param tooltip - the stones signifier tooltip
                            #' @param allow_na - whether the stones signifier allows N/A
                            #' @param fragment - whether the stones signifier is a fragment entry
                            #' @param required - whether the stones signifier is mandatory
                            #' @param sticky - whether the stones signifier is a sticky
                            #' @param stones - a dataframe containing the individual stone definitions, with columns id, image, title, tooltip
                            #' @param background_image - url to the dyad background image
                            #' @param x_name - the x label name
                            #' @param x_end_label - the x end label name
                            #' @param x_start_label - the x start label name
                            #' @param y_name - the y label name
                            #' @param y_end_label - the y end label name
                            #' @param y_start_label - the y start label name
                            #' @param theader -  a 3 elment named list with "name", "id" and "language" as list names. NULL will take the parent framework to add
                            #' @param id - the freetext signifier id - if blank or NULL, id is calculated automatically
                            #' @param load Whether the added signifier is the initial load or a subsequent load (adding a new signifier after the initial load from the json)
                            #' @return self
                            add_stones = function(title, tooltip, allow_na, fragment, required, sticky, stones, background_image, x_name, x_end_label, x_start_label, y_name, y_end_label, y_start_label, theader = NULL, id = "", load = "subsequent") {
                              # stones must be  data frame
                              assertive::assert_is_data.frame(x = stones, severity = "stop")
                              # number of columns of stones is to be 4
                              assertive::assert_all_are_equal_to(ncol(stones), 4,
                                                                 severity = getOption("assertive.severity", "stop"))
                              # column names of items correct
                              assertive::assert_is_identical_to_true(all(c("id", "title", "image", "tooltip") %in% colnames(stones)), severity = "stop")
                              # Any NA column names - assign an id
                              stones[["id"]] <- unlist(purrr::map(stones[["id"]], function(.x) {ifelse(is.na(.x), uuid::UUIDgenerate(use.time = FALSE, n = 1), .x)}))
                              # get the signifier definition entry being processed
                              if (id == "") {
                                id <- uuid::UUIDgenerate(use.time = FALSE, n = 1)
                              }
                              if (is.null(theader)) {
                                theader <- list(name = self$get_parent_name(), id = self$get_parent_id(), language = self$get_parent_language())
                              } else {
                                if (theader[["id"]] == self$get_parent_id()) {
                                  if (theader[["name"]] == "") {
                                    theader[["name"]] == self$get_parent_name()
                                  }
                                  if (theader[["language"]] == "") {
                                    theader[["language"]] == self$get_parent_language()
                                  }
                                } else {
                                  if (theader[["name"]] == "") {
                                    theader[["name"]] == self$get_linked_framework_names(fw_id = theader[["id"]])
                                  }
                                }
                              }
                              result_stones <- vector("list", length = nrow(stones))
                              names(result_stones) <- stones$id
                              stone_items <-  purrr::imap(result_stones, private$build_stone_entry  , stones)
                              stone_x_axis <- private$stones_axis_R6()$new(axis = "x", name = x_name, end_label = x_end_label, start_label = x_start_label)
                              stone_y_axis <- private$stones_axis_R6()$new(axis = "y", name = x_name, end_label = y_end_label, start_label = y_start_label)
                              stone_axis <- list(x = stone_x_axis, y = stone_y_axis)
                              content <- private$stone_content_definition_R6()$new(axis = stone_axis, stones = stone_items, num_stones = length(stone_items), background_image = background_image)
                              definition <- private$signifier_definition_R6()$new(id = id, type = "stones", title = title, tooltip = tooltip, allow_na = allow_na,
                                                                                  fragment = fragment, required = required, sticky = sticky, content = content, include = TRUE)
                              add_list <- list(definition)
                              names(add_list) <- id
                              self$signifier_definitions[["stones"]] <- append(self$signifier_definitions[["stones"]], add_list)
                              # Ripple update to the other fields
                              private$ripple_update(id, "stones", theader[["id"]], load)
                              return(id)
                            },
                            #' @description
                            #' Add an uniqueid signifier definition to the parent definition
                            #' @param title - the uniqueid signifier title
                            #' @param tooltip - the uniqueid signifier tooltip
                            #' @param allow_na - whether the uniqueid signifier allows N/A
                            #' @param fragment - whether the uniqueid signifier is a fragment entry
                            #' @param required - whether the uniqueid signifier is mandatory
                            #' @param sticky - whether the uniqueid signifier is a sticky
                            #' @param theader -  a 3 elment named list with "name", "id" and "language" as list names. NULL will take the parent framework to add
                            #' @param id - the freetext signifier id - if blank or NULL, id is calculated automatically
                            #' @param load Whether the added signifier is the initial load or a subsequent load (adding a new signifier after the initial load from the json)
                            #' @return self
                            add_uniqueid = function(title, tooltip, allow_na, fragment, required, sticky, theader = NULL, id = "", load = "subsequent") {
                              # get the signifier definition entry being processed
                              if (id == "") {
                                id <- uuid::UUIDgenerate(use.time = FALSE, n = 1)
                              }
                              if (is.null(theader)) {
                                theader <- list(name = self$get_parent_name(), id = self$get_parent_id(), language = self$get_parent_language())
                              } else {
                                if (theader[["id"]] == self$get_parent_id()) {
                                  if (theader[["name"]] == "") {
                                    theader[["name"]] == self$get_parent_name()
                                  }
                                  if (theader[["language"]] == "") {
                                    theader[["language"]] == self$get_parent_language()
                                  }
                                } else {
                                  if (theader[["name"]] == "") {
                                    theader[["name"]] == self$get_linked_framework_names(fw_id = theader[["id"]])
                                  }
                                }
                              }
                              definition <- private$signifier_definition_R6()$new(id = id, type = "uniqueid", title = title, tooltip = tooltip, allow_na = allow_na,
                                                                                  fragment = fragment, required = required, sticky = sticky, content = NULL, include = TRUE)
                              add_list <- list(definition)
                              names(add_list) <- id
                              self$signifier_definitions[["uniqueid"]] <- append(self$signifier_definitions[["uniqueid"]], add_list)
                              # Ripple update to the other fields
                              private$ripple_update(id, "uniqueid", theader[["id"]], load)
                              return(id)
                            },
                            #' @description
                            #' Apply a polymorphic definition to the framework. Only one of the parameters should be passed. 
                            #' @param poly_data - the read JSON from the polymorphic signifier json file. 
                            #' @param tpoly_data_file - The JSON path/file name for the json file. 
                            add_polymorphic_signifiers =  function(tpoly_data = NULL, tpoly_data_file = NULL) {
                              # todo - check and validate parameters
                              private$apply_poly(tpoly_data, tpoly_data_file)
                              
                            }
                          ),
                          
                          private = list(
                            # This is the function initialization calls.
                            # Unpack the json
                            # Parameters
                            # tself - The self object - only used for returning the list of all signifier types but might be later used for other things.
                            #       -  ToDo - update fields directly in this code returning a TRUE/FALSE on success or otherwise.
                            # tparsedjson - The parsed json file, included if parsed prior to using the sensemakerframeworkr package.
                            # tjsonfile - The json file if it hasn't been previously parsed (will be parsed here)
                            # tworkbenchid - The workbenchID if no json provided - will be retrieved from the platform
                            # ttoken - The token required to retrieve the json from the platform.
                            # Only one of tparsedjson, tjsonfile or (tworkbenchid, ttoken pair) will be passed. tjsonfile takes priority.
                            # Return a single list of lists containing:
                            # 1. R6 class instance of the signifier definitions.
                            # 2. List of signifier IDs by type.
                            # 3. List of type by signifier ID
                            # 4. Vector containing the types used in the passed json.
                            # The json header properties. 
                            json_header_names = c("name", "id", "language"),
                            # The shape slider types 
                            shape_slider_types = c("triad", "dyad", "stones"),
                            # Need to know the types for each id when we process the layout. 
                            json_sig_type_by_id = NULL,
                            # when it comes to layout, we need to know which of the embedded are pure simple embedding (will be in this list) or pathway linked (not in this list)
                            json_pure_embedded = NULL,
                            json_linked_embedded = NULL,
                            json_name_by_id = NULL,
                            # an embedded ID and the parent to which it belongs. 
                            imbedded_parent = NULL,
                            unpackjson = function(tself, tparsedjson, tparsedlayout, tjsonfile, tlayoutfile, tworkbenchid, ttoken, tpoly_data, tpoly_data_file) {
                              # create the signifiers list
                              self$signifier_definitions <- vector("list", length = length(self$supported_signifier_types))
                              names(self$signifier_definitions) <- self$supported_signifier_types
                              
                              # get json
                              json_parsed <- private$processjson(tparsedjson, tjsonfile, tworkbenchid, ttoken)
                              if (is.null(tworkbenchid)) {
                                layout_parsed <- private$processjson(tparsedlayout, tlayoutfile, tworkbenchid, ttoken)
                              }
                              # get header for primary framework
                              self$parent_header <- json_parsed[private$json_header_names]
                              # following  will be depreciated
                              self$parent_framework <- as.list(json_parsed[private$json_header_names][["name"]])
                              names(self$parent_framework) <- json_parsed[private$json_header_names][["id"]]
                              # end following  will be depreciated
                              # 
                              sig_defs_embedded <- json_parsed[["signifiers"]]
                              sig_defs_header_names_embedded <- json_parsed[private$json_header_names]
                              # create the empty framework graph - will, if applicable, show all the linked and embedded frameworks
                              self$framework_graph <- igraph::make_empty_graph()
                              self$framework_embedded <- igraph::make_empty_graph()
                              
                              header_values <- json_parsed[private$json_header_names]
                              signifier_values <- json_parsed[["signifiers"]]
                              linked_frameworks <- json_parsed[["linked_frameworks"]]$framework
                              self$framework_graph <- igraph::add_vertices(graph = self$framework_graph, nv = 1, type = "framework", id = header_values[["id"]],  name = header_values[["name"]], 
                                                                           parent_id = "Top", parent_name = "Top", list = "Top", item = list("Top"), embedded_id = "Top")
                              
                           ##   self$framework_embedded <- igraph::add_vertices(graph = self$framework_embedded, nv = 1, type = "framework", id = header_values[["id"]],  name = header_values[["name"]],
                            #                                                  parent_id = header_values[["id"]], parent_name = header_values[["name"]], embedded_id = "Top")
                              private$pull_out_definitions(signifier_values, linked_frameworks, header_values)
                              # only add edge labels and colours if there is at least one edge (either linked or embedded)
                              if (nrow(igraph::get.edgelist(graph = self$framework_graph)) > 1) {
                                igraph::E(self$framework_graph)$color <- igraph::E(self$framework_graph)$colour
                                igraph::E(self$framework_graph)$label <- igraph::E(self$framework_graph)$name
                              }
                              # set any signifier types not represented in this framework to "No Entries" - used in processing this list - e.g. self$export_signifier_header_properties
                             # self$signifier_definitions <-  self$signifier_definitions %>% purrr::map( ~ {ifelse(is.null(.x), "No Entries", .x)})
                              for (i in seq_along(self$signifier_definitions)) {
                                
                                if (is.null(self$signifier_definitions[[i]])) {
                                  self$signifier_definitions[[i]] <- "No Entries"
                                }
                                
                              }
                              
                              # do the layouts
                              sig_type_list <- vector("list", length = length(self$get_supported_signifier_types()))
                              names(sig_type_list) <- self$get_supported_signifier_types()
                              self$signifierids_by_type <- sig_type_list
                              layout_signifiers <- dplyr::bind_rows(layout_parsed$settings$sections[[1]]$signifiers) %>% dplyr::select(id)
                              layout_linked_frameworks <- layout_parsed$linked_frameworks[[1]]$layout
                              framework_id  <- layout_parsed$project_id
                              private$pull_out_layout(layout_signifiers, layout_linked_frameworks, framework_id) 
                              
                              # do polymorphic signifiers if the values or file name have been provided
                              if ((!is.null(tpoly_data) | !is.null(tpoly_data_file))) {
                                # todo - validate these paramters above
                                private$apply_poly(tpoly_data, tpoly_data_file)
                              }
                              
                            },
                           
                           
                            # The new completely generic pull out definition
                            
                            pull_out_definitions = function(tsignifier_values, tlinked_frameworks, theader_values) {
                             # The master framework list
                              if (!(theader_values[["id"]] %in% names(self$frameworks))) {
                                temp_list1 <- as.list(theader_values[["name"]])
                                names(temp_list1) <- theader_values[["id"]]
                                self$frameworks <- append(self$frameworks, temp_list1)
                              }
                              # each signifier in this framework
                              for (i in seq_along(tsignifier_values[,"id"])) {
                                signifier_type <- tsignifier_values[i, "type"]
                                # process only proper signifiers or embedded definitions
                                if (signifier_type %in% self$supported_signifier_types) {
                                  # if type "embedded" - and role "collector" then pull out the linked framework definition and process that
                                  if (signifier_type == "embedded") {
                                    
                                    this_embedded_role <- tsignifier_values[i,][["content"]][["role"]]
                                    if (!is.na(this_embedded_role)) {
                                      if (this_embedded_role == "collector") {
                                        embedded_id <- tsignifier_values[i, "id"]
                                        embedded_info <- private$get_embedded_type(tsignifier_values, embedded_id, tlinked_frameworks)
                                        embedded_type <- embedded_info[["type"]]
                                        linked_id <- tsignifier_values[i,][["content"]][["embedded_engagement"]]
                                        sig_defs_child <- tlinked_frameworks %>% dplyr::filter(id == linked_id)
                                        lsignifier_values <- sig_defs_child[["signifiers"]]#[[1]]
                                        # only process the child framework if there are actual signifiers in it (or further embeddings) - it might be a simple message page. 
                                        if (any(lsignifier_values[[1]][["type"]] %in% self$supported_signifier_types)) {
                                          # if the type is linked, then the child definitions will point back up to the current framework as it's parent and will create a new branch. If
                                          # not, it is "embedded" and the signifiers are simply embedded as though they are in the current level and no child framwork is created. 
                                          if (embedded_type == "Linked") {
                                            lheader_values <- as.list(sig_defs_child[private$json_header_names])
                                            # add child vertex to the graph
                                            self$framework_graph <- igraph::add_vertices(graph = self$framework_graph, nv = 1, type = "link", id = lheader_values[["id"]], 
                                                                                         name = lheader_values[["name"]], parent_id = theader_values[["id"]], 
                                                                                         parent_name = theader_values[["name"]], list = embedded_info[["list"]],
                                                                                         item =  list(embedded_info[["item"]]), embedded_id = embedded_id)
                                          } else {
                                            # it is embedded - update the embedded graph and process the child as parent
                                            lheader_values_temp <- as.list(sig_defs_child[private$json_header_names])
                                            
                                            self$framework_embedded <- igraph::add_vertices(graph = self$framework_embedded, nv = 1, type = "linked", id = theader_values[["id"]], 
                                                                                            name = theader_values[["name"]], parent_id = "Top", 
                                                                                            parent_name = "Top",  embedded_id = "Top")
                                            
                                             self$framework_embedded <- igraph::add_vertices(graph = self$framework_embedded, nv = 1, type = "embedded", id = lheader_values_temp[["id"]], 
                                                                                            name = lheader_values_temp[["name"]], parent_id = theader_values[["id"]], 
                                                                                            parent_name = theader_values[["name"]],  embedded_id = embedded_id)
                                             self$framework_embedded <- igraph::add_edges(graph = self$framework_embedded, edges = c(theader_values[["name"]], lheader_values_temp[["name"]]), name = embedded_type, colour = ifelse(embedded_type == "Linked", "blue", "red"))
                                             lheader_values <- theader_values 
                                          }
                                          llinked_frameworks <-  sig_defs_child[["linked_frameworks"]][[1]]$framework
                                          # add the edge to the graph between the child and parent (if "embedded" then will be the same)
                                          self$framework_graph <- igraph::add_edges(graph = self$framework_graph, edges = c(theader_values[["name"]], lheader_values[["name"]]), name = embedded_type, colour = ifelse(embedded_type == "Linked", "blue", "red"))
                                          # pull out child definition
                                          private$pull_out_definitions(lsignifier_values[[1]], llinked_frameworks, lheader_values)
                                        }
                                      }
                                    }
                                  } else {
                                    # we have a standard signifier. Process it (adding to the signifier fields)
                                    # Just ripple this update if the signifier already loaded - will be the case if identical ID used across linked frameworks
                                    if (tsignifier_values[i,"id"] %in% self$get_all_signifier_ids()) {
                                      private$ripple_update(tsignifier_values[i, "id"], tsignifier_values[i,"type"], theader_values[["id"]], "Initial")
                                    } else {
                                      do.call(paste0("apply_", tsignifier_values[i,"type"]), args = list(tsignifier_values[i, ], theader_values), envir = private)
                                    }
                                    # }
                                  }
                                }
                              }
                              
                              
                            },

                           get_embedded_type = function(tjson, tembedded_id, tlinked_frameworks) {
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
                                     if (any(lsignifier_value_types %in% private$shape_slider_types) == FALSE) {
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
                           },
                            
                            # process the layout to update the signifiers by type
                            
                            pull_out_layout = function(tlayout_signifiers, tlayout_linked_frameworks, tframework_id) {
                              
                              for (sig_id in tlayout_signifiers[["id"]]) {
                                # assume neither linked or embedded - test and process accordingly
                                linked_or_embedded <- "none"
                                if (sig_id %in% igraph::vertex_attr(self$framework_graph, name =  "embedded_id") ) {
                                  linked_or_embedded <- "linked"
                                } else {
                                  if (sig_id %in% igraph::vertex_attr(self$framework_embedded, name =  "embedded_id") ) {
                                    linked_or_embedded <- "embedded"
                                  }
                                }
                                # if embedded or linked, then get the embedded framework
                                lframework_id <- NULL
                                if (linked_or_embedded != "none") {
                                  if (linked_or_embedded == "linked") {
                                    llayout_json <- tlayout_linked_frameworks %>% dplyr::filter(project_id == igraph::vertex_attr(self$framework_graph, name =  "id")[[which(sig_id == igraph::vertex_attr(self$framework_graph, name =  "embedded_id"))]])
                                    lframework_id <- llayout_json$project_id
                                  } else {
                                    llayout_json <- tlayout_linked_frameworks %>% dplyr::filter(project_id == igraph::vertex_attr(self$framework_embedded, name =  "id")[[which(sig_id == igraph::vertex_attr(self$framework_embedded, name =  "embedded_id"))]])
                                    lframework_id <- tframework_id
                                  }
                                  llayout_signifiers <- dplyr::bind_rows(llayout_json$settings$sections[[1]][["signifiers"]]) %>% dplyr::select(id)
                                  llayout_linked_frameworks <- llayout_json$linked_frameworks[[1]]$layout
                                  private$pull_out_layout(llayout_signifiers, llayout_linked_frameworks, lframework_id)
                                } else {
                                  
                                  # we might be dealing with a signifier or one of the non supported ones - so check out with the 
                                  if (length(self$get_signifier_type_by_id(id = sig_id)) > 0) {
                                    
                                    if (!(sig_id %in% self$signifierids_by_type[[self$types_by_signifierid[[sig_id]]]])) {
                                      self$signifierids_by_type[[self$types_by_signifierid[[sig_id]]]] <- append(self$signifierids_by_type[[self$types_by_signifierid[[sig_id]]]], sig_id)
                                    }
                                    
                                    temp_list1 <- list(tframework_id = NULL) 
                                    names(temp_list1) <- tframework_id
                                    if (is.null(self$signifierids_by_type_framework[[tframework_id]])) {
                                      self$signifierids_by_type_framework <- append(self$signifierids_by_type_framework, temp_list1)
                                    }
                                    if (is.null(self$signifierids_by_type_framework[[tframework_id]][[self$types_by_signifierid[[sig_id]]]])) {
                                      temp_list1 <-  list(type = NULL)
                                      names(temp_list1) <- self$types_by_signifierid[[sig_id]]
                                      self$signifierids_by_type_framework[[tframework_id]] <- append(self$signifierids_by_type_framework[[tframework_id]], temp_list1)
                                    }
                                    if (!(sig_id  %in% self$signifierids_by_type_framework[[tframework_id]][[self$types_by_signifierid[[sig_id]]]])) {
                                      self$signifierids_by_type_framework[[tframework_id]][[self$types_by_signifierid[[sig_id]]]] <- append(self$signifierids_by_type_framework[[tframework_id]][[self$types_by_signifierid[[sig_id]]]], sig_id)
                                    }
                                    
                                  }
                                }
                              }
                            },
                            
                            # apply triad in the unpacked signifiers. 
                            apply_triad = function(def, theader_values) {
                              
                              id <- def[["id"]]
                              title <- def[["title"]]
                              tooltip <-  ifelse(is.null(def[["tooltip"]]), "", def[["tooltip"]])
                              allow_na <- def[["allow_na"]]
                              fragment <- def[["fragment"]]
                              required <- def[["required"]]
                              sticky <- def[["sticky"]]
                              labels <- def[["content"]][["labels"]][[1]]
                              if (!("image" %in% colnames(labels))) {
                                labels[["image"]] <- rep_len(NA, 3)
                              }
                              if (!("show_image" %in% colnames(labels))) {
                                labels[["show_image"]] <- rep_len(FALSE, 3)
                              }
                              if (!("show_label" %in% colnames(labels))) {
                                labels[["show_label"]] <- rep_len(TRUE, 3)
                              }
                              pointer_image <- def[["content"]][["pointer_image"]]
                              background_image <- def[["content"]][["background_image"]]
                              self$add_triad(title, tooltip, allow_na, fragment, required, sticky, labels, pointer_image, background_image, theader_values, id, load = "initial")
                              
                            },
                            
                            
                            # apply dyad in the unpacked signifiers. 
                            apply_dyad = function(def, theader_values) {
                              id <- def[["id"]]
                              title <- def[["title"]]
                              tooltip <-  ifelse(is.null(def[["tooltip"]]), "", def[["tooltip"]])
                              allow_na <- def[["allow_na"]]
                              fragment <- def[["fragment"]]
                              required <- def[["required"]]
                              sticky <- def[["sticky"]]
                              labels <- def[["content"]][["labels"]][[1]]
                              if (!("image" %in% colnames(labels))) {
                                labels[["image"]] <- rep_len(NA, 2)
                              }
                              if (!("show_image" %in% colnames(labels))) {
                                labels[["show_image"]] <- rep_len(FALSE, 2)
                              }
                              if (!("show_label" %in% colnames(labels))) {
                                labels[["show_label"]] <- rep_len(TRUE, 2)
                              }
                              pointer_image <- def[["content"]][["pointer_image"]]
                              background_image <- def[["content"]][["background_image"]]
                              self$add_dyad(title, tooltip, allow_na, fragment, required, sticky, labels, pointer_image, background_image, theader_values, id, load = "initial")
                            },
                            
                            # apply free text to the unpacked signifiers
                            apply_freetext = function(def, theader_values) {
                              #function(title, tooltip, allow_na, fragment, required, sticky, multiline, include = TRUE, default = "", id = "")
                              id <- def[["id"]]
                              title <- def[["title"]]
                              tooltip <-  ifelse(is.null(def[["tooltip"]]), "", def[["tooltip"]])
                              allow_na <- def[["allow_na"]]
                              fragment <- def[["fragment"]]
                              required <- def[["required"]]
                              sticky <- def[["sticky"]]
                              multiline <- def[["content"]][["multiline"]]
                              default <- def[["content"]][["default"]]
                              self$add_freetext(title, tooltip, allow_na, fragment, required, sticky, multiline, include = TRUE, default, theader_values, id, load = "initial")
                            },
                            
                            
                            
                            # apply photo to the unpacked signifiers
                            apply_photo = function(def, theader_values) {
                              #title, tooltip, allow_na, fragment, required, sticky, id = ""
                              id <- def[["id"]]
                              title <- def[["title"]]
                              tooltip <-  ifelse(is.null(def[["tooltip"]]), "", def[["tooltip"]])
                              allow_na <- def[["allow_na"]]
                              fragment <- def[["fragment"]]
                              required <- def[["required"]]
                              sticky <- def[["sticky"]]
                              self$add_photo(title, tooltip, allow_na, fragment, required, sticky, theader_values, id, load = "initial")
                            },
                            
                            # apply audio to the unpacked signifiers
                            apply_audio = function(def, theader_values) {
                              #title, tooltip, allow_na, fragment, required, sticky, id = ""
                              id <- def[["id"]]
                              title <- def[["title"]]
                              tooltip <-  ifelse(is.null(def[["tooltip"]]), "", def[["tooltip"]])
                              allow_na <- def[["allow_na"]]
                              fragment <- def[["fragment"]]
                              required <- def[["required"]]
                              sticky <- def[["sticky"]]
                              self$add_audio(title, tooltip, allow_na, fragment, required, sticky, theader_values, id, load = "initial")
                            },
                            
                            apply_imageselect = function(def, theader_values) {
                              #add_imageselect = function(title, tooltip, allow_na, fragment, required, sticky, items, id = "")
                              id <- def[["id"]]
                              title <- def[["title"]]
                              tooltip <-  ifelse(is.null(def[["tooltip"]]), "", def[["tooltip"]])
                              allow_na <- def[["allow_na"]]
                              fragment <- def[["fragment"]]
                              required <- def[["required"]]
                              sticky <- def[["sticky"]]
                              items <- def[["content"]][["items"]][[1]]
                              self$add_imageselect(title, tooltip, allow_na, fragment, required, sticky, items, theader_values, id, load = "initial")
                            },
                            
                            
                            apply_stones = function(def, theader_values) {
                              
                              # title, tooltip, allow_na, fragment, required, sticky, stones, background_image, x_name, x_end_label, x_start_label, y_name, y_end_label, y_start_label, id = ""
                              id <- def[["id"]]
                              title <- def[["title"]]
                              tooltip <-  ifelse(is.null(def[["tooltip"]]), "", def[["tooltip"]])
                              allow_na <- def[["allow_na"]]
                              fragment <- def[["fragment"]]
                              required <- def[["required"]]
                              sticky <- def[["sticky"]]
                              stones <- def[["content"]][["stones"]][[1]]
                              background_image <-  def[["content"]][["background_image"]]
                              x_name <- ifelse(length(def[["content"]][["axis"]][["x"]][["name"]] > 0), def[["content"]][["axis"]][["x"]][["name"]], "")
                              x_end_label <-  def[["content"]][["axis"]][["x"]][["end_label"]]
                              x_start_label <-  def[["content"]][["axis"]][["x"]][["start_label"]]
                              y_name <- ifelse(length(def[["content"]][["axis"]][["y"]][["name"]] > 0), def[["content"]][["axis"]][["y"]][["name"]], "")
                              y_end_label <-  def[["content"]][["axis"]][["y"]][["end_label"]]
                              y_start_label <-  def[["content"]][["axis"]][["y"]][["start_label"]]
                              self$add_stones(title, tooltip, allow_na, fragment, required, sticky, stones, background_image, x_name, x_end_label, x_start_label, y_name, y_end_label, y_start_label, theader_values, id, load = "initial")
                            },
                            
                            # apply uniqueid to the unpacked signifiers
                            apply_uniqueid = function(def, theader_values) {
                              #title, tooltip, allow_na, fragment, required, sticky, id = ""
                              id <- def[["id"]]
                              title <- def[["title"]]
                              tooltip <-  ifelse(is.null(def[["tooltip"]]), "", def[["tooltip"]])
                              allow_na <- def[["allow_na"]]
                              fragment <- def[["fragment"]]
                              required <- def[["required"]]
                              sticky <- def[["sticky"]]
                              self$add_uniqueid(title, tooltip, allow_na, fragment, required, sticky, theader_values, id, load = "initial")
                            },
                            apply_list = function(def, theader_values) {
                              #add_list = function(title, tooltip, allow_na, fragment, required, sticky, items, dynamic, random_order, max_responses, min_responses, id = "") {
                              id <- def[["id"]]
                              title <- def[["title"]]
                              tooltip <-  ifelse(is.null(def[["tooltip"]]), "", def[["tooltip"]])
                              allow_na <- def[["allow_na"]]
                              fragment <- def[["fragment"]]
                              required <- def[["required"]]
                              sticky <- def[["sticky"]]
                              items <- def[["content"]][["items"]][[1]]
                              # id, title, tooltip, visible, other_signifier_id
                                items[["tooltip"]] <- items[["title"]]
                              if (!("visible" %in% colnames(items))) {
                                items[["visible"]] <- rep_len(TRUE, nrow(items))
                              }
                              if (!("other_signifier_id" %in% colnames(items))) {
                                items[["other_signifier_id"]] <- rep_len("", nrow(items))
                              } else {
                                # ToDo check this - the logic doesn't look right and not sure on the linked_other_signifier, should be deleted
                                if (all(!is.na(items[["other_signifier_id"]]))) {
                                  temp_list <- as.list(rep_len(x = id,  length.out   = length(items[["other_signifier_id"]])))
                                  names(temp_list) <- items[["other_signifier_id"]]
                                  self$linked_other_signifier <- append(self$linked_other_signifier, temp_list)
                                }
                                
                              }
                              
                              if ("image" %in% colnames(items)) {
                                items[["image"]] <- NULL
                              }
                              
                              dynamic <- def[["content"]][["dynamic"]]
                              max_responses <- def[["content"]][["max_responses"]]
                              min_responses <- def[["content"]][["min_responses"]]
                              other_item_id <- ifelse(length(def[["content"]][["other_item_id"]] > 0), def[["content"]][["other_item_id"]], "")
                              other_signifier_id<- ifelse(length(def[["content"]][["other_signifier_id"]] > 0), def[["content"]][["other_signifier_id"]], "")
                              self$add_list(title, tooltip, allow_na, fragment, required, sticky, items, max_responses, min_responses, other_item_id, other_signifier_id, theader_values, id, load = "initial")
                            },
                            
                            # Process the json passed into the initialize
                            processjson = function(parsedjson, jsonfile, workbenchid, token) {
                              # if the json passed is already parsed, then return -  no processing
                              if(!is.null(parsedjson)) {
                                # ToDo validiate that this is json
                                return(parsedjson)
                              }
                              # Workbench ID and token passed - return the json file from the server
                              if (is.null(jsonfile)) {
                                # ToDo Validate token and workbenchID passed
                                return(private$getserverJSON(workbenchid, token))
                                # JSON file passed, so parse it.
                              } else {
                                # ToDo validate the parse.
                                return(jsonlite::fromJSON(jsonfile, simplifyVector = TRUE, simplifyDataFrame = TRUE, flatten = FALSE))
                              }
                            },
                            # you can call this directly with workbenchid NULL to return all the authorised frameworks. i.e. in
                            # setting up the authorised frameworks you do not call the getJSON.
                            getserverJSON = function(workbenchid, token) {
                              out <- try( {
                                # get the json from the returned project definition
                                return(jsonlite::fromJSON(httr::content(httr::GET(
                                  paste0("https://", private$getsysvalue("openAPIEndPoint"), ".sensemaker-suite.com/apis/projectdefinition/",  workbenchid),
                                  httr::add_headers(.headers = c('Authorization' = paste("Bearer", token, sep = " ")
                                                                 , 'Content-Type' = 'application/json'))
                                ), as = 'text', encoding = 'utf-8'), simplifyVector = TRUE, simplifyDataFrame = TRUE ,flatten = FALSE))
                                
                              }
                              )
                              if(inherits(out, "try-error"))
                              {
                                return(NULL)
                              }
                              if(inherits(out, "try-warning"))
                              {
                                return(NULL)
                              }
                              return(out)
                            },
                           # apply the polymorphic definitions if there are any to this framework
                            apply_poly = function(tpoly_data, tpoly_data_file) {
                              if (is.null(tpoly_data) & is.null(tpoly_data_file)) {return(NULL)}
                              if (!is.null(tpoly_data_file)) {
                                tpoly_data <- jsonlite::fromJSON(tpoly_data_file, simplifyVector = TRUE, simplifyDataFrame = TRUE, flatten = FALSE)
                              }
                              
                              for (i in seq_along(tpoly_data$polymorphic_definitions[["id"]])) {
                                
                                poly_entry <- tpoly_data$polymorphic_definitions[i, ]
                                k <- 1
                                if (poly_entry[["type"]] == "triad") {
                                  top_anchor <- private$label_definition_R6()$new(id = tpoly_data$polymorphic_definitions[i, "content"][["labels"]][[1]][k, "id"], 
                                                                          text = tpoly_data$polymorphic_definitions[i, "content"][["labels"]][[1]][k, "text"], 
                                                                          show_image = tpoly_data$polymorphic_definitions[i, "content"][["labels"]][[1]][k, "show_image"], 
                                                                          show_label = tpoly_data$polymorphic_definitions[i, "content"][["labels"]][[1]][k, "show_label"], 
                                                                          image = tpoly_data$polymorphic_definitions[i, "content"][["labels"]][[1]][k, "image"])
                                  k <- k + 1
                                }
                                left_anchor <- private$label_definition_R6()$new(id = tpoly_data$polymorphic_definitions[i, "content"][["labels"]][[1]][k, "id"], 
                                                                         text = tpoly_data$polymorphic_definitions[i, "content"][["labels"]][[1]][k, "text"], 
                                                                         show_image = tpoly_data$polymorphic_definitions[i, "content"][["labels"]][[1]][k, "show_image"], 
                                                                         show_label = tpoly_data$polymorphic_definitions[i, "content"][["labels"]][[1]][k, "show_label"], 
                                                                         image = tpoly_data$polymorphic_definitions[i, "content"][["labels"]][[1]][k, "image"])
                                k <- k + 1
                                right_anchor <- private$label_definition_R6()$new(id = tpoly_data$polymorphic_definitions[i, "content"][["labels"]][[1]][k, "id"], 
                                                                          text = tpoly_data$polymorphic_definitions[i, "content"][["labels"]][[1]][k, "text"], 
                                                                          show_image = tpoly_data$polymorphic_definitions[i, "content"][["labels"]][[1]][k, "show_image"], 
                                                                          show_label = tpoly_data$polymorphic_definitions[i, "content"][["labels"]][[1]][k, "show_label"], 
                                                                          image = tpoly_data$polymorphic_definitions[i, "content"][["labels"]][[1]][k, "image"])
                                labels <- list(top_anchor = top_anchor, left_anchor = left_anchor,
                                               right_anchor = right_anchor)
                                content <- private$poly_slider_content_definition_R6()$new(labels = labels)
                                # now the polymorphic to signifiers
                                for (l in seq_along(tpoly_data$polymorphic_definitions[i, "polymorphic_to"][["poly_signifiers"]][[1]][["id"]])) {
                                  if (poly_entry[["type"]] == "triad") {
                                    if (l == 1) {
                                      polymorphic_to <- data.frame(id = tpoly_data$polymorphic_definitions[i, "polymorphic_to"][["poly_signifiers"]][[1]][l, "id"], 
                                                                   top = tpoly_data$polymorphic_definitions[i, "polymorphic_to"][["poly_signifiers"]][[1]][l, "top"], 
                                                                   left = tpoly_data$polymorphic_definitions[i, "polymorphic_to"][["poly_signifiers"]][[1]][l, "left"], 
                                                                   right = tpoly_data$polymorphic_definitions[i, "polymorphic_to"][["poly_signifiers"]][[1]][l, "right"])
                                    } else {
                                      polymorphic_to <-  dplyr::bind_rows(polymorphic_to, data.frame(id = tpoly_data$polymorphic_definitions[i, "polymorphic_to"][["poly_signifiers"]][[1]][l, "id"], 
                                                                                                     top = tpoly_data$polymorphic_definitions[i, "polymorphic_to"][["poly_signifiers"]][[1]][l, "top"], 
                                                                                                     left = tpoly_data$polymorphic_definitions[i, "polymorphic_to"][["poly_signifiers"]][[1]][l, "left"], 
                                                                                                     right = tpoly_data$polymorphic_definitions[i, "polymorphic_to"][["poly_signifiers"]][[1]][l, "right"]))
                                    }
                                  } else {
                                    if (l == 1) {
                                      polymorphic_to <- data.frame(id = tpoly_data$polymorphic_definitions[i, "polymorphic_to"][["poly_signifiers"]][[1]][l, "id"], 
                                                                   left = tpoly_data$polymorphic_definitions[i, "polymorphic_to"][["poly_signifiers"]][[1]][l, "left"], 
                                                                   right = tpoly_data$polymorphic_definitions[i, "polymorphic_to"][["poly_signifiers"]][[1]][l, "right"])
                                    } else {
                                      polymorphic_to <-  dplyr::bind_rows(polymorphic_to, data.frame(id = tpoly_data$polymorphic_definitions[i, "polymorphic_to"][["poly_signifiers"]][[1]][l, "id"], 
                                                                                                     left = tpoly_data$polymorphic_definitions[i, "polymorphic_to"][["poly_signifiers"]][[1]][l, "left"], 
                                                                                                     right = tpoly_data$polymorphic_definitions[i, "polymorphic_to"][["poly_signifiers"]][[1]][l, "right"]))
                                    }
                                    
                                  }
                                }
                                temp_thing <- list(private$polymorphic_definition_R6()$new(id = poly_entry[["id"]], type = poly_entry[["type"]], title = poly_entry[["title"]], content = content, polymorphic_to = polymorphic_to, include = TRUE))
                                names(temp_thing) <- poly_entry[["id"]]
                                self$polymorphic_definitions  <- append(self$polymorphic_definitions, temp_thing)
                                # add the polymorphic_type_by_ids
                                temp_list <- as.list(poly_entry[["type"]])
                                names(temp_list) <- poly_entry[["id"]]
                                self$polymorphic_type_by_ids <- append(self$polymorphic_type_by_ids, temp_list)
                                # add the polymorphic_ids_by_title
                                temp_list <- as.list(poly_entry[["id"]])
                                names(temp_list) <- poly_entry[["title"]]
                                self$polymorphic_ids_by_title <- append(self$polymorphic_ids_by_title, temp_list)
                                # add the polymorphic_id_list_by_type
                                if (is.null(self$polymorphic_id_list_by_type[["type"]])) {
                                  temp_list <- as.list(as.list(poly_entry[["id"]]))
                                  names(temp_list) <- poly_entry[["type"]]
                                  self$polymorphic_id_list_by_type <- append(self$polymorphic_id_list_by_type, temp_list)
                                } else {
                                  self$polymorphic_id_list_by_type[["type"]] <- append(self$polymorphic_id_list_by_type[["type"]], poly_entry[["id"]])
                                }
                                
                              }
                                
                                
                             
                              
                            },
                            #============================================================================================
                            # R6 Class Instance Builds
                            # Using composition not inheretence - follows the JSON very directly, thus triads has content
                            #  containing the labels of anchors with supplied properties. List content as lists of list item properties and so on.
                            #============================================================================================
                            
                            
                            #
                            append_stone_columns = function(items, n, stone) {
                              return(unlist(list(paste0(stone, "_", items, "XRight"), paste0(stone, "_", items, "YTop"))))
                            },
                            #
                            
                            #
                            
                            #
                            build_list_item = function(x, tliid, ids) {
                              df_row <- ids %>% dplyr::filter(id == tliid)
                              listR6 <- private$item_list_definition_R6()$new(id = df_row[["id"]], title = df_row[["title"]], tooltip = ifelse("tooltip" %in% colnames(df_row), df_row[["tooltip"]], ""), visible = ifelse("visible" %in% colnames(df_row), df_row[["visible"]], TRUE), image = ifelse("image" %in% colnames(df_row), df_row[["image"]], ""), other_signifier_id = ifelse("other_signifier_id" %in% colnames(df_row), df_row[["other_signifier_id"]], ""))
                              return(listR6)
                            },
                            #
                            
                            #
                            build_stone_entry = function(x, tliid, ids) {
                              df_row <- ids %>% dplyr::filter(id == tliid)
                              stoneR6 <- private$stones_R6()$new(id = df_row[["id"]], image = ifelse("image" %in% colnames(df_row), df_row[["image"]], ""), title = df_row[["title"]], tooltip = ifelse("tooltip" %in% colnames(df_row), df_row[["tooltip"]], ""), visible = ifelse("visible" %in% colnames(df_row), df_row[["visible"]], TRUE))
                              return(stoneR6)
                            },
                            #
                            
                            #
                            build_imageselect_item = function(x, tliid, ids) {
                              df_row <- ids[as.numeric(tliid),]
                              listR6 <- private$imageselect_list_definition_R6()$new(default = df_row)
                              return(listR6)
                            },
                            
                            
                            getsysvalue = function(valuetype) {
                              # ToDo - hard coded for now - put in a system file of some sort
                              # ToDo - Once in a file, then cache
                              
                              if (valuetype == "openAPIEndPoint") {
                                return("openapi")
                              }
                            },
                            #
                            imageselect_content_definition_R6 =function() {
                              
                              contentimageselect <- R6::R6Class("contentslider",
                                                                public = list(
                                                                  items = NA,
                                                                  num_items = NA,
                                                                  initialize = function(items, num_items) {
                                                                    self$items <- items
                                                                    self$num_items <- num_items
                                                                  }
                                                                )
                              )
                            },
                            #
                            imageselect_list_definition_R6 = function() {
                              # An image select signifier definition has items
                              itemimageselect <- R6::R6Class("itemimageselect",
                                                             public = list(
                                                               default = NA,
                                                               initialize = function(default) {
                                                                 self$default <- default
                                                               }
                                                             )
                              )
                            },
                            #
                            freetext_content_definition_R6 = function() {
                              contentfreetext <- R6::R6Class("contentfreetext",
                                                             public = list(
                                                               default = NA,
                                                               multiline = NA,
                                                               initialize = function(default, multiline) {
                                                                 self$default <- default
                                                                 self$multiline <- multiline
                                                               }
                                                             )
                              )
                            },
                            #
                            stone_content_definition_R6 = function() {
                              contentstone <- R6::R6Class("contentstone",
                                                          public = list(
                                                            axis = NA,
                                                            stones = NA,
                                                            num_stones = NA,
                                                            background_image = NA,
                                                            initialize = function(axis, stones, num_stones, background_image) {
                                                              self$axis <- axis
                                                              self$stones <- stones
                                                              self$num_stones <- num_stones
                                                              self$background_image <- background_image
                                                            }
                                                          )
                              )
                            },
                            #
                            stones_R6 = function() {
                              stones <- R6::R6Class("stones",
                                                    public = list(
                                                      id = NA,
                                                      image = NA,
                                                      title = NA,
                                                      tooltip = NA,
                                                      visible = NA,
                                                      initialize = function(id, image, title, tooltip, visible) {
                                                        self$id = id
                                                        self$image = image
                                                        self$title = title
                                                        self$tooltip = tooltip
                                                        self$visible = visible
                                                      }
                                                    )
                              )
                            },
                            #
                            stones_axis_R6 = function() {
                              stoneaxis <- R6::R6Class("stoneaxis",
                                                       public = list(
                                                         axis = NA,
                                                         name = NA,
                                                         end_label = NA,
                                                         start_label = NA,
                                                         initialize = function(axis, name, end_label, start_label) {
                                                           self$axis <- axis
                                                           self$name <- name
                                                           self$end_label <- end_label
                                                           self$start_label <- start_label
                                                         }
                                                       )
                              )
                            },
                            # List content - it has the following fields including a list of items.
                            item_content_definition_R6 = function() {
                              return(contentlist <- R6::R6Class("contentlist",
                                                                public = list(
                                                                  items = NA,
                                                                  num_items = NA,
                                                                  max_responses = NA,
                                                                  min_responses = NA,
                                                                  other_item_id = NA,
                                                                  other_signifier_id = NA,
                                                                  initialize = function(items, num_items, max_responses, min_responses, other_item_id, other_signifier_id) {
                                                                    self$items <- items
                                                                    self$num_items <- num_items
                                                                    self$max_responses <- max_responses
                                                                    self$min_responses <- min_responses
                                                                    self$other_item_id <- other_item_id
                                                                    self$other_signifier_id <- other_signifier_id
                                                                  }
                                                                )
                              )
                              )
                            },
                            #
                            item_list_definition_R6 = function() {
                              # An list signifier definition has items, not labels - but items (like content in general) varies with the signifier type. This is the list item
                              return(R6::R6Class("itemlist",
                                                 public = list(
                                                   id = NA,
                                                   title = NA,
                                                   tooltip = NA,
                                                   visible = NA,
                                                   image = NA,
                                                   other_signifier_id = NA,
                                                   initialize = function(id, title, tooltip, visible, image, other_signifier_id) {
                                                     self$id <- id
                                                     self$title <- title
                                                     self$tooltip <- tooltip
                                                     self$visible <- visible
                                                     self$image <- image
                                                     self$other_signifier_id <- other_signifier_id
                                                   }
                                                 )
                              )
                              )
                            },
                            #
                            # id, title, tooltip, visible, image,  
                            slider_content_definition_R6 = function() {
                              # content for a triad - it has the following fields plus a list of 3 labels (top, left and right)
                              return(R6::R6Class("contentslider",
                                                 public = list(
                                                   labels = NA,
                                                   pointer_image = NA,
                                                   background_image = NA,
                                                   initialize = function(labels, pointer_image, background_image) {
                                                     self$labels <- labels
                                                     self$pointer_image <- pointer_image
                                                     self$background_image <- background_image
                                                   }
                                                 )
                              ))
                            },
                            #
                            label_definition_R6 = function() {
                              # A label class used in triads and dyads
                              return(R6::R6Class("label",
                                                 public = list(
                                                   id = NA,
                                                   text = NA,
                                                   show_image = NA,
                                                   show_label = NA,
                                                   image = NA,
                                                   initialize = function(id, text, show_image, show_label, image) {
                                                     self$id <- id
                                                     self$text <- text
                                                     self$show_image <- show_image
                                                     self$show_label <- show_label
                                                     self$image <- image
                                                   }
                                                 )
                              ))
                            },
                            #
                            signifier_definition_R6 = function() {
                              
                              return(R6::R6Class("signifierdefinition",
                                                 public = list(
                                                   id = NA,
                                                   type = NA,
                                                   title = NA,
                                                   tooltip = NA,
                                                   allow_na = NA,
                                                   fragment = NA,
                                                   required = NA,
                                                   sticky = NA,
                                                   content = NA,
                                                   include = NA,
                                                   hide = FALSE,
                                                   initialize = function(id, type, title, tooltip, allow_na, fragment, required, sticky, content, include) {
                                                     self$id <- id
                                                     self$type <- type
                                                     self$title <- title
                                                     self$tooltip <- tooltip
                                                     self$allow_na <- allow_na
                                                     self$fragment <- fragment
                                                     self$required <- required
                                                     self$sticky <- sticky
                                                     self$content <- content
                                                     self$include <- include
                                                   },
                                                   hide_signifier = function() {
                                                     self$hide <- TRUE
                                                   },
                                                   unhide_signifier = function() {
                                                     self$hide = FALSE
                                                   },
                                                   get_type = function() {
                                                     return(self$type)
                                                   },
                                                   get_title = function() {
                                                     return(self$title)
                                                   },
                                                   get_tooltip = function() {
                                                     return(self$tooltip)
                                                   },
                                                   get_allow_na = function() {
                                                     return(self$allow_na)
                                                   },
                                                   get_fragment = function() {
                                                     return(self$fragment)
                                                   },
                                                   get_required = function() {
                                                     return(self$required)
                                                   },
                                                   get_sticky = function() {
                                                     return(self$sticky)
                                                   },
                                                   get_content = function() {
                                                     return(self$content)
                                                   },
                                                   get_include = function() {
                                                     return(self$include)
                                                   },
                                                   get_hide = function() {
                                                     return(self$hide)
                                                   },
                                                   get_property = function(property) {
                                                     return(self[[property]])
                                                   },
                                                   set_type = function(type) {
                                                     self$type <- type
                                                   },
                                                   set_title = function(title) {
                                                     self$title <- title
                                                   },
                                                   set_tooltip = function(tooltip) {
                                                     self$tooltip <- tooltip
                                                   },
                                                   set_allow_na = function(allow_na) {
                                                     self$allow_na <- allow_na
                                                   },
                                                   set_fragment = function(fragment) {
                                                     self$fragment <- fragment
                                                   },
                                                   set_required = function(required) {
                                                     self$required <- required
                                                   },
                                                   set_sticky = function(sticky) {
                                                     self$sticky <- sticky
                                                   },
                                                   set_content = function(content) {
                                                     self$content <- content
                                                   },
                                                   set_include = function(include) {
                                                     self$include <- include
                                                   },
                                                   set_hide = function(hide) {
                                                     self$hide_signifier <- hide
                                                   },
                                                   set_property = function(property, value) {
                                                     self[[property]] <- value
                                                   }
                                                 )
                              ))
                            },
                           # P O L Y  M O R P H I C. D E F I N I T I O N S
                           # polydefinitions
                           # Defines the definition of a polymorphic signifier - containing the content for the signifier (triad or dyad for now) and
                           # the polymorphic to, which contains all the dyads/triads that this definition combines. 
                           polymorphic_definition_R6 = function() {
                             return(R6::R6Class("polymorphic_definition",
                                                public = list(
                                                  id = NA,
                                                  type = NA,
                                                  title = NA,
                                                  content = NA,
                                                  polymorphic_to = NA,
                                                  include = NA,
                                                  hide = FALSE,
                                                  initialize = function(id, type, title,content, polymorphic_to, include) {
                                                    self$id <- id
                                                    self$type <- type
                                                    self$title <- title
                                                    self$content <- content
                                                    self$polymorphic_to <- polymorphic_to
                                                    self$include <- include
                                                  },
                                                  hide_signifier = function() {
                                                    self$hide <- TRUE
                                                  },
                                                  unhide_signifier = function() {
                                                    self$hide = FALSE
                                                  },
                                                  get_type = function() {
                                                    return(self$type)
                                                  },
                                                  get_title = function() {
                                                    return(self$title)
                                                  },
                                                  get_content = function() {
                                                    return(self$content)
                                                  },
                                                  get_polymorphic_to = function() {
                                                    return(self$polymorphic_to)
                                                  },
                                                  get_include = function() {
                                                    return(self$include)
                                                  },
                                                  get_hide = function() {
                                                    return(self$hide)
                                                  },
                                                  get_property = function(property) {
                                                    return(self[[property]])
                                                  },
                                                  set_type = function(type) {
                                                    self$type <- type
                                                  },
                                                  set_title = function(title) {
                                                    self$title <- title
                                                  },
                                                  set_content = function(content) {
                                                    self$content <- content
                                                  },
                                                  set_polymorphic_to = function(polymorphic_to) {
                                                    self$polymorphic_to <- polymorphic_to
                                                  },
                                                  set_include = function(include) {
                                                    self$include <- include
                                                  },
                                                  set_hide = function(hide) {
                                                    self$hide_signifier <- hide
                                                  },
                                                  set_property = function(property, value) {
                                                    self[[property]] <- value
                                                  }
                                                )
                             ))
                           },
                          
                           
                           #
                           triad_polymorphic_to_R6 = function() {
                             # A label class used in triads and dyads
                             return(R6::R6Class("polymorphic_to",
                                                public = list(
                                                  id = NA,
                                                  top = NA,
                                                  left = NA,
                                                  right = NA,
                                                  initialize = function(id, top, left, right) {
                                                    self$id <- id
                                                    self$top <- top
                                                    self$left <- left
                                                    self$right <- right
                                                  }
                                                )
                             ))
                           },
                           #
                           dyad_polymorphic_to_R6 = function() {
                             # A label class used in triads and dyads
                             return(R6::R6Class("polymorphic_to",
                                                public = list(
                                                  id = NA,
                                                  left = NA,
                                                  right = NA,
                                                  initialize = function(id, left, right) {
                                                    self$id <- id
                                                    self$left <- left
                                                    self$right <- right
                                                  }
                                                )
                             ))
                           },
                           
                           poly_slider_content_definition_R6 = function() {
                             # content for a polymorphic slider definition - - it only contains the labels which will be a dataframe of
                             # triad_polymorphic_to_R6 definitions or dyad_polymorphic_to_R6 definitions
                             return(R6::R6Class("contentslider",
                                                public = list(
                                                  labels = NA,
                                                  initialize = function(labels) {
                                                    self$labels <- labels
                                                  }
                                                )
                             ))
                           },

                            # ripple the addition of a new signifier
                            
                            ripple_update = function(tid, ttype, tframework_id, load) {
    
                              # type by id
                              add_list <- list(ttype)
                              names(add_list) <- tid
                              if (!(tid %in% names(self$types_by_signifierid))) {
                                self$types_by_signifierid <- append(self$types_by_signifierid, add_list)
                              }
                              # id by type
                              if (load != "initial") {
                                self$signifierids_by_type[[ttype]] <- append(self$signifierids_by_type[[ttype]], tid)
                              }
                              # types with signifiers
                              if (!(ttype %in% self$types_with_signifiers)) {
                                self$types_with_signifiers <- append(self$types_with_signifiers, ttype)
                              }
                              
                              # Set up the data structure as we go for the count by linked project signifier type 
                              # 1. the table with counts by signifier type self$signifier_counts_linked_frameworks_type
                              temp_list <- vector("list", length = 1)
                              names(temp_list) <- tframework_id
                              st_df <- tibble::tibble(types = self$supported_signifier_types, n = rep_len(0, length(self$supported_signifier_types)))
                              if (is.null(self$signifier_counts_linked_frameworks_type)) {
                                self$signifier_counts_linked_frameworks_type <- temp_list 
                                self$signifier_counts_linked_frameworks_type[[tframework_id]] <- st_df
                              } else {
                                if (!(tframework_id %in% names(self$signifier_counts_linked_frameworks_type)) && is.null(self$signifier_counts_linked_frameworks_type[[tframework_id]])) {
                                  self$signifier_counts_linked_frameworks_type <- append(self$signifier_counts_linked_frameworks_type, temp_list)
                                  self$signifier_counts_linked_frameworks_type[[tframework_id]] <- st_df
                                }
                              }
                              # now increment the count for the type/linked project
                              self$signifier_counts_linked_frameworks_type[[tframework_id]][which(self$signifier_counts_linked_frameworks_type[[tframework_id]][["types"]] == ttype), "n"] <-  self$signifier_counts_linked_frameworks_type[[tframework_id]][which(self$signifier_counts_linked_frameworks_type[[tframework_id]][["types"]] == ttype), "n"] + 1
                              
                              # types_by_signifierid_framework
                              append_list <- list(ttype)
                              names(append_list) <- tid
                              self$types_by_signifierid_framework[[tframework_id]] <- append(self$types_by_signifierid_framework[[tframework_id]], append_list)
                              
                              #
                              # signifierids_by_type_framework
                              if (load != "initial") {
                                self$signifierids_by_type_framework[[tframework_id]][[ttype]] <- append(self$signifierids_by_type_framework[[tframework_id]][[ttype]], tid)
                              }
                              #
                              # types_with_signifiers_framework
                              if (!(ttype %in% self$types_with_signifiers_framework[[tframework_id]])) {
                                self$types_with_signifiers_framework[[tframework_id]] <- append(self$types_with_signifiers_framework[[tframework_id]], ttype)
                              }
                            },
                            
                            # remove signifier definition reference # ToDo - do the linked projects too
                            remove_signifier_reference = function(tid, tfw_id = "", ttype = "") {
                              # Note with the signifier library, the same signifier id may appear in more than one framework
                              if (fw_id == "") {
                                tfw_id <- self$get_framework_for_id(tid, ttype)
                              }
                              if (ttype == "") {
                                ttype <- self$get_signifier_type_by_id(tid)
                              }
                              self$types_by_signifierid <-   self$types_by_signifierid[-which(names(self$types_by_signifierid) == tid)]
                              self$signifierids_by_type[[ttype]] <- self$signifierids_by_type[[ttype]][self$signifierids_by_type[[ttype]] != tid]
                              # if no more of this type then remove type
                              if (length(self$signifierids_by_type[[ttype]]) == 0) {
                                self$signifierids_by_type[ttype] <- NULL
                                temp_list <- list(NULL)
                                names(temp_list) <- ttype
                                self$signifierids_by_type <- append(self$signifierids_by_type, temp_list)
                                self$types_with_signifiers <- self$types_with_signifiers[self$types_with_signifiers != ttype]
                              }
                              # this next one probably will go
  
                              self$signifier_definitions[[ttype]] <- self$signifier_definitions[[ttype]][! names(self$signifier_definitions[[ttype]]) == tid]
                              # now remove from the linked frameworks (which includes the parent)
                              # now remove entries from the framework thingy
                              for (fw_id in tfw_id) {
                                self$signifier_counts_linked_frameworks_type[[fw_id]][which(self$signifier_counts_linked_frameworks_type[[fw_id]][["types"]] == ttype), "n"] = 
                                  self$signifier_counts_linked_frameworks_type[[fw_id]][which(self$signifier_counts_linked_frameworks_type[[fw_id]][["types"]] == ttype), "n"] - 1
                                self$types_by_signifierid_framework[[fw_id]] <- self$types_by_signifierid_framework[[fw_id]][-which(names(self$types_by_signifierid_framework[[fw_id]]) == tid)]
                                self$signifierids_by_type_framework[[fw_id]][[ttype]] <- self$signifierids_by_type_framework[[fw_id]][[ttype]][-which(self$signifierids_by_type_framework[[fw_id]][[ttype]] == tid)]
                                if (length(self$signifierids_by_type_framework[[fw_id]][[ttype]]) == 0) {
                                  self$signifierids_by_type_framework[[fw_id]] <- self$signifierids_by_type_framework[[fw_id]][-which(names(self$signifierids_by_type_framework[[fw_id]]) == ttype)] 
                                }
                              }
                            },

                           # check whether a particular framework has the signifier - return the fw_id if so otherwise NULL returned
                           check_framework_for_id = function(fw_id, ttype, tsig_id) {
                             ids <- self$get_linked_framework_ids_by_type(fw_id = fw_id, ttype)
                             if (tsig_id %in% ids) {
                               return(fw_id)
                             }
                           },
                           
                            # get a list of list signifier type ids and their max responses
                            get_max_responses = function(R6list) {
                              return(purrr::map(R6list, ~{.x$content$max_responses}))
                            },
                            # Get a list with each listID that has other (as name of return list) containing the other text box id
                            # ToDo - figure how to do this properly in map OR just do loops - it is a bit over the top
                            get_list_with_other = function(tlist) {
                              pl1 <- vector("list", length = length(tlist))
                              names(pl1) <- tlist
                              ls <- unlist(purrr::imap(pl1, private$getMine), recursive = FALSE) 
                              ls <- ls[unname(unlist(purrr::map(ls, private$is_not_blank)))]
                              names(ls) <- names(ls) %>% stringr::str_sub(start = 1, end = 36)
                              item_count <- as.data.frame(table(names(ls)))
                              item_count <- item_count %>% dplyr::filter(Freq == 1)
                              ls <-  ls[names(ls) %in% item_count[["Var1"]]]
                              return(ls)
                            },
                            # to fill - rename it
                            getMine = function(tlist, d) {
                              result <- lapply(self$get_list_items_ids(d), private$get_next, y = d) 
                            },
                            # to fill - rename it
                            get_next = function(x, y) {
                              return(self$get_list_item_other_signifier_id(y, x)) 
                            },
                            # gendric is not blank, null or na helper function. 
                            is_not_blank = function(x) {
                              ret_value <- TRUE
                              if (is.na(x)) {return(FALSE)}
                              if (is.null(x)) {return(FALSE)}
                              if (x == "") {return(FALSE)}
                              return(TRUE)
                            },
                            # generic remove html helper function
                            removeHTML = function(tString) {
                              return(gsub("<.*?>", " ", tString))
                            },
                            # Build a data export from triads
                            build_triad_export = function(x, y) {
                              df <- cbind(data.frame(id = y), data.frame(title = self$get_signifier_title(y)), self$get_triad_anchor_ids(y), self$get_triad_anchor_texts(y))
                              colnames(df) <- c("id", "title", "top_id", "left_id", "right_ids", "top_text", "left_text", "right_text")
                              return(df)
                            },
                            # For update triad anchor titles and title from data frame or csv export
                            apply_triad_conent_update = function(df) {
                              mydf <- df %>%
                                purrr::pwalk(function(...) {
                                  current <- data.frame(...)
                                  self$change_signifier_title(id = current[["id"]], value = current[["title"]])
                                  self$update_triad_top_label_value(id =   current[["id"]], value = current[["top_text"]],   property = "text")
                                  self$update_triad_left_label_value(id =  current[["id"]], value = current[["left_text"]],  property = "text")
                                  self$update_triad_right_label_value(id = current[["id"]], value = current[["right_text"]], property = "text")
                                })
                            },
                            # build a data export from lists items
                            build_list_export = function(x, y) {
                              item_ids <-  self$get_list_items_ids(id = y)
                              item_titles <-  self$get_list_items_titles(id = y)
                              ids <- rep_len(y, length(item_ids))
                              titles <- rep_len(self$get_signifier_title(y), length(item_ids))
                              return(data.frame(id = ids, title =titles, item_ids = item_ids, item_titles = item_titles))
                            },
                            # for update list titles from data frame or csv file
                            apply_list_conent_update = function(df) {
                              mydf <- df %>% 
                                purrr::pwalk(function(...) {
                                  current <- data.frame(...)
                                  self$update_list_content_item_title(sig_id = current[["id"]], item_id = current[["item_ids"]], "title",  value = current[["item_titles"]])
                                })
                            },
                            # build a data export for titles for dyads
                            build_dyad_export = function(x, y) {
                              df <- cbind(data.frame(id = y), data.frame(title = self$get_signifier_title(y)), self$get_dyad_anchor_ids(y), self$get_dyad_anchor_texts(y))
                              colnames(df) <- c("id", "title", "left_id", "right_ids",  "left_text", "right_text")
                              return(df)
                            },
                            # For update dyad titles from data frame or csv file
                            apply_dyad_conent_update = function(df) {
                              mydf <- df %>%
                                purrr::pwalk(function(...) {
                                  current <- data.frame(...)
                                  self$change_signifier_title(id = current[["id"]], value = current[["title"]])
                                  self$update_dyad_left_label_value(id = current[["id"]], value = current[["left_text"]],  property = "text")
                                  self$update_dyad_right_label_value(id = current[["id"]], value = current[["right_text"]], property = "text")
                                })
                            },
                            # Helper function 1 on outputting a data frame of all signifier header properties
                            process_frag_type = function(fragtype) {
                              dplyr::bind_rows(purrr::map(fragtype, private$process_frag))
                            },
                            # Helper function 2 on outputting a data frame of all signifier header properties
                            process_frag = function(frag) {
                              base::as.data.frame(base::as.list(frag)[c("type", "id", self$get_signifier_supported_header_properties())])
                            },
                            # Build a data export from freetext
                            build_freetext_export = function(x, y) {
                              df <- cbind(data.frame(id = y), data.frame(title = self$get_signifier_title(y)))
                              colnames(df) <- c("id", "title")
                              return(df)
                            },
                            # For update freetext titles and title from data frame or csv export
                            apply_freetext_conent_update = function(df) {
                              mydf <- df %>%
                                purrr::pwalk(function(...) {
                                  current <- data.frame(...)
                                  
                                  self$change_signifier_title(id = current[["id"]], value = current[["title"]])
                                })
                            },
                            # Build a data export from stones
                            build_stones_export = function(x, y) {
                              df <- cbind(data.frame(id = y), data.frame(title = self$get_signifier_title(y)))
                              colnames(df) <- c("id", "title")
                              return(df)
                            },
                            # For update stones  titles and title from data frame or csv export
                            apply_stones_conent_update = function(df) {
                              mydf <- df %>%
                                purrr::pwalk(function(...) {
                                  current <- data.frame(...)
                                  
                                  self$change_signifier_title(id = current[["id"]], value = current[["title"]])
                                })
                            },
                            # get R6 property from within an imap loop
                            get_R6_property = function(x, y, property, id) {
                              self$get_stones_stone_by_id_R6(id, y)[[property]]
                            },

                            # flatten a list of lists to a list - useful helper function
                            # ToDo - put this in the data/framework neutral helper package when you do it. 
                            flattenlist = function(x){  
                              morelists <- sapply(x, function(xprime) class(xprime)[1]=="list")
                              out <- c(x[!morelists], unlist(x[morelists], recursive=FALSE))
                              if(sum(morelists)){ 
                                Recall(out)
                              }else{
                                return(out)
                              }
                            }
                          ) # private
) # R6 class
