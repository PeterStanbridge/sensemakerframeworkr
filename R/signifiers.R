
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
#' fw_triads <- myfw$get_signifier_ids_by_type("triad")
#' triad_01_image <- pt$get_triad_background_image(fw_triads[[1]])

Signifiers <- R6::R6Class("Signifiers",
                          public = list(
                            types_by_signifierid = NULL,
                            signifierids_by_type = NULL,
                            signifier_definitions = NULL,
                            types_with_signifiers = NULL,
                            parent_framework = NULL,
                            linked_frameworks = NULL,
                            types_by_signifierid_parent = NULL,
                            signifierids_by_type_parent = NULL,
                            types_with_signifiers_parent = NULL,
                            signifier_counts_linked_frameworks_type = NULL,
                            types_by_signifierid_framework = NULL,
                            signifierids_by_type_framework = NULL,
                            types_with_signifiers_framework = NULL,
                            supported_signifier_types = c("triad", "dyad", "list", "stones", "freetext", "imageselect", "photo", "audio", "uniqueid"),

                            #' @description
                            #' Create a new `signifiers` object.
                            #' #' @details
                            #' The json file is parsed into various data structures returned as a set of R6 classes. Package methods provide the R
                            #' programmer with a toolset to work with the SenseMaker® framework capture data.
                            #' @param jsonfilename if using a json file stored locally, the path and file name of the json file to load.
                            #' @param parsedjson if using a json file previously loaded and parsed, the parsed file.
                            #' @param workbenchid if using the platform security, the id for the workbench/dashboard.
                            #' @param token if using the platform securithy, the token to gain access to the json definition.
                            #' @return A new `signifier` R6 class object and fields type by signifier id, signifier ids by type, and
                            #'           types with signifiers.
                            initialize = function(jsonfilename, parsedjson = NULL, workbenchid = NULL,
                                                  token = NULL) {
                              sensemakerframework <- private$unpackjson(self, parsedjson, jsonfilename, workbenchid, token)

                            },
                            #-----------------------------------------------------------------
                            # Generic Helper Functions
                            #-----------------------------------------------------------------
                            #' @description
                            #' Get the signifier types used within the passed framework definition
                            #' @return
                            #' A vector of the signifier types used within the passed framework definition.
                            get_used_signifier_types = function() {
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
                            #' Get the signifier types defined to the SenseMaker® system - enables an R programmer
                            #' to program over the types.
                            #' @return
                            #' A vector of the signifier types defined to the SenseMaker® system - these are:
                            #' "triad" "dyad"  "list"  "stones" "freetext" "imageselect" "photo" "audio" "uniqueid"
                            get_all_supported_signifier_types = function(){
                              return(self$supported_signifier_types)
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
                            #' @return A vector of the signifier ids in the framework definition for the passed type.
                            get_signifier_ids_by_type = function(type) {
                              if (length(self$signifierids_by_type[[type]]) == 0) {return(NULL)}
                              return(self$signifierids_by_type[[type]])
                            },
                            #-----------------------------------------------------------------
                            # General all Signifier Helper Functions
                            #-----------------------------------------------------------------
                            #' @description
                            #' Get the number of signifiers (count) for a signifier type.
                            #' @param type The signifier type.
                            #' @return An integer of the number of signifiers defined for the type.
                            get_signifier_count_by_type = function(type) {
                              return(length(self$get_signifier_ids_by_type(type)))
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
                            #' Get the signifier title for the passed in signifier id.
                            #' @param id The signifier id whose type to retrieve.
                            #' @return A string with the title of the passed in signifier id.
                            get_signifier_title = function(id) {
                              return(self$get_signifier_by_id_R6(id)[["title"]])
                            },
                            #' @description
                            #' Get the signifier tooltip for the passed in signifier id.
                            #' @param id The signifier id whose type to retrieve.
                            #' @return A string with the tooltip of the passed in signifier id.
                            get_signifier_tooltip = function(id) {
                              return(self$get_signifier_by_id_R6(id)[["tooltip"]])
                            },
                            #' @description
                            #' Get the signifier type of the passed in signifier id.
                            #' @param id The signifier id whose type to retrieve.
                            #' @return A string with the type of the passed in signifier id.
                            get_signifier_type = function(id) {
                              return(self$get_signifier_by_id_R6(id)[["type"]])
                            },
                            #' @description
                            #' Get the allow N/a property of the passed in signifier.
                            #' @param id The signifier id whose type to retrieve.
                            #' @return A boolean TRUE if the framework allows the N/A selected for the passed in signifier id, FALSE if not.
                            get_signifier_allow_na = function(id) {
                              return(self$get_signifier_by_id_R6(id)[["allow_na"]])
                            },
                            #' @description
                            #' Get the allow is fragment property of the passed in signifier.
                            #' @param id The signifier id whose type to retrieve.
                            #' @return A boolean TRUE if the passed in signifier id is a fragment, FALSE if not.
                            get_signifier_fragment = function(id) {
                              return(self$get_signifier_by_id_R6(id)[["fragment"]])
                            },
                            #' @description
                            #' Get the is required property of the passed in signifier.
                            #' @param id The signifier id whose type to retrieve.
                            #' @return A boolean TRUE is the passed in signifier id is a mandatory entry for the respondent, FALSE if not.
                            get_signifier_required = function(id) {
                              return(self$get_signifier_by_id_R6(id)[["required"]])
                            },
                            #' @description
                            #' Get the is sticky property of the passed in signifier.
                            #' @param id The signifier id whose type to retrieve.
                            #' @return A boolean TRUE is the passed in signifier id is a demographic one (sticky), FALSE if not.
                            get_signifier_sticky = function(id) {
                              return(self$get_signifier_by_id_R6(id)[["sticky"]])
                            },
                            #' @description
                            #'  Get the framework definition content R6 class for the passed signifier id.
                            #' @param id The signifier id whose type to retrieve.
                            #' @return The R6 class for the content portion of the passed in signifier id.
                            get_signifier_content_R6 = function(id) {
                              return(self$get_signifier_by_id_R6(id)[["content"]])
                            },
                            #' @description
                            #' Get whether the signifier is currently included in the capture using this framework definition.
                            #' @param id The signifier id whose type to retrieve.
                            #' @return A boolean TRUE if the passed in signifier id is currently used in collector capture, FALSE if not.
                            get_signifier_include = function(id) {
                              return(self$get_signifier_by_id_R6(id)[["include"]])
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
                              self$signifier_definitions[[type]][[id]][[property]] <- value
                              invisible(self)
                            },
                            #' @description
                            #' Change signifier title value
                            #' @param id The signifier id.
                            #' @param value The new value.
                            #' @return invisible self
                            change_signifier_title = function(id, value) {
                              self$change_signifier_property_value(id, value, "title")
                              invisible(self)
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
                              return(do.call(eval(parse(text = paste0("self$get_", type, "_anchor", ifelse(anchor_id == "", "", "_by_id")))), list(id, ifelse(anchor_id == "", anchor, anchor_id)))[[field]])
                            },
                            #' @description
                            #' Get the anchor ids for the triad or dyad specified by the id.
                            #' @param id The signifier id to retrieve anchor ids.
                            #' @param delist. Default FALSE If TRUE return the list of ids as an unnamed vector otherwise as a named list ("top"/triads and "left" and "right" for dyads/triads)
                            #' @param type. Default "". Optional. The type of signifier, "triad" or "dyad". If blank, type determined by lookup.
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
                            #' @param type. Default "". Optional. The type of signifier, "triad" or "dyad". If blank, type determined by lookup.
                            #' @return A character vector of the data column names for the compositional values for the dyad or triad.
                            get_anchor_compositional_column_names = function(id, type = "") {
                              if (type == "") {
                                type <- self$get_signifier_type_by_id(id)
                              }
                              return(do.call(eval(parse(text = paste0("self$get_", type, "_compositional_column_names"))), list(id)))
                            },
                            #' @description
                            #' Get an anchor R6 class instance for the supplied triad or dyad anchor.
                            #' @param id The signifier id to retrieve anchor ids.
                            #' @param anchor_id Optional. Default "". The id of the anchor to be retrieved. One of anchor_id or anchor must be supplied.
                            #' @param anchor Optional, Default "". Optional. The anchor name to return. Values "top" (triad), "left", "right" (triad and dyad)
                            #' @param type. Default "". Optional. The type of signifier, "triad" or "dyad". If blank, type determined by lookup.
                            #' @return An R6 class instance of the anchor requested.
                            get_anchor_R6 = function(id, anchor_id = "", anchor = "", type = "") {
                              if (type == "") {
                                type <- self$get_signifier_type_by_id(id)
                              }
                              return(do.call(eval(parse(text = paste0("self$get_", type, "_anchor", ifelse(anchor_id == "", "", "_by_id")))), list(id, ifelse(anchor_id == "", anchor, anchor_id))))
                            },
                            #' @description
                            #' Get a list of list item ids for passed list signifier id. Currently only lists supported but expected imageselect and others to follow.
                            #' @param id The signifier id to retrieve anchor ids.
                            #' @param type. Default "". Optional. The type of signifier, "list" only allowable value. If not provided, value obtained via lookup.
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
                            #' Get parent name
                            #' @return A character string of the parent fraework id
                            get_parent_name = function() {
                              return(self$parent_framework)
                            },
                            #' @description
                            #' Get parent framework signifier ids by type
                            #' @param type The signifier type.
                            #' @return A vector of signifier ids.
                            get_parent_framework_ids_by_type = function(type) {
                              return(self$signifierids_by_type_parent[[type]])
                            },
                            #' @description
                            #' Get parent framework triad signifier ids
                            #' @return A vector of triad signifier ids.
                            get_parent_framework_triad_ids = function() {
                              return(self$signifierids_by_type_parent[["triad"]])
                            },
                            #' @description
                            #' Get parent framework dyad signifier ids
                            #' @return A vector of dyad signifier ids.
                            get_parent_framework_dyad_ids = function() {
                              return(self$signifierids_by_type_parent[["dyad"]])
                            },
                            #' @description
                            #' Get parent framework list signifier ids
                            #' @return A vector of list signifier ids.
                            get_parent_framework_list_ids = function() {
                              return(self$signifierids_by_type_parent[["list"]])
                            },
                            #' @description
                            #' Get parent framework stones signifier ids
                            #' @return A vector of stones signifier ids.
                            get_parent_framework_stones_ids = function() {
                              return(self$signifierids_by_type_parent[["stones"]])
                            },
                            #' @description
                            #' Get parent framework freetext signifier ids
                            #' @return A vector of freetext signifier ids.
                            get_parent_framework_freetext_ids = function() {
                              return(self$signifierids_by_type_parent[["freetext"]])
                            },
                            #' @description
                            #' Get parent framework imageselect signifier ids
                            #' @return A vector of imageselect signifier ids.
                            get_parent_framework_imageselect_ids = function() {
                              return(self$signifierids_by_type_parent[["imageselect"]])
                            },
                            #' @description
                            #' Get parent framework audio signifier ids
                            #' @return A vector of audio signifier ids.
                            get_parent_framework_audio_ids = function() {
                              return(self$signifierids_by_type_parent[["audio"]])
                            },
                            #' @description
                            #' Get parent framework photo signifier ids
                            #' @return A vector of photo signifier ids.
                            get_parent_framework_photo_ids = function() {
                              return(self$signifierids_by_type_parent[["photo"]])
                            },
                            #' @description
                            #' Get parent framework uniqueid signifier ids
                            #' @return A vector of uniqueid signifier ids.(should only return one)
                            get_parent_framework_uniqueid_ids = function() {
                              return(self$signifierids_by_type_parent[["uniqueid"]])
                            },
                            #' @description
                            #' Get parent framework type by signifier id
                            #' @param sig_id The signifier id
                            #' @return A character string of the signifier type.
                            get_parent_framework_type_by_signifierid = function(sig_id) {
                              return(self$types_by_signifierid_parent[[sig_id]])
                            },
                            #' @description
                            #' Get parent framework signifier count by type
                            #' @param type The signifier type
                            #' @return An integer with the signifier count.
                            get_parent_framework_count_by_type = function(fw_id, type) {
                              return(self$signifier_counts_linked_frameworks_type[[fw_id]][[type]])
                            },
                            #' @description
                            #' Get used signifier types by framework
                            #' @param fw_id The linked framework id.
                            #' @return A vector with the signifier types used in the linked framework.
                            get_parent_framework_used_signifier_types = function() {
                              return(self$types_with_signifiers_parent)
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
                            remove_signifier_definition = function(tid, ttype = "") {
                              if (ttype == "") {ttype <- self$get_signifier_type_by_id(tid)}
                              private$remove_signifier_reference(tid, ttype)
                              invisible(self)

                            },
                            #-----------------------------------------------------------------
                            # linked Framework Helper Functions
                            # These functions enable processing of the linked frameworks and
                            # identifiction of their signifiers and signifier types.
                            # These are not used for the full array of helper functions - to
                            # use the full helper functions use the linked framework helper functions
                            # to give you an id or set of ids.
                            #-----------------------------------------------------------------
                            #' @description
                            #' Get linked framework count
                            #' @return An integer of the number of linked frameworks
                            get_linked_framework_count = function() {
                              return(length(self$linked_frameworks))
                            },
                            #' @description
                            #' Get linked framework parent id
                            #' @return A character string of the parent fraework id
                            get_linked_parent_framework_id = function() {
                              return(names(self$parent_framework))
                            },
                            get_linked_parent_framework_name = function() {
                              return(unname(unlist(self$parent_framework)))
                            },
                            #' @description
                            #' Get linked framework ids
                            #' @return A vector of linked framework ids
                            get_linked_framework_ids = function() {
                              return(names(self$linked_frameworks))
                            },
                            #' @description
                            #' Get linked framework names
                            #' @return A vector of linked framework names
                            get_linked_framework_names = function() {
                              return(unname(unlist((self$linked_frameworks))))
                            },
                            #' @description
                            #' Get linked framework list
                            #' @return A named list of linked frameworks- names ids, values names.
                            get_linked_framework_list = function() {
                              return(self$linked_frameworks)
                            },
                            #' @description
                            #' Get linked framework list widget.
                            #' @return A named list of linked frameworks - ids ids, names names.
                            get_linked_framework_mcq_list = function() {
                              ret_list <- self$get_linked_framework_ids()
                              names(ret_list) <- self$get_linked_framework_names()
                              return(ret_list)
                            },
                            #' @description
                            #' Get linked framework signifier ids by type
                            #' @param fw_id The linked framework id.
                            #' @param type The signifier type.
                            #' @return A vector of signifier ids.
                            get_linked_framework_ids_by_type = function(fw_id, type) {
                              return(self$signifierids_by_type_framework[[fw_id]][[type]])
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
                            #' Get linked framework uniqueid signifier ids
                            #' @param fw_id The linked framework id.
                            #' @return A vector of uniqueid signifier ids.(should only return one)
                            get_linked_framework_uniqueid_ids = function(fw_id) {
                              return(self$signifierids_by_type_framework[[fw_id]][["uniqueid"]])
                            },
                            #' @description
                            #' Get linked framework type by signifier id
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
                              return(self$signifier_counts_linked_frameworks_type[[fw_id]][[type]])
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
                              my_ret <- names(self$signifier_definitions[["list"]] %>%
                                                purrr::keep(names(.) %in% self$get_linked_framework_list_ids(fw_id)) %>%
                                                private$get_max_responses() %>%
                                                purrr::keep((.) > 1))
                              if (length(my_ret) == 0) {
                                return(NULL)
                              } else {return(my_ret)}
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
                            #' @return A vector of the framework list ids
                            get_list_ids = function() {
                              return(self$get_signifier_ids_by_type("list"))
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
                            #' Get the other text value for passed list.
                            #' @param id The signifier id of the list whose other text is to be retrieved.
                            #' @return A positive integer value of the minimum number of allowable selections for the list signifier id passed in.
                            get_list_other_text = function(id) {
                              sig_text <- self$get_signifier_content_R6(id)[["other_text"]]
                              if (length(sig_text) == 0) return(NULL)
                              return(sig_text)
                            },
                            #' @description
                            #' Get the other signifier id for passed list.
                            #' @param id The signifier id of the list whose other signifier id is to be retrieved.
                            #' @return A text value of the signifier id for the other texbox.
                            get_list_other_signifier_id = function(id) {
                              sig_id <- self$get_signifier_content_R6(id)[["other_signifier_id"]]
                              if (length(sig_id) == 0) {return(NULL)}
                              return(sig_id)
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
                            #' @description
                            #' Get the signifier ids for all multiple select select lists
                            #' @return A vector of signifier ids.
                            get_multi_select_list_ids = function() {
                              my_ret <- names(self$signifier_definitions[["list"]] %>%
                                                private$get_max_responses() %>%
                                                purrr::keep((.) > 1))
                              if (length(my_ret) == 0) {
                                return(NULL)
                              } else {return(my_ret)}
                            },
                            #' @description
                            #' Update the list content properties
                            #' @param id the signifier id
                            #' @param property the property to update (valid values "max_responses", "min_responses", "other_signifier_id", "other_text", "random_order")
                            #' @param value the new value.
                            #' @return invisible self
                            update_list_content_property = function(id, property, value) {
                              # property must belong to a list
                              assertive::assert_is_identical_to_true(all(property %in% c("max_responses", "min_responses", "other_signifier_id", "other_text", "random_order")), severity = "stop")
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
                            #' @param id the signifier id
                            #' @param value the new value.
                            #' @return invisible self
                            update_list_content_item_title = function(sig_id, item_id, value) {
                              # property must belong to a list
                              assertive::assert_is_identical_to_true(all(property %in% c("image", "title", "tooltip", "visible", "other_signifier_id")), severity = "stop")
                              self$signifier_definitions[["list"]][[sig_id]][["content"]][["items"]][[item_id]][[property]] <- value
                              invisible(self)
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
                            #' @return A vector of the framework triad ids
                            get_triad_ids = function() {
                              return(self$get_signifier_ids_by_type("triad"))
                            },
                            #' @description
                            #' Get the triad labels
                            #' @param sig_id The signifier id of the list whose list item image URL to be returned.
                            #' @param item_id The signifier item id of the list whose list item item image URL to be returned.
                            #' @return A Boolean indicating whether the anchor is visible.
                            get_triad_labels_R6 = function(id) {
                              return(self$get_signifier_content_R6(id)$labels)
                            },
                            #' @description
                            #' Get the pointer image of the passed triad.
                            #' @param sig_id The signifier id of the list whose tryad item pointer image to be returned.
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
                            #' @param sig_id The triad id.
                            #' @return An R6 class instasnce of the top triad anchor for the passed triad id.
                            get_triad_top_anchor_R6 = function(id) {
                              return(self$get_triad_anchor_R6(id, "top"))
                            },
                            #' @description
                            #' Get the right triad anchor R6 object instance for passed triad signifier id.
                            #' @param sig_id The triad id.
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
                            #' @param sig_id The triad id.
                            #' @param anchorThe anchor ("top", "left", "right")
                            #' @return Character string containing the triad anchor text.
                            get_triad_anchor_text_by_anchor = function(id, anchor) {
                              return(self$get_triad_anchor_R6(id, anchor)$text)
                            },
                            #' @description
                            #' Get triad anchor show image for passed in triad id and anchor.
                            #' @param sig_id The triad id.
                            #' @param anchor The anchor ("top", "left", "right")
                            #' @return Boolean containing the triad anchor show image
                            get_triad_anchor_show_image_by_anchor = function(id, anchor) {
                              return(self$get_triad_anchor_R6(id, anchor)$show_image)
                            },
                            #' @description
                            #' Get triad anchor image URL for passed in triad id and anchor.
                            #' @param sig_id The triad id.
                            #' @param anchor The anchor ("top", "left", "right")
                            #' @return String containing the triad image URL
                            get_triad_anchor_image_by_anchor = function(id, anchor) {
                              return(self$get_triad_anchor_R6(id, anchor)$image)
                            },
                            #' @description
                            #' Get triad anchor show label for passed in triad id and anchor.
                            #' @param sig_id The triad id.
                            #' @param anchor The anchor ("top", "left", "right")
                            #' @return Boolean containing the triad anchor show label
                            get_triad_anchor_show_label_by_anchor = function(id, anchor) {
                              return(self$get_triad_anchor_R6(id, anchor)$show_label)
                            },
                            #' @description
                            #' Get triad top anchor text for passed in triad id.
                            #' @param sig_id The triad id.
                            #' @return Character string containing the triad anchor text.
                            get_triad_top_anchor_text = function(id) {
                              return(self$get_triad_anchor_R6(id, "top")$text)
                            },
                            #' @description
                            #' Get triad top anchor show image for passed in triad id.
                            #' @param sig_id The triad id.
                            #' @return Boolean containing the triad show image.
                            get_triad_top_anchor_show_image = function(id) {
                              return(self$get_triad_anchor_R6(id, "top")$show_image)
                            },
                            #' @description
                            #' Get triad top anchor image for passed in triad id.
                            #' @param sig_id The triad id.
                            #' @return Character string containing the triad image URL.
                            get_triad_top_anchor_image = function(id) {
                              return(self$get_triad_anchor_R6(id, "top")$image)
                            },
                            #' @description
                            #' Get triad top anchor show label for passed in triad id.
                            #' @param sig_id The triad id.
                            #' @return Boolean containing the triad show label
                            get_triad_top_anchor_show_label = function(id) {
                              return(self$get_triad_anchor_R6(id, "top")$show_label)
                            },
                            #' @description
                            #' Get triad left anchor text for passed in triad id.
                            #' @param sig_id The triad id.
                            #' @return Character string containing the triad  left anchor text.
                            get_triad_left_anchor_text = function(id) {
                              return(self$get_triad_anchor_R6(id, "left")$text)
                            },
                            #' @description
                            #' Get triad left anchor show image for passed in triad id.
                            #' @param sig_id The triad id.
                            #' @return Boolean containing the triad show image.
                            get_triad_left_anchor_show_image = function(id) {
                              return(self$get_triad_anchor_R6(id, "left")$show_image)
                            },
                            #' @description
                            #' Get triad left anchor image for passed in triad id.
                            #' @param sig_id The triad id.
                            #' @return Character string containing the triad image URL
                            get_triad_left_anchor_image = function(id) {
                              return(self$get_triad_anchor_R6(id, "left")$image)
                            },
                            #' @description
                            #' Get triad left anchor show label for passed in triad id.
                            #' @param sig_id The triad id.
                            #' @return Boolean containing the triad show label
                            get_triad_left_anchor_show_label = function(id) {
                              return(self$get_triad_anchor_R6(id, "left")$show_label)
                            },
                            #' @description
                            #' Get triad right anchor text for passed in triad id.
                            #' @param sig_id The triad id.
                            #' @return Character string containing the triad  left anchor text.
                            get_triad_right_anchor_text = function(id) {
                              return(self$get_triad_anchor_R6(id, "right")$text)
                            },
                            #' @description
                            #' Get triad right anchor show image for passed in triad id.
                            #' @param sig_id The triad id.
                            #' @return Boolean containing the triad show image.
                            get_triad_right_anchor_show_image = function(id) {
                              return(self$get_triad_anchor_R6(id, "right")$show_image)
                            },
                            #' @description
                            #' Get triad right anchor show image for passed in triad id.
                            #' @param sig_id The triad id.
                            #' @return Boolean containing the triad show image.
                            get_triad_right_anchor_image = function(id) {
                              return(self$get_triad_anchor_R6(id, "right")$image)
                            },
                            #' @description
                            #' Get triad left anchor show label for passed in triad id.
                            #' @param sig_id The triad id.
                            #' @return Boolean containing the triad show label
                            get_triad_right_anchor_show_label = function(id) {
                              return(self$get_triad_anchor_R6(id, "right")$show_label)
                            },
                            #' @description
                            #' Get triad top anchor id for passed in triad id.
                            #' @param sig_id The triad id.
                            #' @return String of the top anchor id
                            get_triad_top_anchor_id = function(id) {
                              return(self$get_triad_anchor_id(id, "top"))
                            },
                            #' @description
                            #' Get triad left anchor id for passed in triad id.
                            #' @param sig_id The triad id.
                            #' @return String of the left anchor id
                            get_triad_left_anchor_id = function(id) {
                              return(self$get_triad_anchor_id(id, "left"))
                            },
                            #' @description
                            #' Get triad right anchor id for passed in triad id.
                            #' @param sig_id The triad id.
                            #' @return String of the right anchor id
                            get_triad_right_anchor_id = function(id) {
                              return(self$get_triad_anchor_id(id, "right"))
                            },
                            #' @description
                            #' Get data column name for passed in triad id and column.
                            #' @param sig_id The triad id.
                            #' @param column The column to return. ("top", "left", "right", "x", "y", "X", "Y")
                            #' @return Character string of the column name
                            get_triad_column_name = function(id, column) {
                              return(paste0(id, ifelse(column %in% c("x", "y", "X", "Y"), "", "_"), ifelse(column %in% c("x", "y", "X", "Y"), toupper(column), self$get_triad_anchor_id(id, column))))
                            },
                            #' @description
                            #' Get data X column name for passed in triad id.
                            #' @param sig_id The triad id.
                            #' @return Character string of the X column name
                            get_triad_x_column_name = function(id) {
                              return(self$get_triad_column_name(id, "x"))
                            },
                            #' @description
                            #' Get data Y column name for passed in triad id.
                            #' @param sig_id The triad id.
                            #' @return Character string of the Y column name
                            get_triad_y_column_name = function(id) {
                              return(self$get_triad_column_name(id, "y"))
                            },
                            #' @description
                            #' Get data X and Y column name for passed in triad id.
                            #' @param sig_id The triad id.
                            #' @return List of X and Y
                            get_triad_x_y_column_names = function(id, delist = FALSE) {
                              col_names <- c(self$get_triad_column_name(id, "x"), self$get_triad_column_name(id, "y"))
                              if (delist) {return(col_names)}
                              names(col_names) <- c("x", "y")
                              return(col_names)
                            },

                            #' @description
                            #' Get data top column name for passed in triad id.
                            #' @param sig_id The triad id.
                            #' @return Character string of the top column name
                            get_triad_top_column_name = function(id) {
                              return(self$get_triad_column_name(id, "top"))
                            },
                            #' @description
                            #' Get data left column name for passed in triad id.
                            #' @param sig_id The triad id.
                            #' @return Character string of the left column name
                            get_triad_left_column_name = function(id, column) {
                              return(self$get_triad_column_name(id, "left"))
                            },
                            #' @description
                            #' Get data right column name for passed in triad id.
                            #' @param sig_id The triad id.
                            #' @return Character string of the right column name
                            get_triad_right_column_name = function(id, column) {
                              return(self$get_triad_column_name(id, "right"))
                            },
                            #' @description
                            #' Get data N/A column name for passed in triad id.
                            #' @param sig_id The triad id.
                            #' @return Character string of the N/A column name
                            get_triad_na_column_name = function(id, column) {
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
                            #' @return A vector of the framework dyad ids
                            get_dyad_ids = function() {
                              return(self$get_signifier_ids_by_type("dyad"))
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
                            #' @param sig_id The dyad id.
                            #' @param anchor_id Anchor ("left", "right").
                            #' @return Character string of dyad anchor text.
                            get_dyad_anchor_text_by_anchor = function(id, anchor) {
                              return(self$get_dyad_anchor_R6(id, anchor)$text)
                            },
                            #' @description
                            #' Get dyad anchor show image for dyad and anchor.
                            #' @param sig_id The dyad id.
                            #' @param anchor_id Anchor ("left", "right").
                            #' @return Boolean of dyad anchor show image
                            get_dyad_anchor_show_image_by_anchor = function(id, anchor) {
                              return(self$get_dyad_anchor_R6(id, anchor)$show_image)
                            },
                            #' @description
                            #' Get dyad anchor  image for dyad and anchor.
                            #' @param sig_id The dyad id.
                            #' @param anchor_id Anchor ("left", "right").
                            #' @return Character string of dyad anchor image URL
                            get_dyad_anchor_image_by_anchor = function(id, anchor) {
                              return(self$get_dyad_anchor_R6(id, anchor)$image)
                            },
                            #' @description
                            #' Get dyad anchor show label for dyad and anchor.
                            #' @param sig_id The dyad id.
                            #' @param anchor_id Anchor ("left", "right").
                            #' @return Boolean of dyad anchor show label
                            get_dyad_anchor_show_label_by_anchor = function(id, anchor) {
                              return(self$get_dyad_anchor_R6(id, anchor)$show_label)
                            },
                            #' @description
                            #' Get dyad left anchor text for dyad.
                            #' @param sig_id The dyad id.
                            #' @return Character string of dyad left anchor text
                            get_dyad_left_anchor_text = function(id) {
                              return(self$get_dyad_anchor_R6(id, "left")$text)
                            },
                            #' @description
                            #' Get dyad left anchor show image for dyad.
                            #' @param sig_id The dyad id.
                            #' @return Boolean of dyad left anchor show image
                            get_dyad_left_anchor_show_image = function(id) {
                              return(self$get_dyad_anchor_R6(id, "left")$show_image)
                            },
                            #' @description
                            #' Get dyad left anchor image for dyad.
                            #' @param sig_id The dyad id.
                            #' @return Character string of dyad left anchor image URL
                            get_dyad_left_anchor_image = function(id) {
                              return(self$get_dyad_anchor_R6(id, "left")$image)
                            },
                            #' @description
                            #' Get dyad left anchor show label for dyad.
                            #' @param sig_id The dyad id.
                            #' @return Boolean of dyad left anchor show label
                            get_dyad_left_anchor_show_label = function(id) {
                              return(self$get_dyad_anchor_R6(id, "left")$show_label)
                            },
                            #' @description
                            #' Get dyad right anchor text for dyad.
                            #' @param sig_id The dyad id.
                            #' @return Character string of dyad right anchor text
                            get_dyad_right_anchor_text = function(id) {
                              return(self$get_dyad_anchor_R6(id, "right")$text)
                            },
                            #' @description
                            #' Get dyad right anchor show image for dyad.
                            #' @param sig_id The dyad id.
                            #' @return Boolean of dyad right anchor show image
                            get_dyad_right_anchor_show_image = function(id) {
                              return(self$get_dyad_anchor_R6(id, "right")$show_image)
                            },
                            #' @description
                            #' Get dyad right anchor image for dyad.
                            #' @param sig_id The dyad id.
                            #' @return Character string of dyad right anchor image URL
                            get_dyad_right_anchor_image = function(id) {
                              return(self$get_dyad_anchor_R6(id, "right")$image)
                            },
                            #' @description
                            #' Get dyad right anchor show label for dyad.
                            #' @param sig_id The dyad id.
                            #' @return Boolean of dyad right anchor show label
                            get_dyad_right_anchor_show_label = function(id) {
                              return(self$get_dyad_anchor_R6(id, "right")$show_label)
                            },
                            #' @description
                            #' Get dyad left anchor id for dyad.
                            #' @param sig_id The dyad id.
                            #' @return Character string of dyad left anchor id
                            get_dyad_left_anchor_id = function(id) {
                              return(self$get_dyad_anchor_id(id, "left"))
                            },
                            #' @description
                            #' Get dyad right anchor id for dyad.
                            #' @param sig_id The dyad id.
                            #' @return Character string of dyad right anchor id
                            get_dyad_right_anchor_id = function(id) {
                              return(self$get_dyad_anchor_id(id, "right"))
                            },
                            #' @description
                            #' Get dyad anchor ids by dyad.
                            #' @param sig_id The dyad id.
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
                            get_dyad_left_column_name = function(id, column) {
                              return(self$get_dyad_column_name(id, "left"))
                            },
                            #' @description
                            #' Get dyad right column name for dyad.
                            #' @param id The dyad id.
                            #' @return Character string of the dyad right column name
                            get_dyad_right_column_name = function(id, column) {
                              return(self$get_dyad_column_name(id, "right"))
                            },
                            #' @description
                            #' Get dyad N/A column name for dyad.
                            #' @param id The dyad id.
                            #' @return Character string of the dyad N/A column name
                            get_dyad_na_column_name = function(id, column) {
                              if (self$get_signifier_allow_na(id)) {
                                return(paste0(id, "_NA"))
                              }
                              return(NULL)
                            },
                            #' @description
                            #' Get as percent column name for dyad. This is the x column as a value between 0 and 100
                            #' @param id The dyad id.
                            #' @return Character string of the dyad as percent column name
                            get_dyad_aspercent_x_column_name = function(id, column) {
                              return(paste0(id, "XR"))
                            },
                            #' @description
                            #' Get dyad compositional anchor column names by dyad (i.e. left and right).
                            #' @param sig_id The dyad id.
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
                            #' @param sig_id The dyad id.
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
                            #' @param sig_id The dyad id.
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
                            update_dyadd_right_label_value = function(id, value, property = "") {
                              if (property == "") {property <- "text"}
                              self$signifier_definitions[["dyad"]][[id]][["content"]][["labels"]][["right_anchor"]][[property]] <- value
                              invisible(self)
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
                            #' @return A vector of the framework stones ids
                            get_stones_ids = function() {
                              return(self$get_signifier_ids_by_type("stones"))
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
                            #' @return vectpr of stones stones ids.
                            get_stones_items_ids = function(id) {
                              return(names(self$get_stones_stones_R6(id)))
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
                              return(unlist(purrr::imap(myfw$get_stones_items_ids(id), private$append_stone_columns, id), recursive = TRUE))
                             # return(unlist(purrr::imap(myfw$get_stones_items_ids(id), eval(parse(text = "private$append_stone_columns")), id), recursive = TRUE))
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
                            #' @param id The stones id.
                            #' @param axis The stones axis ("x" or "y")
                            #' @param property The property to update (values "end_label, "start_label", "name")
                            #' @param value the updated value
                            #' @return invisible self
                            Update_stones_stone_property = function(sig_id, stone_id, property, value) {
                              self$signifier_definitions[["stones"]][[sig_id]][["content"]][["stones"]][[stone_id]][[property]] <- value
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
                            #' @return A vector of the framework freetext ids
                            get_freetext_ids = function() {
                              return(self$get_signifier_ids_by_type("freetext"))
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
                            #' @return A vector of the framework imageselect ids
                            get_imageselect_ids = function() {
                              return(self$get_signifier_ids_by_type("imageselect"))
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
                            #' @return A vector of the framework photo ids
                            get_photo_ids = function() {
                              return(self$get_signifier_ids_by_type("photo"))
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
                            #' @return A vector of the framework audio ids
                            get_audio_ids = function() {
                              return(self$get_signifier_ids_by_type("audio"))
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
                            #' @return A vector of the framework uniqueid ids
                            get_uniqueid_ids = function() {
                              return(self$get_signifier_ids_by_type("uniqueid"))
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
                            #' @param id - the freetext signifier id - if blank or NULL, id is calculated automatically
                            #' @return self
                            add_freetext = function(title, tooltip, allow_na, fragment, required, sticky, multiline, include = TRUE, default = "", id = "") {
                              # get the signifier definition entry being processed
                              if (id == "") {
                                id <- uuid::UUIDgenerate(use.time = FALSE, n = 1)
                              }
                              # Add the signifier definition
                              content <- private$freetext_content_definition_R6()$new(default = default, multiline = multiline)
                              definition <- private$signifier_definition_R6()$new(id = id, type = "freetext", title = title, tooltip = tooltip, allow_na = allow_na,
                                                                                  fragment = fragment, required = required, sticky = sticky, content = content, include = TRUE)
                              add_list <- list(definition)
                              names(add_list) <- id
                              self$signifier_definitions[["freetext"]] <- append(self$signifier_definitions[["freetext"]], add_list)
                              # Ripple update to the other fields
                              private$ripple_update(id, "freetext")
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
                            #' @param id - the imageselect signifier id - if blank or NULL, id is calculated automatically
                            #' @return self
                            add_imageselect = function(title, tooltip, allow_na, fragment, required, sticky, items, id = "") {
                              # get the signifier definition entry being processed
                              if (id == "") {
                                id <- uuid::UUIDgenerate(use.time = FALSE, n = 1)
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
                              private$ripple_update(id, "imageselect")
                              return(id)
                            },
                            #' @description
                            #' Add an audio signifier definition to the parent definition
                            #' @param title - the audio signifier title
                            #' @param tooltip - the audio signifier tooltip
                            #' @param allow_na - whether the audio signifier allows N/A
                            #' @param fragment - whether the audio signifier is a fragment entry
                            #' @param required - whether the audio signifier is mandatory
                            #' @param sticky - whether the audio signifier is a sticky
                            #' @param id - the audio signifier id - if blank or NULL, id is calculated automatically
                            #' @return self
                            add_audio = function(title, tooltip, allow_na, fragment, required, sticky, id = "") {
                              # get the signifier definition entry being processed
                              if (id == "") {
                                id <- uuid::UUIDgenerate(use.time = FALSE, n = 1)
                              }
                              definition <- private$signifier_definition_R6()$new(id = id, type = "audio", title = title, tooltip = tooltip, allow_na = allow_na,
                                                                                  fragment = fragment, required = required, sticky = sticky, content = NULL, include = TRUE)
                              add_list <- list(definition)
                              names(add_list) <- id
                              self$signifier_definitions[["audio"]] <- append(self$signifier_definitions[["audio"]], add_list)
                              # Ripple update to the other fields
                              private$ripple_update(id, "audio")
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
                            #' @param id - the photo signifier id - if blank or NULL, id is calculated automatically
                            #' @return self
                            add_photo = function(title, tooltip, allow_na, fragment, required, sticky, id = "") {
                              # get the signifier definition entry being processed
                              if (id == "") {
                                id <- uuid::UUIDgenerate(use.time = FALSE, n = 1)
                              }
                              definition <- private$signifier_definition_R6()$new(id = id, type = "photo", title = title, tooltip = tooltip, allow_na = allow_na,
                                                                                  fragment = fragment, required = required, sticky = sticky, content = NULL, include = TRUE)
                              add_list <- list(definition)
                              names(add_list) <- id
                              self$signifier_definitions[["photo"]] <- append(self$signifier_definitions[["photo"]], add_list)
                              # Ripple update to the other fields
                              private$ripple_update(id, "photo")
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
                            #' @param id - the list signifier id - if blank or NULL, id is calculated automatically
                            #' @return self
                            add_list = function(title, tooltip, allow_na, fragment, required, sticky, items, dynamic, random_order, max_responses, min_responses, other_signifier_id, other_text, id = "") {
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
                              result_item <- vector("list", length = nrow(items))
                              names(result_item) <- items$id
                              list_items <-  purrr::imap(result_item, private$build_list_item , items)
                              print(list_items)
                              content <- private$item_content_definition_R6()$new(items = list_items, num_items = length(list_items), dynamic = dynamic, random_order = random_order, max_responses = max_responses, min_responses = min_responses,  other_signifier_id = other_signifier_id,  other_text = other_text)
                              definition <- private$signifier_definition_R6()$new(id = id, type = "list", title = title, tooltip = tooltip, allow_na = allow_na,
                                                                                  fragment = fragment, required = required, sticky = sticky, content = content, include = TRUE)
                              add_list <- list(definition)
                              names(add_list) <- id
                              self$signifier_definitions[["list"]] <- append(self$signifier_definitions[["list"]], add_list)
                              # Ripple update to the other fields
                              private$ripple_update(id, "list")
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
                            #' @param id - the list signifier id - if blank or NULL, id is calculated automatically
                            #' @return self
                            add_triad = function(title, tooltip, allow_na, fragment, required, sticky, labels, pointer_image, background_image, id = "") {
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
                              private$ripple_update(id, "triad")
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
                            #' @param id - the list signifier id - if blank or NULL, id is calculated automatically
                            #' @return self
                            add_dyad = function(title, tooltip, allow_na, fragment, required, sticky, labels, pointer_image, background_image, id = "") {
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
                              private$ripple_update(id, "dyad")
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
                            #' @param id - the list signifier id - if blank or NULL, id is calculated automatically
                            #' @return self
                            add_stones = function(title, tooltip, allow_na, fragment, required, sticky, stones, background_image, x_name, x_end_label, x_start_label, y_name, y_end_label, y_start_label, id = "") {
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
                              private$ripple_update(id, "stones")
                              return(id)
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
                            unpackjson = function(tself, tparsedjson, tjsonfile, tworkbenchid, ttoken) {
                              # Whatever input received, return json parsed with jsonlite from_json
                              # Think about recursive structures - we need multi-deep and keeping track of relationships
                              
                              json_parsed <- private$processjson(tparsedjson, tjsonfile, tworkbenchid, ttoken)
                              # ======================== Populate linked framework fields =======================
                              parent_list <- list(json_parsed$name)
                              names(parent_list) <- json_parsed$id
                              tself$parent_framework <- parent_list
                              linked_framework_list <- json_parsed$linked_frameworks$framework$name
                              names(linked_framework_list) <- json_parsed$linked_frameworks$framework$id
                              self$linked_frameworks <- as.list(linked_framework_list)
                              self$signifier_counts_linked_frameworks_type <- private$get_sig_counts_type_linked(tself, json_parsed)
                              # get the ids by type by framework
                              self$signifierids_by_type_framework <- private$get_sig_ids_linked(tself, json_parsed, linked_framework_list)
                              # get the types by signifier id by framework
                              types_by_signifierid_framework <- vector("list", length =length(linked_framework_list))
                              names(types_by_signifierid_framework) <- json_parsed$linked_frameworks$framework$id
                              self$types_by_signifierid_framework <- purrr::imap(types_by_signifierid_framework, private$get_ids_type, json_parsed$linked_frameworks$framework)
                              # get the signifers used by each linked framework
                              types_used_framework <- vector("list", length =length(linked_framework_list))
                              names(types_used_framework) <- json_parsed$linked_frameworks$framework$id
                              self$types_with_signifiers_framework <- purrr::imap(types_used_framework, private$get_types_used_framework, self$signifierids_by_type_framework)
                              # Parent signifiers
                              my_json_parent <- dplyr::bind_rows(json_parsed[["sections"]][["signifiers"]])
                              sig_list_parent <- vector("list", length = length(parent_list))
                              names(sig_list_parent) <- names(parent_list)
                              type_list <- vector("list", length = length(self$supported_signifier_types))
                              names(type_list) <- self$supported_signifier_types
                              self$signifierids_by_type_parent <- purrr::imap(type_list, private$count_ids_type, my_json_parent)
                              self$signifier_counts_linked_frameworks_type <- private$get_sig_counts(self, my_json_parent$type)
                              parent_sigs <- my_json_parent[,c('id', "type")] %>% dplyr::filter(type %in%  self$supported_signifier_types)
                              parent_sigs_list <- as.list(parent_sigs[["type"]])
                              names(parent_sigs_list) <- as.list(parent_sigs[["id"]])
                              self$types_by_signifierid_parent <- parent_sigs_list
                              self$types_with_signifiers_parent <- unique(parent_sigs[["type"]])
                               # ================ Populate full definition fields =======================
                              # Extract the signifiers from the json
                              # ToDo - this still does not to recursive extraction of linked frameworks below the second level - create a test project and check how this is represented and complete
                              signifier_dataframe <- dplyr::bind_rows(dplyr::bind_rows(json_parsed[["sections"]][["signifiers"]]), dplyr::bind_rows(lapply(json_parsed$linked_frameworks$framework$sections, function(x) {x$signifiers})))
                              # Get the counts of the number of occurrences of each signifier type
                              sig_type_counts <- private$get_sig_counts(tself, signifier_dataframe$type)
                              # The list whose names will be the signifier id and values the signifier type
                              types_by_signifierid <- vector("list", length = colSums(sig_type_counts[,"n"]))
                              names(types_by_signifierid) <- NA
                              # The list whose names will be the signifier types and the values a list of signifier ids for each type
                              signifierids_by_type <- purrr::map(sig_type_counts[["n"]], function(x) {vector("list", length = x)})
                              names(signifierids_by_type) <- sig_type_counts[["types"]]
                              # The list containing list of R6 class instances of each signifier type
                              signifier_R6_definitions <- as.list(unlist(sig_type_counts[["n"]]))
                              names(signifier_R6_definitions) <- unlist(sig_type_counts[["types"]])
                              # Populate the list of list of R6 signifier class instances by processing the signifier_dataframe data frame built from the json
                              signifier_R6_definitions <- purrr::imap(signifier_R6_definitions, private$buildDefinitions_by_type, signifier_dataframe)
                              #signifier_R6_definitions <- purrr::imap(signifier_R6_definitions, eval(parse(text = "private$buildDefinitions_by_type")), signifier_dataframe)
                              # Signifier Ids by type
                              signifierids_by_type <- purrr::imap(signifierids_by_type, private$build_signifier_by_type, signifier_R6_definitions)
                              #signifierids_by_type <- purrr::imap(signifierids_by_type, eval(parse(text = "private$build_signifier_by_type")), signifier_R6_definitions)
                              # Types by signifier IDs
                              types_by_signifierid <- private$build_types_by_signifierid(types_by_signifierid, signifierids_by_type)
                              # Types that have signifiers designed in this definition
                              types_with_signifiers <- names(which(sapply(purrr::map(signifierids_by_type, ~{!is.null(.x)}), rlang::is_true)))
                              # Return list of above lists/vectors
                              self$signifier_definitions <- signifier_R6_definitions
                              self$signifierids_by_type <- signifierids_by_type
                              self$types_by_signifierid <- types_by_signifierid
                              self$types_with_signifiers <- types_with_signifiers
                              return(TRUE)
                              #return(list(signifier_definitions = signifier_R6_definitions, signifierids_by_type = signifierids_by_type, types_by_signifierid = types_by_signifierid, types_with_signifiers = types_with_signifiers))
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
                            # Get the signifier counts for each type - i.e. the number of occurrences in the passed json for each type included in the json.
                            get_sig_counts = function(tself, ttypes) {
                              # create tible of types available
                              sig_types <- tibble::tibble(types = tself$supported_signifier_types)
                              # count the instances of types passed
                              sig_used <- data.frame(types = ttypes) %>% dplyr::count(types)
                              # return a tible of only those used with counts.
                              result <- sig_types %>% dplyr::rowwise() %>% dplyr::mutate(n = private$matchEntry(types, sig_used))
                              return(result)
                            },
                           # sig_type_counts_linked <- private$get_sig_counts_type_linked(tself, json_parsed)
                           # get the signifier counts for each type for each linked project - sadly this has to
                           #  be a different function from main function as layouts different requiring different processing
                           get_sig_counts_type_linked = function(tself, tjson_parsed) {
                             linked_Frameworks <- tjson_parsed$linked_frameworks$framework$id
                             linked_frameworks_counts <- vector("list", length = length(linked_Frameworks))
                             names(linked_frameworks_counts) <- linked_Frameworks
                             res <- purrr::imap(linked_frameworks_counts, private$calculate_count, tjson_parsed$linked_frameworks$framework, tself)
                             return(res)
                           },
                           calculate_count = function(fw_counts, tlist, data, tself) {
                             df_row <- data %>% dplyr::filter(id == tlist)
                             counts <- private$get_sig_counts(tself, dplyr::bind_rows(lapply(df_row$sections, function(x) {x$signifiers}))$type)
                             count_list <- as.list(counts[["n"]])
                             names(count_list) <- counts[["types"]]
                             return(count_list)
                           },
                           get_sig_ids_linked = function(tself, tjson_parsed, tframework_list) {
                             sig_list_linked <- vector("list", length = length(tframework_list))
                             names(sig_list_linked) <- names(tframework_list)
                             return(purrr::imap(sig_list_linked, private$extract_ids, tjson_parsed$linked_frameworks$framework, tself))

                           },
                           extract_ids = function(fw_counts, tlist, data, tself) {
                             df_row <- data %>% dplyr::filter(id == tlist)
                             type_list <- vector("list", length = length(tself$supported_signifier_types))
                             names(type_list) <- tself$supported_signifier_types
                             sigs <- purrr::imap(type_list, private$count_ids_type, dplyr::bind_rows(lapply(df_row$sections, function(x) {x$signifiers})))
                             },
                           count_ids_type = function(fw_counts, tlist, data) {
                             df_row <- data %>% dplyr::filter(type == tlist)
                             if (nrow(df_row) >0) {
                               return(df_row[["id"]])
                             } else {
                               return("No Entry")
                             }
                           },
                           get_ids_type = function(fw_counts, tlist, data) {
                             df_row <- data %>% dplyr::filter(id == tlist)
                             signifiers <- dplyr::bind_rows(lapply(df_row$sections, function(x) {x$signifiers}))
                             ret_list <- as.list(signifiers[["type"]])
                             names(ret_list) <- signifiers[["id"]]
                             return(ret_list)

                           },
                           get_types_used_framework = function(fw_counts, tlist, data) {
                             df_row <- data[[tlist]]
                             return(names(which(sapply(purrr::map(df_row, ~{.x != "No Entry"}), function(x) {x[[1]] == TRUE}))))
                           },
                            #============================================================================================
                            # R6 Class Instance Builds
                            # Using composition not inheretence - follows the JSON very directly, thus triads has content
                            #  containing the labels of anchors with supplied properties. List content as lists of list item properties and so on.
                            #============================================================================================
                            # # Build the R6 class instancesd for the type passed in.
                            # parameters:
                            # n - not used, provided by the map function.
                            # ttype - the type being processed.
                            # signifier_dataframe - the data frame of signifier types extracted in unpackjson
                            # Return - list of R6 class instances of the signifier type passed.
                            buildDefinitions_by_type = function(n, ttype, signifier_dataframe) {
                              if (n > 0) {
                                # filter the data to the type being processed
                                type_definition <- signifier_dataframe %>% dplyr::filter(type == ttype)
                                result_list <- vector("list", length = nrow(type_definition))
                                names(result_list) <- type_definition$id
                                # call relevant signifier processing function to build the list - or string "No Entries" if signifier type containing no entries.
                                ret_value <- purrr::imap(result_list,  get(paste0("build_", ttype), envir = private), type_definition, ttype)
                              } else {
                                ret_value <- "No Entries"
                              }
                              return(ret_value)
                            },
                            # Return the signifier IDs by type
                            build_signifier_by_type = function(n, ttype, signifier_definitions) {
                              return(names(signifier_definitions[[ttype]]))
                            },
                            # Return the type for each signifier id
                            # ToDo - find a way to vectorise this function.
                            build_types_by_signifierid = function(inlist, signifiers_by_type_list) {
                              k <- 0
                              for (type in names(signifiers_by_type_list)) {
                                for (sig_id in signifiers_by_type_list[[type]]) {
                                  if (length(sig_id) != 0) {
                                    k <- k + 1
                                    names(inlist)[[k]] <- sig_id
                                    inlist[[k]] <- type
                                  }
                                }
                              }
                              return(inlist)
                            },
                            #
                            append_stone_columns = function(items, n, stone) {
                              return(unlist(list(paste0(stone, "_", items, "XRight"), paste0(stone, "_", items, "YTop"))))
                            },
                            #
                            matchEntry = function(type,  lookuplist){
                              val <- lookuplist %>% dplyr::filter(types == type) %>% dplyr::select("n") %>% unlist(use.names = FALSE)
                              return(ifelse(length(val) == 0, 0, val))
                            },
                            #
                            build_triad = function(x, tid, df, type) {
                              # get the signifier definition entry being processed
                              df <- df %>% dplyr::filter(id == tid)
                              anchors <- df[["content"]][["labels"]][[1]]
                              top_anchor  <-  private$label_definition_R6()$new(id = anchors[1,"id"], text =
                                                                                  anchors[1,"text"], show_image = ifelse("show_image" %in%
                                                                                                                           colnames(anchors), anchors[1, "show_image"], TRUE),
                                                                                show_label = ifelse("show_label" %in%
                                                                                                      colnames(anchors), anchors[1, "show_label"], TRUE),
                                                                                image = ifelse("image" %in%
                                                                                                 colnames(anchors), anchors[1, "image"], ""))

                              left_anchor  <-  private$label_definition_R6()$new(id = anchors[2,"id"], text =
                                                                                   anchors[2,"text"], show_image = ifelse("show_image" %in%
                                                                                                                            colnames(anchors), anchors[2, "show_image"], TRUE),
                                                                                 show_label = ifelse("show_label" %in%
                                                                                                       colnames(anchors), anchors[2, "show_label"], TRUE),
                                                                                 image = ifelse("image" %in%
                                                                                                  colnames(anchors), anchors[2, "image"], ""))
                              right_anchor  <-   private$label_definition_R6()$new(id = anchors[3,"id"], text =
                                                                                     anchors[3,"text"], show_image = ifelse("show_image" %in%
                                                                                                                              colnames(anchors), anchors[3, "show_image"], TRUE),
                                                                                   show_label = ifelse("show_label" %in%
                                                                                                         colnames(anchors), anchors[3, "show_label"], TRUE),
                                                                                   image = ifelse("image" %in%
                                                                                                    colnames(anchors), anchors[3, "image"], ""))
                              labels <- list(top_anchor = top_anchor, left_anchor = left_anchor,
                                             right_anchor = right_anchor)

                              content <- private$slider_content_definition_R6()$new(labels = labels, pointer_image = df[["content"]][["pointer_image"]],  background_image = df[["content"]][["background_image"]])

                              definition  <- private$signifier_definition_R6()$new(id = df[["id"]], type = df[["type"]], title = df[["title"]], tooltip = df[["tooltip"]], allow_na = df[["allow_na"]],
                                                                                   fragment = df[["fragment"]], required = df[["required"]], sticky = df[["sticky"]], content = content, include = TRUE)
                              return(definition)

                            },
                            #
                            build_dyad = function(x, tid, df, type) {

                              # get the signifier definition entry being processed
                              df <- df %>% dplyr::filter(id == tid)
                              # get the anchors
                              anchors <- df[["content"]][["labels"]][[1]]
                              left_anchor  <-  private$label_definition_R6()$new(id = anchors[1,"id"], text =
                                                                                   anchors[1,"text"], show_image = ifelse("show_image" %in%
                                                                                                                            colnames(anchors), anchors[1, "show_image"], TRUE),
                                                                                 show_label = ifelse("show_label" %in%
                                                                                                       colnames(anchors), anchors[1, "show_label"], TRUE),
                                                                                 image = ifelse("image" %in%
                                                                                                  colnames(anchors), anchors[1, "image"], ""))
                              right_anchor  <-   private$label_definition_R6()$new(id = anchors[2,"id"], text =
                                                                                     anchors[2,"text"], show_image = ifelse("show_image" %in%
                                                                                                                              colnames(anchors), anchors[2, "show_image"], TRUE),
                                                                                   show_label = ifelse("show_label" %in%
                                                                                                         colnames(anchors), anchors[2, "show_label"], TRUE),
                                                                                   image = ifelse("image" %in%
                                                                                                    colnames(anchors), anchors[2, "image"], ""))
                              labels <- list(left_anchor = left_anchor,
                                             right_anchor = right_anchor)

                              content <- private$slider_content_definition_R6()$new(labels = labels, pointer_image = df[["content"]][["pointer_image"]],  background_image = df[["content"]][["background_image"]])

                              definition  <- private$signifier_definition_R6()$new(id = df[["id"]], type = df[["type"]], title = df[["title"]], tooltip = df[["tooltip"]], allow_na = df[["allow_na"]],
                                                                                   fragment = df[["fragment"]], required = df[["required"]], sticky = df[["sticky"]], content = content, include = TRUE)
                              return(definition)
                            },
                            #
                            build_list_item = function(x, tliid, ids) {
                              df_row <- ids %>% dplyr::filter(id == tliid)
                              listR6 <- private$item_list_definition_R6()$new(id = df_row[["id"]], title = df_row[["title"]], tooltip = ifelse("tooltip" %in% colnames(df_row), df_row[["tooltip"]], ""), visible = ifelse("visible" %in% colnames(df_row), df_row[["visible"]], TRUE), image = ifelse("image" %in% colnames(df_row), df_row[["image"]], ""))
                              return(listR6)
                            },
                            #
                            build_list = function(x, tid, tdf, type) {
                              # get the signifier definition entry being processed
                              df <- tdf %>% dplyr::filter(id == tid)
                              item <- df[["content"]][["items"]][[1]]
                              itemcontent <- df[["content"]]
                              result_item <- vector("list", length = nrow(item))
                              names(result_item) <- item$id
                              list_items <-  purrr::imap(result_item, private$build_list_item , item)
                             # list_items <-  purrr::imap(result_item, eval(parse(text = "private$build_list_item")) , item)
                              # , other_signifier_id = ifelse("other_signifier_id" %in% colnames(df_row), df_row[["other_signifier_id"]], "")
                              content <- private$item_content_definition_R6()$new(items = list_items, num_items = length(list_items), dynamic = itemcontent[["dynamic"]], random_order = itemcontent[["random_order"]], max_responses = itemcontent[["max_responses"]], min_responses = itemcontent[["min_responses"]],  other_signifier_id = ifelse(!is.na(itemcontent[["other_signifier_id"]]), itemcontent[["other_signifier_id"]], ""),  other_text = ifelse(!is.na(itemcontent[["other_text"]]), itemcontent[["other_text"]], ""))
                              definition <- private$signifier_definition_R6()$new(id = df[["id"]], type = df[["type"]], title = df[["title"]], tooltip = df[["tooltip"]], allow_na = df[["allow_na"]],
                                                                                  fragment = df[["fragment"]], required = df[["required"]], sticky = df[["sticky"]], content = content, include = TRUE)
                              return(definition)
                            },
                            #
                            build_stone_entry = function(x, tliid, ids) {
                              df_row <- ids %>% dplyr::filter(id == tliid)
                              stoneR6 <- private$stones_R6()$new(id = df_row[["id"]], image = ifelse("image" %in% colnames(df_row), df_row[["image"]], ""), title = df_row[["title"]], tooltip = ifelse("tooltip" %in% colnames(df_row), df_row[["tooltip"]], ""), visible = ifelse("visible" %in% colnames(df_row), df_row[["visible"]], TRUE))
                              return(stoneR6)
                            },
                            #
                            build_stones = function(x, tid, tdf, type) {
                              # get the signifier definition entry being processed
                              df <- tdf %>% dplyr::filter(id == tid)
                              stones <- df[["content"]][["stones"]][[1]]
                              x_axis <- df[["content"]][["axis"]][["x"]]
                              y_axis <- df[["content"]][["axis"]][["y"]]
                              stonescontent <- df[["content"]]
                              result_stones <- vector("list", length = nrow(stones))
                              names(result_stones) <- stones$id
                              stone_items <-  purrr::imap(result_stones, private$build_stone_entry  , stones)
                              #stone_items <-  purrr::imap(result_stones, eval(parse(text = "private$build_stone_entry"))  , stones)
                              stone_x_axis <- private$stones_axis_R6()$new(axis = "x", name = ifelse(!is.null(x_axis[["name"]]), x_axis[["name"]], "" ), end_label = ifelse(!is.null(x_axis[["end_label"]]), x_axis[["end_label"]], "" ), start_label = ifelse(!is.null(x_axis[["start_label"]]), x_axis[["start_label"]], "" ))
                              stone_y_axis <- private$stones_axis_R6()$new(axis = "y", name = ifelse(!is.null(y_axis[["name"]]), y_axis[["name"]], "" ), end_label = ifelse(!is.null(y_axis[["end_label"]]), y_axis[["end_label"]], "" ), start_label = ifelse(!is.null(y_axis[["start_label"]]), y_axis[["start_label"]], "" ))
                              stone_axis <- list(x = stone_x_axis, y = stone_y_axis)
                              content <- private$stone_content_definition_R6()$new(axis = stone_axis, stones = stone_items, num_stones = length(stone_items), background_image = stonescontent[["background_image"]])
                              definition <- private$signifier_definition_R6()$new(id = df[["id"]], type = df[["type"]], title = df[["title"]], tooltip = df[["tooltip"]], allow_na = df[["allow_na"]],
                                                                                  fragment = df[["fragment"]], required = df[["required"]], sticky = df[["sticky"]], content = content, include = TRUE)
                              return(definition)
                            },
                            #
                            build_freetext = function(x, tid, df, type) {
                              # get the signifier definition entry being processed
                              df <- df %>% dplyr::filter(id == tid)
                              text_content <- df[["content"]]
                              content <- private$freetext_content_definition_R6()$new(default = text_content[["default"]], multiline = text_content[["multiline"]])
                              definition <- private$signifier_definition_R6()$new(id = df[["id"]], type = df[["type"]], title = df[["title"]], tooltip = df[["tooltip"]], allow_na = df[["allow_na"]],
                                                                                  fragment = df[["fragment"]], required = df[["required"]], sticky = df[["sticky"]], content = content, include = TRUE)
                              return(definition)
                            },
                            #
                            build_imageselect_item = function(x, tliid, ids) {
                              df_row <- ids[as.numeric(tliid),]
                              listR6 <- private$imageselect_list_definition_R6()$new(default = df_row)
                              return(listR6)
                            },
                            #
                            build_imageselect =function(x, tid, df, type) {
                              # get the signifier definition entry being processed
                              df <- df %>% dplyr::filter(id == tid)
                              item <- df[["content"]][["items"]][[1]]
                              itemcontent <- df[["content"]]
                              result_item <- vector("list", length = nrow(item))
                              names(result_item) <- 1:length(result_item)
                              list_items <-  purrr::imap(result_item, private$build_imageselect_item, item)
                              content <- private$imageselect_content_definition_R6()$new(items = list_items, num_items = length(list_items))
                              definition <- private$signifier_definition_R6()$new(id = df[["id"]], type = df[["type"]], title = df[["title"]], tooltip = df[["tooltip"]], allow_na = df[["allow_na"]],
                                                                                  fragment = df[["fragment"]], required = df[["required"]], sticky = df[["sticky"]], content = content, include = TRUE)

                              return(definition)
                            },
                            #
                            build_photo = function(x, tid, df, type) {
                              # get the signifier definition entry being processed
                              df <- df %>% dplyr::filter(id == tid)
                              definition <- private$signifier_definition_R6()$new(id = df[["id"]], type = df[["type"]], title = df[["title"]], tooltip = df[["tooltip"]], allow_na = df[["allow_na"]],
                                                                                  fragment = df[["fragment"]], required = df[["required"]], sticky = df[["sticky"]], content = NULL, include = TRUE)
                              return(definition)
                            },
                            #
                            build_audio = function(x, tid, df, type) {
                              # get the signifier definition entry being processed
                              df <- df %>% dplyr::filter(id == tid)
                              definition <- private$signifier_definition_R6()$new(id = df[["id"]], type = df[["type"]], title = df[["title"]], tooltip = df[["tooltip"]], allow_na = df[["allow_na"]],
                                                                                  fragment = df[["fragment"]], required = df[["required"]], sticky = df[["sticky"]], content = NULL, include = TRUE)
                              return(definition)
                            },
                            #
                            build_uniqueid = function(x, tid, df, type) {
                              # get the signifier definition entry being processed
                              df <- df %>% dplyr::filter(id == tid)
                              definition <- private$signifier_definition_R6()$new(id = df[["id"]], type = df[["type"]], title = df[["title"]], tooltip = df[["tooltip"]], allow_na = df[["allow_na"]],
                                                                                  fragment = df[["fragment"]], required = df[["required"]], sticky = df[["sticky"]], content = NULL, include = TRUE)
                              return(definition)
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
                                                                  dynamic = NA,
                                                                  random_order = NA,
                                                                  max_responses = NA,
                                                                  min_responses = NA,
                                                                  other_signifier_id = NA,
                                                                  other_text = NA,
                                                                  initialize = function(items, num_items, dynamic, random_order, max_responses, min_responses, other_signifier_id, other_text) {
                                                                    self$items <- items
                                                                    self$num_items <- num_items
                                                                    self$dynamic <- dynamic
                                                                    self$random_order <- random_order
                                                                    self$max_responses <- max_responses
                                                                    self$min_responses <- min_responses
                                                                    self$other_signifier_id <- other_signifier_id
                                                                    self$other_text <- other_text
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
                                                   initialize = function(id, title, tooltip, visible, image) {
                                                     self$id <- id
                                                     self$title <- title
                                                     self$tooltip <- tooltip
                                                     self$visible <- visible
                                                     self$image <- image
                                                   }
                                                 )
                              )
                              )
                            },
                            #
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
                                                   }
                                                 )
                              ))
                            },
                           # ripple the addition of a manually added new signifier
                           ripple_update = function(tid, ttype) {
                             # type by id
                             add_list <- list(ttype)
                             names(add_list) <- tid
                             self$types_by_signifierid <- append(self$types_by_signifierid, add_list)
                             # id by type
                             self$signifierids_by_type[[ttype]] <- append(self$signifierids_by_type[[ttype]], tid)
                             # types with signifiers
                             if (!(ttype %in% self$types_with_signifiers)) {
                               self$types_with_signifiers <- append(self$types_with_signifiers, ttype)
                             }
                             # types by signiifier ID parent
                             self$types_by_signifierid_parent[[ttype]] <- append(self$types_by_signifierid_parent[[ttype]], add_list)
                             # id by type parent
                             self$signifierids_by_type_parent[[ttype]] <- append(self$signifierids_by_type_parent[[ttype]], tid)
                             # types with signifiers parent
                             if (!(ttype %in% self$types_with_signifiers_parent)) {
                               self$types_with_signifiers_parent <- append(self$types_with_signifiers_parent, ttype)
                             }
                           },
                           # remove signifier definition reference
                           remove_signifier_reference = function(tid, ttype) {
                           self$types_by_signifierid <- self$types_by_signifierid[self$types_by_signifierid != tid]
                           self$signifierids_by_type[[ttype]] <- self$signifierids_by_type[[ttype]][self$signifierids_by_type[[ttype]] != tid]
                           # if no more of this type then remove type
                           if (length(self$signifierids_by_type[[ttype]]) == 0) {
                             self$signifierids_by_type <- self$signifierids_by_type[self$signifierids_by_type != ttype]
                             self$types_with_signifiers <- self$types_with_signifiers[self$types_with_signifiers != ttype]
                           }
                           self$signifierids_by_type_parent[[ttype]] <- self$signifierids_by_type_parent[[ttype]][self$signifierids_by_type_parent[[ttype]] != tid]
                           self$signifier_definitions[[ttype]] <- self$signifier_definitions[[ttype]][! names(self$signifier_definitions[[ttype]]) == tid]
                           },
                           # get a list of list signifier type ids and their max responses
                           get_max_responses = function(R6list) {
                             return(purrr::map(R6list, ~{.x$content$max_responses}))
                           }
                          ) # private
) # R6 class
