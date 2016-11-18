########## post_segment ----

#' Create a new segment
#'
#' @description Create a new segment in a specific list.
#'
#' @param name The name of the segment.
#' @param static_segment Vector of emails. Any emails provided that are not present will be ignored. Passing an empty vector will create a static segment without any subscribers. This field cannot be provided with the options field.
#' @param apikey API key for mailchimp account
#' @param user User to use in authentication. Can be any string you like. Defaults to "anystring".
#' @param ... Query string parameters. See available parameters under CREATE here: \href{https://developer.mailchimp.com/documentation/mailchimp/reference/lists/segments/}{API documentation}.
#' @return Response body parameters. See response parameters under CREATE here: \href{https://developer.mailchimp.com/documentation/mailchimp/reference/lists/segments/}{API documentation}.
#'
#' @examples
#'
#' **## Not run:**
#' post_segment <- post_segment(name = "my_new_segment", static_segment = c("e@mail1.com", "e@mail2.com"),  apikey = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx-usxx")
#' ## End(**Not run**)
#'
#' @export
#'
post_segment <- function(list_id = NULL,
                         name = NULL,
                         static_segment = NULL,
                         apikey = NULL,
                         user = "anystring",
                         ...){

  # Sanity check
  if(is.null(apikey)) stop("API key is missing")
  if(is.null(list_id)) stop("List id is missing")
  if(is.null(name)) stop("Segment name is missing")

  # Query string parameters
  body <- list(name = name, static_segment = list(static_segment))
  body <- RJSONIO::toJSON(body)

  # Construct API url
  apiurl <- paste0("https://", mailchimpR:::apiending(apikey),".api.mailchimp.com/3.0/lists/", list_id, "/segments")

  # Call the API to get the reports data
  POSTdata <- httr::POST(url = apiurl, body = body,
                       httr::authenticate(user, apikey))

  # Convert the response to JSON
  POSTdata <- httr::content(POSTdata, type = "application/json")

  # Return data
  return(POSTdata)

}

########## post_segment_members ----

#' Batch add/remove list members to static segment
#'
#' @description Batch add/remove list members to static segment
#'
#' @param segment_id Id of segment.
#' @param members_to_add A vector of emails to be used for a static segment. Any emails provided that are not present on the list will be ignored.
#' @param members_to_remove A vector of emails to be used for a static segment. Any emails provided that are not present on the list will be ignored.
#' @param apikey API key for mailchimp account
#' @param user User to use in authentication. Can be any string you like. Defaults to "anystring".
#' @param ... Query string parameters. See available parameters under CREATE here: \href{https://developer.mailchimp.com/documentation/mailchimp/reference/lists/segments/}{API documentation}.
#' @return Response body parameters. See response parameters under CREATE here: \href{https://developer.mailchimp.com/documentation/mailchimp/reference/lists/segments/}{API documentation}.
#'
#' @examples
#'
#' **## Not run:**
#' post_segment_members <- post_segment_members(list_id = "xxxxxxxxxx", members_to_add = c("e@mail1.com", "e@mail2.com"), members_to_remove = c("e@mail3.com", "e@mail4.com"), apikey = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx-usxx")
#' ## End(**Not run**)
#'
#' @export
#'
post_segment_members <- function(list_id = NULL,
                                 segment_id = NULL,
                                 members_to_add = NULL,
                                 members_to_remove = NULL,
                                 apikey = NULL,
                                 user = "anystring",
                                 ...){

  # Sanity check
  if(is.null(apikey)) stop("API key is missing")
  if(is.null(list_id)) stop("List id is missing")
  if(is.null(segment_id)) stop("Segment id is missing")

  # Query string parameters
  body <- list(members_to_add = members_to_add, members_to_remove = members_to_remove)
  body <- purrr::compact(body)
  body <- RJSONIO::toJSON(body)

  # Construct API url
  apiurl <- paste0("https://", mailchimpR:::apiending(apikey),".api.mailchimp.com/3.0/lists/", list_id, "/segments/", segment_id)

  # Call the API to get the reports data
  POSTdata <- httr::POST(url = apiurl, body = body,
                         httr::authenticate(user, apikey))

  # Convert the response to JSON
  POSTdata <- httr::content(POSTdata, type = "application/json")

  # Return data
  return(POSTdata)

}

