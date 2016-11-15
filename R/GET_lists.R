########## get_lists ----

#' Get information about all lists
#'
#' @description Get information about all lists in the account.
#'
#' @param apikey API key for mailchimp account
#' @param user User to use in authentication. Can be any string you like. Defaults to "anystring".
#' @param ... Query string parameters. See available parameters under READ here: \href{https://developer.mailchimp.com/documentation/mailchimp/reference/lists/#}{API documentation}.
#' @return Response body parameters. See response parameters under READ here: \href{https://developer.mailchimp.com/documentation/mailchimp/reference/lists/#}{API documentation}.
#'
#' @examples
#'
#' **## Not run:**
#' mylists <- get_lists(apikey = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx-usxx")
#' ## End(**Not run**)
#'
#' @export

get_lists <- function(apikey = NULL,
                      user = "anystring",
                      ...){

  # Sanity check
  if(is.null(apikey)) stop("API key is missing")

  # Query string parameters
  request <- list(...)

  # Construct API url
  apiurl <- paste0("https://", mailchimpR:::apiending(apikey),".api.mailchimp.com/3.0/lists")

  # Call the API to get the reports data
  GETdata <- httr::GET(url = apiurl, query = request,
                       httr::authenticate(user, apikey))

  # Convert the response to JSON
  GETdata <- httr::content(GETdata, type = "application/json")

  # Return data
  return(GETdata)

}


########## get_list ----

#' 	Get information about a specific list
#'
#' @description Get information about a specific list in your MailChimp account. Results include list members who have signed up but haven’t confirmed their subscription yet and \href{http://kb.mailchimp.com/lists/managing-subscribers/view-unsubscribed-and-cleaned-addresses}{unsubscribed or cleaned}.
#'
#' @param list_id The unique id for the list. Find the list id with \code{\link{get_lists}}
#' @param apikey API key for mailchimp account
#' @param user User to use in authentication. Can be any string you like. Defaults to "anystring".
#' @param ... Query string parameters. See available parameters under READ here: \href{https://developer.mailchimp.com/documentation/mailchimp/reference/lists/#}{API documentation}.
#' @return Response body parameters. See response parameters under READ here: \href{https://developer.mailchimp.com/documentation/mailchimp/reference/lists/#}{API documentation}.
#'
#' @examples
#'
#' **## Not run:**
#' mylist <- get_list(apikey = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx-usxx", list_id = "xxxxxxxxxx")
#' ## End(**Not run**)
#'
#' @export

get_list <- function(list_id = NULL,
                     apikey = NULL,
                     user = "anystring",
                     ...){

  # Sanity check
  if(is.null(apikey)) stop("API key is missing")
  if(is.null(list_id)) stop("List id is missing")

  # Query string parameters
  request <- list(...)

  # Construct API url
  apiurl <- paste0("https://", mailchimpR:::apiending(apikey),".api.mailchimp.com/3.0/lists/", list_id)

  # Call the API to get the reports data
  GETdata <- httr::GET(url = apiurl, query = request,
                       httr::authenticate(user, apikey))

  # Convert the response to JSON
  GETdata <- httr::content(GETdata, type = "application/json")

  # Return data
  return(GETdata)

}


########## get_list_members ----

#' Get information about members in a list
#'
#' @description Get information about members in a specific MailChimp list.
#'
#' @param list_id The unique id for the list. Find the list id with \code{\link{get_lists}}
#' @param apikey API key for mailchimp account
#' @param user User to use in authentication. Can be any string you like. Defaults to "anystring".
#' @param ... Query string parameters. See available parameters under READ here: \href{https://developer.mailchimp.com/documentation/mailchimp/reference/lists/members/#}{API documentation}.
#' @return Response body parameters. See response parameters under READ here: \href{https://developer.mailchimp.com/documentation/mailchimp/reference/lists/members/#}{API documentation}.
#'
#' @examples
#'
#' **## Not run:**
#' mymembers <- get_list_members(apikey = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx-usxx", list_id = "xxxxxxxxxx")
#' ## End(**Not run**)
#'
#' @export

get_list_members <- function(list_id = NULL,
                     apikey = NULL,
                     user = "anystring",
                     ...){

  # Sanity check
  if(is.null(apikey)) stop("API key is missing")
  if(is.null(list_id)) stop("List id is missing")

  # Query string parameters
  request <- list(...)

  # Construct API url
  apiurl <- paste0("https://", mailchimpR:::apiending(apikey),".api.mailchimp.com/3.0/lists/", list_id, "/members")

  # Call the API to get the reports data
  GETdata <- httr::GET(url = apiurl, query = request,
                       httr::authenticate(user, apikey))

  # Convert the response to JSON
  GETdata <- httr::content(GETdata, type = "application/json")

  # Return data
  return(GETdata)

}

########## get_list_member ----

#' Get information about a specific list member
#'
#' @description Get information about a specific list member, including a currently subscribed, unsubscribed, or bounced member.
#'
#' @param list_id The unique id for the list. Find the list id with \code{\link{get_lists}}.
#' @param subscriber_hash The MD5 hash of the lowercase version of the list member’s email address.
#' @param apikey API key for mailchimp account.
#' @param user User to use in authentication. Can be any string you like. Defaults to "anystring".
#' @param ... Query string parameters. See available parameters under READ here: \href{https://developer.mailchimp.com/documentation/mailchimp/reference/lists/members/#}{API documentation}.
#' @return Response body parameters. See response parameters under READ here: \href{https://developer.mailchimp.com/documentation/mailchimp/reference/lists/members/#}{API documentation}.
#'
#' @examples
#'
#' **## Not run:**
#' mymember <- get_list_member(apikey = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx-usxx", list_id = "xxxxxxxxxx", subscriber_hash = "xxxxxxxxxx")
#' ## End(**Not run**)
#'
#' @export
get_list_member <- function(list_id = NULL,
                            subscriber_hash = NULL,
                             apikey = NULL,
                             user = "anystring",
                             ...){

  # Sanity check
  if(is.null(apikey)) stop("API key is missing")
  if(is.null(list_id)) stop("List id is missing")
  if(is.null(subscriber_hash)) stop("Subscriber hash is missing")

  # Query string parameters
  request <- list(...)

  # Construct API url
  apiurl <- paste0("https://", mailchimpR:::apiending(apikey),".api.mailchimp.com/3.0/lists/", list_id, "/members/", subscriber_hash)

  # Call the API to get the reports data
  GETdata <- httr::GET(url = apiurl, query = request,
                       httr::authenticate(user, apikey))

  # Convert the response to JSON
  GETdata <- httr::content(GETdata, type = "application/json")

  # Return data
  return(GETdata)

}

########## get_list_member_activity ----

#' Get recent list member activity
#'
#' @description Get the last 50 events of a member’s activity on a specific list, including opens, clicks, and unsubscribes.
#'
#' @param list_id The unique id for the list. Find the list id with \code{\link{get_lists}}.
#' @param subscriber_hash The MD5 hash of the lowercase version of the list member’s email address.
#' @param apikey API key for mailchimp account.
#' @param user User to use in authentication. Can be any string you like. Defaults to "anystring".
#' @param ... Query string parameters. See available parameters under READ here: \href{https://developer.mailchimp.com/documentation/mailchimp/reference/lists/members/activity/}{API documentation}.
#' @return Response body parameters. See response parameters under READ here: \href{https://developer.mailchimp.com/documentation/mailchimp/reference/lists/members/activity/}{API documentation}.
#'
#' @examples
#'
#' **## Not run:**
#' mymember_activity <- get_list_member_activity(apikey = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx-usxx", list_id = "xxxxxxxxxx", subscriber_hash = "xxxxxxxxxx")
#' ## End(**Not run**)
#'
#' @export
get_list_member_activity <- function(list_id = NULL,
                            subscriber_hash = NULL,
                            apikey = NULL,
                            user = "anystring",
                            ...){

  # Sanity check
  if(is.null(apikey)) stop("API key is missing")
  if(is.null(list_id)) stop("List id is missing")
  if(is.null(subscriber_hash)) stop("Subscriber hash is missing")

  # Query string parameters
  request <- list(...)

  # Construct API url
  apiurl <- paste0("https://", mailchimpR:::apiending(apikey),".api.mailchimp.com/3.0/lists/", list_id, "/members/", subscriber_hash, "/activity")

  # Call the API to get the reports data
  GETdata <- httr::GET(url = apiurl, query = request,
                       httr::authenticate(user, apikey))

  # Convert the response to JSON
  GETdata <- httr::content(GETdata, type = "application/json")

  # Return data
  return(GETdata)

}

########## get_list_member_goals ----

#' Get the last 50 Goal events for a member on a specific list
#'
#' @description Get the last 50 Goal events for a member on a specific list
#'
#' @param list_id The unique id for the list. Find the list id with \code{\link{get_lists}}.
#' @param subscriber_hash The MD5 hash of the lowercase version of the list member’s email address.
#' @param apikey API key for mailchimp account.
#' @param user User to use in authentication. Can be any string you like. Defaults to "anystring".
#' @param ... Query string parameters. See available parameters under READ here: \href{https://developer.mailchimp.com/documentation/mailchimp/reference/lists/members/goals/}{API documentation}.
#' @return Response body parameters. See response parameters under READ here: \href{https://developer.mailchimp.com/documentation/mailchimp/reference/lists/members/goals/}{API documentation}.
#'
#' @examples
#'
#' **## Not run:**
#' mymember_goals <- get_list_member_goals(apikey = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx-usxx", list_id = "xxxxxxxxxx", subscriber_hash = "xxxxxxxxxx")
#' ## End(**Not run**)
#'
#' @export
get_list_member_goals <- function(list_id = NULL,
                                     subscriber_hash = NULL,
                                     apikey = NULL,
                                     user = "anystring",
                                     ...){

  # Sanity check
  if(is.null(apikey)) stop("API key is missing")
  if(is.null(list_id)) stop("List id is missing")
  if(is.null(subscriber_hash)) stop("Subscriber hash is missing")

  # Query string parameters
  request <- list(...)

  # Construct API url
  apiurl <- paste0("https://", mailchimpR:::apiending(apikey),".api.mailchimp.com/3.0/lists/", list_id, "/members/", subscriber_hash, "/goals")

  # Call the API to get the reports data
  GETdata <- httr::GET(url = apiurl, query = request,
                       httr::authenticate(user, apikey))

  # Convert the response to JSON
  GETdata <- httr::content(GETdata, type = "application/json")

  # Return data
  return(GETdata)

}

########## get_list_member_notes ----

#' Get recent notes for a specific list member
#'
#' @description Get recent notes for a specific list member
#'
#' @param list_id The unique id for the list. Find the list id with \code{\link{get_lists}}.
#' @param subscriber_hash The MD5 hash of the lowercase version of the list member’s email address.
#' @param apikey API key for mailchimp account.
#' @param user User to use in authentication. Can be any string you like. Defaults to "anystring".
#' @param ... Query string parameters. See available parameters under READ here: \href{https://developer.mailchimp.com/documentation/mailchimp/reference/lists/members/notes/}{API documentation}.
#' @return Response body parameters. See response parameters under READ here: \href{https://developer.mailchimp.com/documentation/mailchimp/reference/lists/members/notes/}{API documentation}.
#'
#' @examples
#'
#' **## Not run:**
#' mymember_notes <- get_list_member_notes(apikey = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx-usxx", list_id = "xxxxxxxxxx", subscriber_hash = "xxxxxxxxxx")
#' ## End(**Not run**)
#'
#' @export
get_list_member_notes <- function(list_id = NULL,
                                  subscriber_hash = NULL,
                                  apikey = NULL,
                                  user = "anystring",
                                  ...){

  # Sanity check
  if(is.null(apikey)) stop("API key is missing")
  if(is.null(list_id)) stop("List id is missing")
  if(is.null(subscriber_hash)) stop("Subscriber hash is missing")

  # Query string parameters
  request <- list(...)

  # Construct API url
  apiurl <- paste0("https://", mailchimpR:::apiending(apikey),".api.mailchimp.com/3.0/lists/", list_id, "/members/", subscriber_hash, "/notes")

  # Call the API to get the reports data
  GETdata <- httr::GET(url = apiurl, query = request,
                       httr::authenticate(user, apikey))

  # Convert the response to JSON
  GETdata <- httr::content(GETdata, type = "application/json")

  # Return data
  return(GETdata)

}

########## get_list_member_note ----

#' Get recent notes for a specific list member
#'
#' @description Get recent notes for a specific list member
#'
#' @param list_id The unique id for the list. Find the list id with \code{\link{get_lists}}.
#' @param subscriber_hash The MD5 hash of the lowercase version of the list member’s email address.
#' @param note_id The id for the note.
#' @param apikey API key for mailchimp account.
#' @param user User to use in authentication. Can be any string you like. Defaults to "anystring".
#' @param ... Query string parameters. See available parameters under READ here: \href{https://developer.mailchimp.com/documentation/mailchimp/reference/lists/members/notes/}{API documentation}.
#' @return Response body parameters. See response parameters under READ here: \href{https://developer.mailchimp.com/documentation/mailchimp/reference/lists/members/notes/}{API documentation}.
#'
#' @examples
#'
#' **## Not run:**
#' mymember_note <- get_list_member_note(apikey = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx-usxx", list_id = "xxxxxxxxxx", subscriber_hash = "xxxxxxxxxx",  note_id = "xxxxxxxxxx")
#' ## End(**Not run**)
#'
#' @export
get_list_member_note <- function(list_id = NULL,
                                  subscriber_hash = NULL,
                                  apikey = NULL,
                                  user = "anystring",
                                  ...){

  # Sanity check
  if(is.null(apikey)) stop("API key is missing")
  if(is.null(list_id)) stop("List id is missing")
  if(is.null(subscriber_hash)) stop("Subscriber hash is missing")

  # Query string parameters
  request <- list(...)

  # Construct API url
  apiurl <- paste0("https://", mailchimpR:::apiending(apikey),".api.mailchimp.com/3.0/lists/", list_id, "/members/", subscriber_hash, "/notes/", note_id)

  # Call the API to get the reports data
  GETdata <- httr::GET(url = apiurl, query = request,
                       httr::authenticate(user, apikey))

  # Convert the response to JSON
  GETdata <- httr::content(GETdata, type = "application/json")

  # Return data
  return(GETdata)

}

########## get_list_segments ----

#' Get information about all segments in a list
#'
#' @description Get information about all segments in a list
#'
#' @param list_id The unique id for the list. Find the list id with \code{\link{get_lists}}
#' @param apikey API key for mailchimp account
#' @param user User to use in authentication. Can be any string you like. Defaults to "anystring".
#' @param ... Query string parameters. See available parameters under READ here: \href{https://developer.mailchimp.com/documentation/mailchimp/reference/lists/segments/#}{API documentation}.
#' @return Response body parameters. See response parameters under READ here: \href{https://developer.mailchimp.com/documentation/mailchimp/reference/lists/segments/#}{API documentation}.
#'
#' @examples
#'
#' **## Not run:**
#' mysegments <- get_list_segments(apikey = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx-usxx", list_id = "xxxxxxxxxx")
#' ## End(**Not run**)
#'
#' @export

get_list_segments <- function(list_id = NULL,
                             apikey = NULL,
                             user = "anystring",
                             ...){

  # Sanity check
  if(is.null(apikey)) stop("API key is missing")
  if(is.null(list_id)) stop("List id is missing")

  # Query string parameters
  request <- list(...)

  # Construct API url
  apiurl <- paste0("https://", mailchimpR:::apiending(apikey),".api.mailchimp.com/3.0/lists/", list_id, "/segments")

  # Call the API to get the reports data
  GETdata <- httr::GET(url = apiurl, query = request,
                       httr::authenticate(user, apikey))

  # Convert the response to JSON
  GETdata <- httr::content(GETdata, type = "application/json")

  # Return data
  return(GETdata)

}

########## get_list_segment ----

#' Get information about a specific segment
#'
#' @description Get information about a specific segment.
#'
#' @param list_id The unique id for the list. Find the list id with \code{\link{get_lists}}.
#' @param segment_id The unique id for the segment.
#' @param apikey API key for mailchimp account
#' @param user User to use in authentication. Can be any string you like. Defaults to "anystring".
#' @param ... Query string parameters. See available parameters under READ here: \href{https://developer.mailchimp.com/documentation/mailchimp/reference/lists/segments/#}{API documentation}.
#' @return Response body parameters. See response parameters under READ here: \href{https://developer.mailchimp.com/documentation/mailchimp/reference/lists/segments/#}{API documentation}.
#'
#' @examples
#'
#' **## Not run:**
#' mysegment <- get_list_segment(apikey = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx-usxx", list_id = "xxxxxxxxxx", segment_id = "xxxxxxxxxx")
#' ## End(**Not run**)
#'
#' @export
get_list_segments <- function(list_id = NULL,
                              segment_id = NULL,
                              apikey = NULL,
                              user = "anystring",
                              ...){

  # Sanity check
  if(is.null(apikey)) stop("API key is missing")
  if(is.null(list_id)) stop("List id is missing")
  if(is.null(segment_id)) stop("Segment id is missing")

  # Query string parameters
  request <- list(...)

  # Construct API url
  apiurl <- paste0("https://", mailchimpR:::apiending(apikey),".api.mailchimp.com/3.0/lists/", list_id, "/segments/", segment_id)

  # Call the API to get the reports data
  GETdata <- httr::GET(url = apiurl, query = request,
                       httr::authenticate(user, apikey))

  # Convert the response to JSON
  GETdata <- httr::content(GETdata, type = "application/json")

  # Return data
  return(GETdata)

}

########## get_list_segment_members ----

#' Get information about all members in a list segment
#'
#' @description Get information about all members in a list segment
#'
#' @param list_id The unique id for the list. Find the list id with \code{\link{get_lists}}.
#' @param segment_id The unique id for the segment.
#' @param apikey API key for mailchimp account
#' @param user User to use in authentication. Can be any string you like. Defaults to "anystring".
#' @param ... Query string parameters. See available parameters under READ here: \href{https://developer.mailchimp.com/documentation/mailchimp/reference/lists/segments/#}{API documentation}.
#' @return Response body parameters. See response parameters under READ here: \href{https://developer.mailchimp.com/documentation/mailchimp/reference/lists/segments/#}{API documentation}.
#'
#' @examples
#'
#' **## Not run:**
#' mysegment_members <- get_list_segment_members(apikey = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx-usxx", list_id = "xxxxxxxxxx", segment_id = "xxxxxxxxxx")
#' ## End(**Not run**)
#'
#' @export
get_list_segment_members <- function(list_id = NULL,
                              segment_id = NULL,
                              apikey = NULL,
                              user = "anystring",
                              ...){

  # Sanity check
  if(is.null(apikey)) stop("API key is missing")
  if(is.null(list_id)) stop("List id is missing")
  if(is.null(segment_id)) stop("Segment id is missing")

  # Query string parameters
  request <- list(...)

  # Construct API url
  apiurl <- paste0("https://", mailchimpR:::apiending(apikey),".api.mailchimp.com/3.0/lists/", list_id, "/segments/", segment_id, "/members")

  # Call the API to get the reports data
  GETdata <- httr::GET(url = apiurl, query = request,
                       httr::authenticate(user, apikey))

  # Convert the response to JSON
  GETdata <- httr::content(GETdata, type = "application/json")

  # Return data
  return(GETdata)

}

