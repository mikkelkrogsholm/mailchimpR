#' @export

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


# mylists <- get_lists(apikey = apikey)
#
# lapply(mylists$lists, function(x) data.frame(x$name, x$id))
#
# post_segment(list_id = "3434d68dab", name = "mikkelssegment", apikey = apikey, static_segment = "mikkelkrogsholm@gmail.com")

