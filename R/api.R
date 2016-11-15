########## apiending ----
#' Splits the api key to get server for api call
#'
#' @description Is a helperfunction to extract the server that needs to be called in the api call. The server is the ending of the api key.
#'
#' @param apikey API key for mailchimp account
#' @return the server for use in later api call
#'
#' @examples
#'
#' **## Not run:**
#' apiending(apikey = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx-usxx")
#' ## End(**Not run**)

apiending <- function(apikey){

  ending <- stringr::str_split(apikey, "-")
  ending <- unlist(ending)[2]

  return(ending)
}
