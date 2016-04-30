
# https://wiki.teamfortress.com/wiki/WebAPI/GetMatchHistory


# a function that querys steams dotat2 api
match_history_url <- function (api_key = api_key, https = TRUE, ...){
  api_args <- c("key"=api_key, list(...))
  url_builder("GetMatchHistory", api_args, https)
}

# builds a url for a player details query
match_details_url <- function (api_key = api_key, https = TRUE, ...){
  api_args <- c("key"=api_key, list(...))
  url_builder("GetMatchDetails", api_args, https)
}


#' builds a url for the dota 2 api
#'
#' See: https://wiki.teamfortress.com/wiki/WebAPI
#'
#' @param query A String The type of query to build GetMatchHistory or GetMatchDetails
#' @param api_args A list of parameters for the query
#' @param https Logical, if True: https, False: http
#' @param api string: The api id
#' @return  The url of the query
url_builder <- function (query, api_args, https, api = "IDOTA2Match_570", version = "V001"){
  paste0(ifelse(https, "https", "http"),
         "://api.steampowered.com/", api, "/",query,
         "/", version ,"/?format=JSON&",
         Reduce(
           function(x, y) paste(x, y, sep = "&"),
           paste(names(api_args), api_args, sep = "=")))
}

#
match_history <- function (raw = FALSE, ...){
  if (raw) jsonlite::fromJSON(match_history_url(...), flatten = TRUE)
}




