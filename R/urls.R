#' Match History url
#'
#' Builds a URL for retrieving match histories from the dota 2 api.
#'
#' When called with no query parameters it will generate a url for  retrieveing the last 100 public games.
#'
#' see: \url{http://wiki.teamfortress.com/wiki/WebAPI/GetMatchHistory}
#'
#' @param api_key A String of your steam developer key, see: \url{https://steamcommunity.com/dev/apikey}
#' @param https Logical, if True: https, False: http
#' @param ... Query parameters, see: \url{http://wiki.teamfortress.com/wiki/WebAPI/GetMatchHistory}
#' @return The url for querying the dota 2 api.
#'
#' @examples
#' match_history_url(api_key = "<your_api_key>")
#'
#' @export
match_history_url <- function (api_key = api_key, https = TRUE, ...){
  api_args <- c("key"=api_key, list(...))
  url_builder("GetMatchHistory", api_args, https)
}

#' Match Details url
#'
#' Builds a URL for retrieving match details
#'
#' @param api_key A String of your steam developer key, see: \url{https://steamcommunity.com/dev/apikey}
#' @param https Logical, if True: https, False: http
#' @param match_id The id of the match that you wish to get more information about.
#' @return The url for querying the dota 2 api.
#'
#' @examples
#' match_details_url(api_key = "<your_api_key>", match_id = 123114122)
#'
#' @export
match_details_url <- function (api_key = api_key, match_id,  https = TRUE){
  api_args <- c("key"=api_key, "match_id"=match_id)
  url_builder("GetMatchDetails", api_args, https)
}


#' Builds a url for the dota 2 api
#'
#' See: \url{https://wiki.teamfortress.com/wiki/WebAPI}
#'
#' @param query A String The type of query to build GetMatchHistory or GetMatchDetails
#' @param api_args A list of parameters for the query.
#' @param https Logical, if True: https, False: http.
#' @param api The string id of the api.
#' @param version The string of the api version.
#'
#' @return  The url of the query
#'
#' @export
url_builder <- function (query, api_args, https, api = "IDOTA2Match_570", version = "v1"){
  paste0(ifelse(https, "https", "http"),
         "://api.steampowered.com/", api, "/",query,
         "/", version ,"/?",
         Reduce(
           function(x, y) paste(x, y, sep = "&"),
           paste(names(api_args), api_args, sep = "=")))
}
