
get_json <- function(url){
  tryCatch(jsonlite::fromJSON(url),
           error = function(e){
             # print the error
             print(e)
             Sys.sleep(0.5)
             cat(paste("trying again on", url))
             # return an empty data.frame for this match
             return(get_json(url))
           })

}

#' Match history as an R object
#'
#' Visits the url generated by \code{\link{match_history_url}}
#' and parses the JSON into an R object
#'
#' @inheritParams match_history_url
#' @export
#'
raw_match_history <- function (api_key, account_id, ...){
  if (account_id == ""){
    print(match_history_url(api_key, ...))
    return(get_json(match_history_url(api_key, ...)))
  } else {
    return(get_json(match_history_url(api_key, account_id = account_id, ...)))
  }
}


#' Match Details as an R object
#'
#' Visits the url generated by \code{\link{match_details_url}}
#' and parses the JSON into an R object.
#'
#' @inheritParams match_details_url
#' @export
#' @return An R object from the api.
#'
raw_match_details <- function (api_key, match_id, ...){
  get_json(match_details_url(api_key, match_id, ...))
}


#' Get all of a players availible match ids
#'
#' Get a list of game ids that are availible for a player.
#' The maximum number of games that steam remembers for a player is 500
#'
#' @inheritParams match_history_url
#' @param account_id The id of the account whose match id's we want.
#' @param delay The amount of time (in seconds) to wait between api calls.
#' @param verbose Print progress messages?
#' #'
#' @return
#'  A data frame of a players matches, with
#'  a column of match ids. (Integer)
#'  a column of start times, (Integer UNIX time)
#'  a column of of lobby types
#'
#' @export
#'
#' @examples
#'  all_matches(api_key = "<your key>", account_id = 122191358)
all_matches <- function (api_key, account_id = "", delay = 1, verbose = TRUE, ...){
  if (verbose) cat(paste("Making first api call, on account id", account_id, "\n"))

  first <- raw_match_history(api_key, account_id = account_id)

  remaining_calls <- ceiling(first$result$results_remaining / 100)

  output <- data.frame(match_id = first$result$matches$match_id,
                       start_time = first$result$matches$start_time,
                       lobby_type = first$result$matches$lobby_type)


  while(remaining_calls > 0) {
    Sys.sleep(delay)
    if (verbose) cat(paste("with", remaining_calls, "remaining", "\n"))
    first <- raw_match_history(api_key,
                               account_id = account_id,
                               start_at_match_id = min(output$match_id) - 1)

    output <-   rbind(output, data.frame(match_id = first$result$matches$match_id,
                                     start_time = first$result$matches$start_time,
                                     lobby_type = first$result$matches$lobby_type))
    remaining_calls <- remaining_calls - 1
  }
  return(output)
}

#' Details
#'
#' Query the api for each match_id in match_ids,
#' returns the results in a data.frame.
#'
#' @inheritParams match_details_url
#' @param match_ids A list or a single match id that we will query the api with
#' @param delay The amount of time (in seconds) to wait between api calls.
#' @param simplify_colnames
#' @param ... Additional arguments for the url builder.
#'
#' @return A data.frame of the match details
#' @export
#'
#' @examples
#' details(api_key = "<api_key>", match_ids = c(2331506594, 2331349045))
details <- function(api_key, match_ids, verbose = FALSE, delay = 0.5, ...){
  output <- plyr::ldply(match_ids, .progress = plyr::progress_time(),
         function(id){
           if(verbose) cat(paste("Getting details for match:",id, "\n"))
           Sys.sleep(delay)
           result <- raw_match_details(api_key, match_id = id, ...)
           result$result$picks_bans <- NULL
           as.data.frame(result)
         })

  # clean up the column names
  colnames(output) <- gsub("result.|players.", "", colnames(output))

  # add some extra columns
  output$date <- lubridate::ymd_hms(as.POSIXct(output$start_time, origin="1970-01-01"))
  output$team <- ifelse(output$player_slot < 100,  "Radiant", "Dire")
  output$winner <- !xor(output$team == "Radiant", output$radiant_win)
  return(output)
}

#' Convert 32-bit account id to a 64 bit Steam id
#'
#' Converts a 32-bit account id to a 64 bit Steam id that can be used to query
#' api's that require the 64 bit steam id.
#'
#' @param account_id The 32 bit account id, can be numeric or character.
#'
#' @return A string representing a users 64 bit steam id.
#' @export
#'
#' @examples
#' account_to_steam(76561198082457086)
#'
#'
# see:
# https://gist.github.com/almirsarajcic/4664387
account_to_steam <- function (account_id){
  paste0('765', as.integer(account_id) + 61197960265728)
}


#' Convert a 64-bit Steam id into a 32-bit account id
#'
#' Convert a 64-bit Steam id into a 32-bit account id that can be used to query dota 2
#' apis
#'
#' @param steam_id The 64 bit string steam id, must be a string (character) as otherwise.
#'
#' @return A string representing a users 64 bit steam id.
#' @export
#'
#' @examples
#' steam_to_account("76561197992765754")
#'
#'
# see:
# https://gist.github.com/almirsarajcic/4664387
steam_to_account <- function (steam_id){
  as.double(substr(steam_id, 4, nchar(steam_id))) - 61197960265728
}

