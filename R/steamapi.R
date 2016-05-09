
#' Player Summary
#'
#' Calls the steam community API See: \url{https://developer.valvesoftware.com/wiki/Steam_Web_API}.
#'
#'
#'
#' @param api_key A String of your steam developer key, see: \url{https://steamcommunity.com/dev/apikey}
#' @param account_ids The 32-bit account id that will be converted into a steam id
#'
#' @return
#' @export
#'
#' @examples
#' get_player_summaries(api_key = "<api_key>", account_ids = 122191358)
get_player_summaries <- function(api_key = api_key, account_ids, delay = 0.5){
  output <- data.frame()

  # the api can only handle requests of 100 ids or less
  # so we will use recursion
  if (length(account_ids) > 100){
    Sys.sleep(delay)
    # recursion step
    output <- get_player_summaries(api_key = api_key, account_ids[101:length(account_ids)])
    account_ids <- account_ids[1:100]
  }
  steam_ids <- account_to_steam(account_ids)
  api_args <- list(key = api_key,
                   steamids = Reduce(steam_ids,
                                     f = function(a,b) paste(a, b, sep = ",")))
  url <- url_builder("GetPlayerSummaries",
              api = "ISteamUser",
              https = TRUE,
              version = "v0002",
              api_args = api_args)

  temp <- jsonlite::fromJSON(url)$response$players
  temp$account_id <- account_ids
  rbind(output, temp)
}

#' Player Bans
#'
#' @param api_key A String of your steam developer key, see: \url{https://steamcommunity.com/dev/apikey}
#' @param account_ids
#'
#' @return a data frame of
#' @export
#'
#' @examples
#' get_account_bans(api_key = "<api_key>", account_ids = 122191358)
#' get_account_bans(api_key = "<api_key>", account_ids = c(122191358, 114304446))
get_account_bans <- function(api_key = api_key, account_ids, delay = 0.5){
  output <- data.frame()
  # the api can only handle requests of 100 ids or less
  # so we will use recursion
  if (length(account_ids) > 100){
    Sys.sleep(delay)
    # recursion step
    output <- get_account_bans(api_key = api_key, account_ids[101:length(account_ids)])
    account_ids <- account_ids[1:100]
  }
  steam_ids <- account_to_steam(account_ids)
  api_args <- list(key = api_key,
                   steamids = Reduce(steam_ids,
                                     f = function(a,b) paste(a, b, sep = ",")))

#: http://api.steampowered.com/ISteamUser/GetPlayerBans/v1/?key=XXXXXXXXXXXXXXXXX&steamids=XXXXXXXX,YYYYY
  url <- url_builder("GetPlayerBans",
                     api = "ISteamUser",
                     https = TRUE,
                     version = "v1",
                     api_args = api_args)

  temp <- jsonlite::fromJSON(url)$players
  temp$account_id <- account_ids
  rbind(output, temp)
}

get_dota_playtime <- function(){
  # TODO
}

# http://api.steampowered.com/ISteamUser/GetFriendList/v0001/?key=<key>&steamid=76561197960435530&relationship=friend
#' Get Friends List
#'
#' @param api_key A String of your steam developer key, see: \url{https://steamcommunity.com/dev/apikey}
#' @param account_id the dota account
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
get_friend_list <- function (api_key, account_id, ...){
  steam_id <- account_to_steam(account_id)

  url <- url_builder("GetFriendList",
                     api = "ISteamUser",
                     https = TRUE,
                     version = "v0001",
                     api_args = list(key = api_key,
                                     steamid = steam_id,
                                     relationship = "friend"))

  jsonlite::fromJSON(url)$friends
}

