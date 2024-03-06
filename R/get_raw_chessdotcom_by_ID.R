# DS: we are changinn the original function so we can access
# users by ID

# We first check the API rules: https://www.chess.com/news/view/published-data-api
# My reading is that we can indeed randomly pick IDs

# to note about player ID: 
# Note: the "player_id" is provided as a convenience to determine when a username has been changed. If you retrieve a Player Profile by the username-based URL linked from a Game or other object, and this new Player Profile has a "player_id" that matches a Profile you previously downloaded, then you can safely assume that this new Profile replaces the old, and all URLs with the previous username will now be found under the new username. This should be an extremely rare occurrence. This "player_id" will never change for a given account, however the future availability of this ID is not guaranteed.

# Response codes, which can help:
#200 = "enjoy your JSON"
#301 = if the URL you requested is bad, but we know where it should be; your client should remember and correct this to use the new URL in future requests
#304 = if your client supports "ETag/If-None-Match" or "Last-Modified/If-Modified-Since" caching headers and the data have not changed since the last request
#404 = we try to tell you if the URL is malformed or the data requested is just not available (e.g., a username for a user that does not exist)
#410 = we know for certain that no data will ever be available at the URL you requested; your client should not request this URL again
#429 = we are refusing to interpret your request due to rate limits; see "Rate Limiting" above

#' Get Single Player Raw chess.com Game Data
#'
#' This function returns the raw json data for a player's
#' chess.com data as a data frame
#'
#' @param username A valid username from chess.com
#' @param year_month An integer of YYYYMM
#'
#' @return a dataframe of chess.com data
#'
#' @importFrom magrittr %>%
#'
get_each_player_chessdotcom_by_ID <- function(player_id, year_month) {

  # if(is.na(year_month)) {
  #   print(glue::glue("Extracting {username} data for all months played, please wait"))
  # } else {
  #   print(glue::glue("Extracting {username} data for {year_month}, please wait"))
  # }

  # this function gets a list of all year/months the player(s) has played on chess.com
  get_month_urls <- function(){
    resp <- httr::GET(url = paste0("https://api.chess.com/pub/player/", player_id, "/games/archives"))
    check_status(resp)
    resp <- resp %>% httr::content()
    resp <- resp$archives
    return(resp)
  }
  # apply function to get a character vector of game urls
  if(is.na(year_month)) {
    month_urls <- get_month_urls()
  } else {
    month_urls <- get_month_urls() %>% unlist()
    year_mon <- stringr::str_sub(month_urls, start=-7) %>% gsub("/", "", .) %>% as.numeric()
    month_urls <- data.frame(year_mon, month_urls)
    month_urls <- month_urls %>% dplyr::filter(year_mon %in% year_month) %>% dplyr::pull(month_urls)
  }

  if(length(month_urls) >0) {
    # this function will parse the list of game urls and extract a json blob
    get_games <- function(y) {
      y <- jsonlite::fromJSON(y)
    }
    # apply function to get a list of all the games and game data
    games <- month_urls %>% purrr::map(get_games)

    # function to parse and extract game metadata
    extract_pgn <- function(x){
      tryCatch( {x <- x$games$pgn}, error = function(x) {x <- NA})
    }
    # apply to get a list of all games' metadata
    extracted_pgns <- games %>% purrr::map(extract_pgn)
    # function to create a single list to prepare for converting to a data frame
    create_pgn_list <-function(x) {
      x <- unlist(x) %>% as.list()
    }
    # apply the function to result in a list of each individual game
    pgn_list <- create_pgn_list(extracted_pgns)

    # Additional metadata:
    # function to extract the rules of each game
    extract_rules <- function(x){
      tryCatch( {x <- x$games$rules}, error = function(x) {x <- NA}) %>% as.character() %>% data.frame() %>% dplyr::mutate_if(is.factor, as.character)
    }
    GameRules <- games %>% purrr::map_df(extract_rules)
    # function to extract the time class of each game (ie blitz, bullet, daily, etc)
    extract_time_class <- function(x){
      tryCatch( {x <- x$games$time_class}, error = function(x) {x <- NA}) %>% as.character() %>% data.frame() %>% dplyr::mutate_if(is.factor, as.character)
    }
    TimeClass <- games %>%  purrr::map_df(extract_time_class)

    extra_df <- cbind(GameRules, TimeClass) %>% data.frame()
    colnames(extra_df) <- c("GameRules", "TimeClass")

    # function to extract all elements as columns, and all games as row in a data frame
    convert_to_df <- function(exp_list) {
      if(is.na(exp_list)) {
        df <- data.frame(Event=NA_character_)
      } else {
        pgn_list <- strsplit(exp_list, "\n") %>% unlist()
        tab_names <- c(gsub( "\\s.*", "", pgn_list[grep("\\[", pgn_list)][-c(length(pgn_list), (length(pgn_list)-1))]) %>% gsub("\\[", "", .), "Moves")
        tab_values <- gsub(".*[\"]([^\"]+)[\"].*", "\\1", pgn_list[grep("\\[", pgn_list)])
        if(length(tab_names) != length(tab_values)) {
          tab_values <- c(tab_values, NA)
        }
        #create the df of values
        df <- rbind(tab_values) %>% data.frame(stringsAsFactors = F)
        colnames(df) <- tab_names
        # remove the row names
        rownames(df) <- c()
        # need to clean up date variables
        df$Date <-  gsub("\\.", "-", df$Date)
        df$EndDate <- gsub("\\.", "-", df$EndDate)
      }

      return(df)
    }
    # convert the lists to data frames
    df <- pgn_list %>% purrr::map_df(convert_to_df)
    df <- cbind(extra_df, df)
    df$Username <- username
    df$player_id<- player_id
    
  } else {
    df <- data.frame()
  }

  # output the final data frame for each player
  return(df)
}


#' Get Raw chess.com Game Data
#'
#' This function returns the raw json data for a player's or list of players'
#' chess.com data as a data frame, for all or select months played
#'
#' @param usernames A vector of a valid username or usernames from chess.com
#' @param year_month An integer of YYYYMM
#'
#' @return a dataframe of chessdotcom data
#'
#' @importFrom magrittr %>%
#'
#' @export
#'
#' @examples
#' \dontrun{
#' get_raw_chessdotcom(usernames = "JaseZiv")
#' get_raw_chessdotcom(usernames = "JaseZiv", year_month = c(202112:202201))
#' get_raw_chessdotcom(usernames = c("JaseZiv", "Smudgy1"), year_month = 202201)
#' }
get_raw_chessdotcom_by_ID <- function(player_id, year_month=NA_integer_) {
  df <- purrr::map2_df(player_id, year_month, get_each_player_chessdotcom_by_ID)
  return(df)
}

