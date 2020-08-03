#' Harvest API v2 get request wrapper
#'
#' Submits get requests to the Harvest API v2. The function is a wrapper around the httr::GET function and returns the Harvest table.
#'
#' @param table Character table name to be submitted to the Harvest API v2. Refer to \href{https://help.getharvest.com/api-v2}{Harvest APIv2}.
#' @param query A query.
#' @inheritParams get_request
#' @inheritParams get_requests
#'
#' @examples
#'
#' \dontrun{
#' user_id <- 'your_user_id'
#' api_key <- paste0('Bearer ','you_api_key')
#' get_table(table = 'projects', user = user_id,key = api_key)
#' get_table(table = 'time_entries' , user = user_id, key = api_key,
#'   email = 'your_email',query = list(from = '2018-01-01', to = '2018-03-31'))
#' }
#' @author Mark Druffel, \email{mdruffel@propellerpdx.com}
#' @references
#' \url{https://id.getharvest.com/developers}
#' \url{https://help.getharvest.com/api-v2}
#'
#' @import httr
#' @importFrom magrittr %>%
#'
#' @export
#'

get_table <- function(
  table = NULL,
  user = NULL,
  key = NULL,
  email = '',
  query=NULL,
  auto_retry = FALSE,
  strategy = 'sequential',
  verbose=FALSE){


  if(!is.null(key) && !grepl("^Bearer ", key))
    key <- paste0('Bearer ', key)

  if(!is.null(query$return_df)){
    return_df <- query[[length(query)]]
    query <- query[[1:(length(query)-1)]]}

  if(is.null(query$page)){query$page <- '1'}

  url <- paste0('v2/',table) %>% httr::modify_url(url="https://api.harvestapp.com",path=.,query=query)

  # The curl version installed on EC2 / Lambda is slightly different from local.
  # Harvest's API responds that it can support HTTP/2 when it doesn't seem to be able to
  # Due to curl version or possibly some other factor, local curl figures this out and gets by it, using HTTP/1.1 while the one on EC2 does not and uses HTTP/2, resulting in failure.
  # http_version=2 forces HTTP/1.1
  # Full http_version options can be seen here https://github.com/curl/curl/blob/master/include/curl/curl.h by searching for "http_version"
  # Full httr docs https://cran.r-project.org/web/packages/httr/httr.pdf

  response_df <- url %>%
    purrr::map(., function(x) httr::with_config(config=config(verbose=verbose, http_version=2), httr::GET(x,httr::add_headers("Harvest-Account-ID" = user,Authorization = key,'User-Agent=Propeller R API Helper (mdruffel@propellerpdx.com)', 'From' = email), query = query))) %>%
    purrr::map(., function(x) httr::content(x, as="text", encoding = "UTF-8")) %>%
    purrr::map(., function(x) jsonlite::fromJSON(x, flatten = T))

  next_page <- response_df %>% purrr::map('next_page')

  if(!exists('return_df')){return_df <- NULL}
  return_df <- response_df %>%
    purrr::map(.,paste0(table)) %>%
    purrr::flatten_df(.) %>%
    dplyr::bind_rows(.,return_df)

  query$page <- paste0(unlist(next_page))
  if(is.null(unlist(next_page))){return(return_df)}else{
    query <- list(query=query,return_df=return_df)

    if(response_df %>% purrr::map_dbl(.,'total_pages')>99){naptime::naptime(.1)}

    get_table(table=table,user=user,key=key,email=email,query=query)}
}
