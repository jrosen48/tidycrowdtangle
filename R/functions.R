#' ct_get_links
#'
#' @param x A number
#' @param platforms What platforms
#' @return The sum of \code{x} and \code{y}
#' @examples
#' # add(1, 1)

ct_get_links <- function(x = "", platforms = "", count = 100, startDate = "", endDate = "", token = "")
{
  token <- Sys.getenv("CT_TOKEN")
  endpoint.links <- "https://api.crowdtangle.com/links"
  query.string <- paste0(endpoint.links, "?link=", x, "&platforms=", platforms, "&count=", count, "&startDate=", startDate, "&endDate=", endDate, "&token=", token)
  response.json <- try(jsonlite::fromJSON(query.string), silent = TRUE)
  if (!class(response.json) == "try-error")
  {
    status <- response.json$status
    postcount <- nrow(response.json$result$posts)
    if (status == 200 & !is.null(postcount))
    {
      nextpage <- response.json$result$pagination$nextPage
      posts <- response.json$result$posts
      if("expandedLinks" %in% colnames(posts)) posts <- dplyr::select(posts, -expandedLinks)
      if("media" %in% colnames(posts)) posts <- dplyr::select(posts, -media)
      posts <- jsonlite::flatten(posts)
      return(dplyr::as_tibble(posts))
    }
    else if (status == 429)
    {
      print("API rate limit hit, sleeping...")
      Sys.sleep(60)
    }
  }
}

#ct_get_links(searchTerm = "school")

#' ct_get_posts
#'
#' @param x A number
#' @param platforms What platforms
#' @return The sum of \code{x} and \code{y}
#' @importFrom dplyr "%>%
#' @examples
#' # add(1, 1)

ct_get_posts <- function(listID = "", searchTerm = "", language = "",
                         types= "", minInteractions = 0, count = 100,
                         startDate = "", endDate = "", token = "")
{
  token <- Sys.getenv("CT_TOKEN")
  endpoint.posts <- "https://api.crowdtangle.com/posts"
  query.string <- paste0(endpoint.posts, "?listIds=", listID, "&searchTerm=", searchTerm, "&language=", language, "&types=", types, "&minInteractions=", minInteractions, "&count=", count, "&startDate=", startDate, "&endDate=", endDate, "&token=", token)
  response.json <- try(jsonlite::fromJSON(query.string), silent = TRUE)
  status <- response.json$status
  nextpage <- response.json$result$pagination$nextPage
  posts <- response.json$result$posts %>% dplyr::select(-expandedLinks, -media) %>% jsonlite::flatten()
  return(dplyr::as_tibble(posts))
}

# ct_get_posts(listID = "1461358", searchTerm = "school")

#' ct_search_posts
#'
#' @param x A number
#' @return The sum of \code{x} and \code{y}
#' @examples
#' # add(1, 1)

ct_search_posts <- function(x = "", and = "", or = "", not = "",
                            inAccountIds = "", inListIds = "", notInAccountIds = "",
                            notInListIds = "", notInTitle = "", platforms = "",
                            types= "", minInteractions = 0, minSubscriberCount = 0,
                            verifiedOnly = "false",  count = 100, startDate = "",
                            language = "en", endDate = "", token = "")
{
  token <- Sys.getenv("CT_TOKEN")
  endpoint.posts <- "https://api.crowdtangle.com/posts"
  query.string <- paste0(endpoint.posts,
                         "?searchTerm=", x,
                         "&token=", token,
                         # "&and=", and,
                         # "&or=", or,
                         "&inAccountIds=", inAccountIds
                         # "&inListIds=", inListIds,
                         # "&language=", language, "&types=", types, "&minInteractions=", minInteractions, "&count=", count, "&startDate=", startDate, "&endDate=", endDate, "&token=", token
                         )
  response.json <- try(jsonlite::fromJSON(query.string), silent = TRUE)
  status <- response.json$status
  nextpage <- response.json$result$pagination$nextPage
  posts <- response.json$result$posts %>% dplyr::select(-expandedLinks, -media) %>% jsonlite::flatten()
  return(posts %>% dplyr::as_tibble())
}

ct_search_posts(x = "school")
