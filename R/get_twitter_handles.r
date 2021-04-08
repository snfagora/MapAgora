
#' Find Twitter handles using Bing API
#'
#' @param org_name An organization name
#' @param bing_search_api_key A Bing search API key (Note that the base URL is "https://api.cognitive.microsoft.com/bing/v7.0/search")
#'
#' @return A list of Twitter handles
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_detect
#' @importFrom httr GET
#' @importFrom glue glue
#' @importFrom httr add_headers
#' @importFrom httr content
#' @importFrom purrr map
#' @export

find_twitter_handle_from_bing <- function(org_name,
                                          bing_search_api_key) {

    # Define search query
    search_term <- str_replace_all(glue("{org_name} twitter"), " ", "+")

    base_url <- "https://api.cognitive.microsoft.com/bing/v7.0/search"

    api_key <- Sys.getenv("bing_search_api_key")

    # Make a request to Bing Search API (v.7)
    resp <- GET(
        url = glue("{base_url}/?q={search_term}"),
        add_headers("Ocp-Apim-Subscription-Key" = api_key)
    )

    # Response object
    r <- content(resp)


    # Get the associated URLs
    urls <- r$webPages$value %>%
        map("url")

    # GET the Twitter handles
    twitter_handle <- urls[str_detect(urls, "twitter.com")] %>%
        unlist() %>%
        str_replace_all("https://twitter.com/", "") %>%
        tolower()

    return(twitter_handle)
}

#' Parse Twitter handles from a webpage
#'
#' @param website_address An website address of an organization
#' @param google_search_results The default is NULL. If you use a Google search result, use a non-NULL value.
#'
#' @return A list of Twitter handles
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes
#' @importFrom rvest html_attr
#' @importFrom rvest html_text
#' @importFrom dplyr filter
#' @importFrom dplyr distinct
#' @importFrom dplyr pull
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_detect
#' @export

parse_twitter_handle_from_page <- function(website_address, google_search_results = NULL) {

    # Whether using Google search
    if (is.null(google_search_results) == TRUE) {
        # Read HTML from website_address
        pg <- read_html(website_address)
    } else {
        pg <- google_api_results # Read HTML from Google search results
    }

    # HTML links
    hrefs <- pg %>%
        html_nodes("a") %>%
        html_attr("href")

    # Link texts
    link_texts <- pg %>%
        html_nodes("a") %>%
        html_text()

    # Combined them as a data.frame
    all_links <- data.frame(
        href = tolower(hrefs), # lower cases
        link_text = tolower(link_texts)
    ) # lower cases

    twitter_handle <- all_links %>%
        # Find hrefs associated with twitter
        filter(grepl("twitter.com", href) &
                   !grepl("/status/", href) & # about network connections
                   !grepl("/share\\?", href) & # not the org twitter handle but a post they shared
                   !grepl("intent/tweet\\?", href) & # not the org twitter handle but a post they shared
                   !grepl("twitter.com/wix", href)) %>% # This is the Wix website dev platform
        # Distinct hrefs
        distinct(href) %>%
        pull(href) %>%
        str_replace_all("https://twitter.com/|http://twitter.com/", "")

    # Find whether there's no associated Twitter handle
    twitter_handle <- ifelse(length(twitter_handle) == 0, NA, twitter_handle)

    if (is.na(twitter_handle) != TRUE) {

        # Remove extra stuff
        ## Separate by a special character then select the substring appears before the special character
        twitter_handle <- ifelse(str_detect(twitter_handle, "[?]") == TRUE,
                                 strsplit(twitter_handle, "?", fixed = TRUE)[[1]][1], twitter_handle
        )

        # Error patterns
        error_patterns <- c("#|@|!")

        ## Remove certain first elements of the string vector
        twitter_handle <- ifelse(str_detect(twitter_handle, error_patterns),
                                 gsub("^.", "", twitter_handle), twitter_handle
        )

        return(twitter_handle)
    } else {
        return(twitter_handle)
    }
}

#' Parse Twitter handles from an organization website
#'
#' @param website_address An website address of an organization
#'
#' @return A list of Twitter handles
#' @export

find_twitter_handle_from_org_page <- function(website_address) {
    twitter_handle <- parse_twitter_handle_from_page(website_address)

    return(twitter_handle)
}

#' Find Twitter handles either using a name (default strategy) or an website of an organization.
#'
#' @param org_name A name of an organization
#' @param website_address An website of an organization. The default value is NULL.
#'
#' @return If no website information were provided, the function returns the Twitter handles discovered via Bing API. If website information were provided, the function returns a dataframe that contains Twitter handles discovered by Bing API ("Bing" column), the website ("Website" column), and the intersection of the two search results ("Intersection" column).
#' @importFrom purrr reduce
#' @export

find_all <- function(org_name, website_address = NULL) {

    # Using Bing Search API (v.7)
    tw_bing <- find_twitter_handle_from_bing(org_name)

    # Using Organizational Websites
    if (is.null(website_address) == FALSE) {
        tw_org <- find_twitter_handle_from_org_page(website_address)
    } else {
        tw_org <- NULL
    }

    # Combined output
    if (is.null(tw_org) == TRUE) {
        out <- tw_bing
    }

    else {
        out <- list(
            "Bing" = tw_bing,
            "Website" = tw_org,
            "Intersection" = reduce(list(tw_bing, tw_org), intersect)
        )
    }

    return(out)
}
