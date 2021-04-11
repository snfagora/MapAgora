
#' Parse Facebook handles from a webpage
#'
#' @param website_address An website address of an organization
#' @param google_search_results The default is NULL. If you use a Google search result, use a non-NULL value.
#'
#' @return A list of facebook pages
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

parse_facebook_page_from_page <- function(website_address, google_search_results = NULL) {

    # Whether using Google search
    if (is.null(google_search_results) == TRUE) {
        # Read HTML from website_address
        pg <- read_html(website_address)

        response <- GET(website_address, config(ssl_verifypeer = FALSE, timeout = 10, followlocation = TRUE))

        possible_read <- possibly(read_html, otherwise = "This URL is broken.")

        pg <- possible_read(response)

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

    facebook_page <- all_links %>%
        # Find hrefs associated with twitter
        filter(grepl("facebook.com/", href) &
                   !grepl("/sharer/", href) & # not the org fb page but a link to share the url
                   !grepl("/posts/", href) & # not the org fb page but a post they shared
                   !grepl("facebook.com/wix", href)) %>% # This is the Wix website dev default
        # Distinct hrefs
        distinct(href) %>%
        pull(href) %>%
        str_replace_all("https://twitter.com/|http://twitter.com/", "")


    # Find whether there's no associated Facebook page
    facebook_page <- ifelse(length(facebook_page) == 0, NA, facebook_page)

    if (is.na(facebook_page) != TRUE) {

        # Remove extra stuff
        ## Separate by a special character then select the substring appears before the special character
        facebook_page <- ifelse(str_detect(facebook_page, "[?]") == TRUE,
                                strsplit(facebook_page, "?", fixed = TRUE)[[1]][1], facebook_page
        )

        # Error patterns
        error_patterns <- c("#|@|!")

        ## Remove certain first elements of the string vector
        facebook_page <- ifelse(str_detect(facebook_page, error_patterns),
                                gsub("^.", "", facebook_page), facebook_page
        )

        return(facebook_page)
    } else {
        return(facebook_page)
    }
}

#' Parse Facebook handles from an organization website
#'
#' @param website_address An website address of an organization
#'
#' @return A list of Facebook handles
#' @export

find_facebook_page_from_org_page <- function(website_address) {
    facebook_page <- parse_facebook_page_from_page(website_address)

    return(facebook_page)
}
