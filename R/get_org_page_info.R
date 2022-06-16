
#' Check whether the href attribute contains a user-defined "search_term"
#'
#' @param href An href attribute
#'
#' @return Either an href attribute 1 (``YES``) or NA (``NA``)
#' @importFrom stringr str_detect
#' @export

if_not_search_term <- function(href, search_term) {

    if (TRUE %in% (href %>%
                   tolower() %>%
                   str_detect(search_term) == TRUE)) {
        return(1)
    } else {
        return(NA)
    }

}

#' Find a possible link based on a base URL
#'
#' @param base_url  base URL (the base part of the web address)
#' @param href possible_link Possible link
#'
#' @return If successful, the function returns a dataframe of three columns ("href", "link_text", "href"). href is "base." link_text is "Found without tree search." href is the URL. If not successful, the result is NA.
#' @importFrom stringr str_detect
#' @importFrom tidyr tibble
#' @importFrom RCurl curlOptions
#' @importFrom RCurl url.exists
#' @importFrom glue glue
#' @export

find_possible_link <- function(base_url, possible_link) {

    if (!grepl("/$", base_url)) {
        base_url <- glue("{base_url}/")
    }

    opts <- curlOptions(
        # Follow redirections
        followlocation = TRUE,

        # Set a timeout for a request
        timeout = 40,
        useragent = "R App",
        referer = "https://google.com",
        failonerror = FALSE
    )

    # Form a possible URL
    possible_url <- glue("{base_url}{possible_link}")

    # Check whether a request for the specific URL works without error
    if (url.exists(possible_url, .opts = opts)) {
        about_links <- tibble(
            href = "Base",
            link_text = "Found without tree search.",
            link = possible_url
        )

        return(about_links)

    } else {

        return(NA)

    }

}

#' Get links base on base URL
#'
#' @param base_url A base URL (the base part of the web address)
#'
#' @return The function returns a dataframe of three columns ("href", "link_text", and "link"). If the function runs into an error getting the links based on the base URL, the value of the href column should be NA. The link_text will inform the error case. This should be either "This website is broken.", "The website is flat (no tree structure)." or "The website does not have about page."
#' @importFrom httr GET
#' @importFrom httr config
#' @importFrom purrr possibly
#' @importFrom tidyr tibble
#' @importFrom rvest html_nodes
#' @importFrom rvest html_attr
#' @importFrom rvest html_text
#' @export

get_links <- function(base_url) {

    response <- GET(base_url,
                    config(ssl_verifypeer = FALSE,
                           timeout = 10,
                           followlocation = TRUE))

    possible_read_html <- possibly(read_html, otherwise = "This URL is broken.")

    pg <- possible_read_html(response)

    if ("xml_document" %in% class(pg) == FALSE) {

        # Dataframe with three columns
        about_links <- tibble(
            href = NA,
            link_text = "This website is broken.",
            link = base_url
        )

    } else {

        # URL of pages
        href <- pg %>%
            html_nodes("a") %>%
            html_attr("href")

        # Alternative
        link_text <- pg %>%
            html_nodes("a") %>%
            html_text()

        if (length(href) == 0) {
            # Data frame with three columns

            about_links <- tibble(
                href = NA,
                link_text = "The website is flat (no tree structure).",
                link = base_url
            )

        } else {

            about_links <- tibble(
                href = href,
                link_text = link_text,
                link = rep(base_url, length(href))
            )

        }
    }

    return(about_links)

}

#' Check whether the href attribute contains "search_term" or not
#'
#' @param about_links A dataframe of three columns ("href", "link_text", and "link").
#' @param search_term A search term.
#' @param base_url A base URL (the base part of the web address)
#'
#' @return A dataframe of three columns ("href", "link_text", and "link") filtered by a search term and cleaned for later use in the workflow.
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace_all
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr distinct
#' @importFrom dplyr group_by
#' @importFrom dplyr slice
#' @importFrom dplyr ungroup
#' @importFrom glue glue
#' @export

find_search_term <- function(about_links, search_term, base_url="http://example.com") {

    if (sum(c(is.na(if_not_search_term(df$href, search_term)),
              is.na(if_not_search_term(tolower(df$link_text), search_term)))) < 2) {

        res <- about_links %>%
            filter(str_detect(tolower(link_text), search_term) |
                       str_detect(tolower(href), search_term)) %>%
            filter(!is.na(href)) %>%
            distinct() %>%
            mutate(href = ifelse(str_detect(href, "http.{0,1}://"), href,
                                 str_replace_all(href, "/", ""))) %>%
            mutate(link = ifelse(str_detect(href, "http.{0,1}://"), href,
                                 glue("{base_url}{href}"))) %>%
            mutate(link_text = str_replace_all(link_text,"[\n|\t]","")) %>%
            group_by(link) %>%
            slice(1) %>%
            ungroup()

        return(res)
    } else {
        return(NA)
    }
}

#' Search all activities
#'
#' @param base_url A base URL (the base part of the web address)
#'
#' @return A data.frame that contains the binary outcomes of the 13 search results: donations, events, membership, digital membership, chapters, volunteers, services, taking actions, advocacy, visits, resources, boards, and press.
#' @importFrom stringr str_detect
#' @export

search_all_activities <- function(base_url) {

    df <- get_links(base_url)

    # ACCEPTS DONATIONS

    donations <- 0

    if ( !max(is.na(find_search_term(df,"donate")))) {
        donations <- 1
    } else if (!max(is.na(find_search_term(df,"give")))) {
        donations <- 1
    } else if (!max(is.na(find_search_term(df,"contribute")))) {
        donations <- 1
    } else if (!max(is.na(find_search_term(df,"support us")))) {
        donations <- 1
    }

    # HOLDS EVENTS

    events <- 0

    if ( !max(is.na(find_search_term(df,"events")))) {
        events <- 1
    } else if (!max(is.na(find_search_term(df,"calendar")))) {
        events <- 1
    } else if (!max(is.na(find_search_term(df,"meeting")))) {
        events <- 1
    }

    # HAS MEMBERSHIP

    membership <- 0

    if ( !max(is.na(find_search_term(df,"(?<![a-zA-Z])join")))) {
        membership <- 1
    } else if (!max(is.na(find_search_term(df,"member")))) {
        membership <- 1
    } else if (!max(is.na(find_search_term(df,"sign up")))) {
        membership <- 1
    }

    # HAS "DIGITAL MEMBERSHIP"

    digital_membership <- 0

    if ( !max(is.na(find_search_term(df,"sign up")))) {
        membership <- 1
    } else if (!max(is.na(find_search_term(df,"subscribe")))) {
        membership <- 1
    } else if (!max(is.na(find_search_term(df,"(?<![a-zA-Z])join")))) {
        membership <- 1
    }

    # HAS NEWSLETTER

    newsletter <- 0

    if ( !max(is.na(find_search_term(df,"newsletter")))) {
        newsletter <- 1
    } else if (!max(is.na(find_search_term(df,"bulletin")))) {
        newsletter <- 1
    }

    # HAS CHAPTERS

    chapters <- 0

    if ( !max(is.na(find_search_term(df,"chapter")))) {
        chapters <- 1
    }

    # RECRUITS VOLUNTEERS

    volunteer <- 0

    if ( !max(is.na(find_search_term(df,"volunteer")))) {
        volunteer <- 1
    } else if (!max(is.na(find_search_term(df,"get involved")))) {
        volunteer <- 1
    } else if (!max(is.na(find_search_term(df,"getinvolved")))) {
        volunteer <- 1
    }

    # PROVIDES SERVICES

    services <- 0

    if ( !max(is.na(find_search_term(df,"services")))) {
        services <- 1
    } else if (!max(is.na(find_search_term(df,"get help")))) {
        services <- 1
    } else if (!max(is.na(find_search_term(df,"gethelp")))) {
        services <- 1
    }

    # TAKE ACTIONS

    take_action <- 0

    if ( !max(is.na(find_search_term(df,"take action")))) {
        take_action <- 1
    } else if ( !max(is.na(find_search_term(df,"takeaction")))) {
        take_action <- 1
    } else if ( !max(is.na(find_search_term(df,"justice")))) {
        take_action <- 1
    } else if ( !max(is.na(find_search_term(df,"social(.?)action")))) {
        take_action <- 1
    }

    # BE INVOLVED IN ADVOCACY/LEGISLATION

    advocacy <- 0

    if ( !max(is.na(find_search_term(df,"legislat")))) {
        advocacy <- 1
    } else if (!max(is.na(find_search_term(df,"election")))) {
        advocacy <- 1
    } else if (!max(is.na(find_search_term(df,"endorsement")))) {
        advocacy <- 1
    } else if (!max(is.na(find_search_term(df,"issues")))) {
        advocacy <- 1
    } else if (!max(is.na(find_search_term(df,"campaigns")))) {
        advocacy <- 1
    } else if (!max(is.na(find_search_term(df,"(?<![a-zA-Z])petition")))) { #e.g. not competition
        advocacy <- 1
    }

    # VISIT/LOCATIONS

    visit <- 0

    if ( !max(is.na(find_search_term(df,"visit")))) {
        visit <- 1
    } else if (!max(is.na(find_search_term(df,"location")))) {
        visit <- 1
    }

    # RESOURCES

    resources <- 0

    if ( !max(is.na(find_search_term(df,"resource")))) {
        resources <- 1
    } else if (!max(is.na(find_search_term(df,"education")))) {
        resources <- 1
    } else if (!max(is.na(find_search_term(df,"publication")))) {
        resources <- 1
    } else if (!max(is.na(find_search_term(df,"learning")))) {
        resources <- 1
    } else if (!max(is.na(find_search_term(df,"reports")))) {
        resources <- 1
    }

    # BOARD

    board <- 0
    if ( !max(is.na(find_search_term(df,"board")))) {
        board <- 1
    }

    # PRESS

    press <- 0

    if ( !max(is.na(find_search_term(df,"(?<![a-zA-Z])press")))) {
        press <- 1
    } else if (!max(is.na(find_search_term(df,"(?<!social(.?))media")))) {
        press <- 1
    }

    # Putting these outcomes together

    res <- data.frame(donations = donations,
                      events = events,
                      membership = membership,
                      digital_membership = digital_membership,
                      chapters = chapters,
                      volunteer = volunteer,
                      services = services,
                      take_action = take_action,
                      advocacy = advocacy,
                      visit = visit,
                      resources = resources,
                      board = board,
                      press = press)

    return(res)

}

#' Extract search term from links
#'
#' @param base_url A base URL (the base part of the web address)
#' @param possible_links A possible link
#' @param search_terms A search term
#'
#' @return @return If successful, the function returns a dataframe of three columns ("href", "link_text", "href"). href is "base." link_text is "Found without tree search." href is the URL. If not successful, the result is NA.
#' @export
