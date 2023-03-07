#' Check whether the href attribute contains "contact" or not
#'
#' @param href An href attribute
#'
#' @return Either an href attribute (YES) or NA (NO)
#' @importFrom stringr str_detect
#' @export

if_not_contact <- function(href) {
  if ((TRUE %in% (href %>%
    tolower() %>%
    str_detect("contact"))) &
    (any(href %>%
      tolower() %>%
      str_detect("contact") == TRUE))) {
    return(href)
  } else {
    return(NA)
  }
}

#' Extract links and other information related to contact page
#'
#' @param base_url A base URL (the base part of the web address)
#' @param timeout_thres A timeout threshold. The default value is 10 seconds.
#'
#' @return If successful, the function returns a dataframe of two columns ("href", "link"). If not successful, the function returns a dataframe of three columns ('href', 'link_text', link'). In this dataframe, href should be NA and link_test should inform one of the following five error cases: "Found without tree search.", "This website is broken.", "The website is flat (no tree structure).", "PHP error", or "The website does not have contact page."
#' @importFrom stringr str_detect
#' @importFrom stringr str_match
#' @importFrom stringr str_remove
#' @importFrom stringr str_count
#' @importFrom stringr str_replace
#' @importFrom stringr str_replace_all
#' @importFrom glue glue
#' @importFrom tibble tibble
#' @importFrom purrr possibly
#' @importFrom purrr is_empty
#' @importFrom furrr future_map_int
#' @importFrom rvest html_nodes
#' @importFrom rvest html_attr
#' @importFrom rvest html_text
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom dplyr distinct
#' @importFrom dplyr mutate
#' @importFrom xml2 read_html
#' @importFrom xml2 url_absolute
#' @importFrom jsonlite fromJSON
#' @importFrom httr parse_url
#' @importFrom httr GET
#' @export
#'

extract_contact_links <- function(base_url, timeout_thres = 10) {

  # Timeout to prevent hanging on unreachable/very slow websites
  if (url_exists(base_url, timeout_thres = timeout_thres) == FALSE) {
    stop(glue("This URL is not responding ({timeout_thres} seconds timeout)."))
  }

  response <- GET(base_url, config(ssl_verifypeer = FALSE, timeout = 10, followlocation = TRUE))

  # no-encoding issues from the server
  possible_read <- possibly(read_html, otherwise = "This URL is broken.")

  # encoding issues from the server
  possible_content <- possibly(~ content(., encoding = "ISO-8859-1"), otherwise = "This URL is broken.")

  pg <- if (!str_detect(base_url, ".asp")) {
    possible_read(response)
  } else {
    possible_content(response)
  }

  # check if page is broken
  if ("xml_document" %in% class(pg) == FALSE) {
    # Dataframe with three columns
    contact_links <- tibble(
      href = NA,
      link_text = "This website is broken.",
      link = base_url
    )
  }
  # else check whether the website is built by Wix
  if (TRUE %in% grepl(
    "Wix.com",
    html_nodes(pg, "meta") %>%
      html_attr("content")
  )) {
    emailjs <- pg %>%
      html_nodes("script") %>%
      html_text()

    script_idx <- which(grepl("rendererModel", emailjs))

    res <- str_match(emailjs[script_idx], "var rendererModel =\\s*(.*?)\\s*;")

    if (!is_empty(res)) {
      res <- fromJSON(res[2])

      page_list <- res$pageList$pages

      contact_pages <- page_list %>%
        filter(grepl("contact", tolower(title)) | grepl("contact", tolower(pageUriSEO))) %>%
        select(pageUriSEO)

      # Dataframe with three columns
      contact_links <- tibble(
        href = "Base",
        link_text = "The website is built by Wix.",
        link = contact_pages <- glue("{base_url}/{contact_pages}")
      )
    }
  }

  # else look through links on page
  if ("xml_document" %in% class(pg)) {

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
      contact_links <- tibble(
        href = NA,
        link_text = "The website is flat (no tree structure).",
        link = base_url
      )
    }

    # Dataframe with three columns
    else if (prod(c(is.na(if_not_contact(tolower(href))), is.na(if_not_contact(tolower(link_text))))) > 0) {

      # try checking directly before giving up
      # If base url includes either index.html or index.php (or other similar cases)
      suffix <- str_replace(base_url, "^.*/", "")

      if (suffix %>% str_detect("\\.")) {
        # Going up to the host level
        base_url <- base_url %>%
          str_replace(paste0(suffix, "$"), "")
      }

      if (!grepl("/$", base_url)) {
        base_url <- glue("{base_url}/")
      }

      possible_contact_urls <- c(
        glue("{base_url}contact"), # contact case 1
        glue("{base_url}contact-us"), # contact case 2
        )

      # Check whether a request for the specific URL works without error
      if (sum(future_map_int(possible_contact_urls, ~ url_exists(., timeout_thres = timeout_thres))) >= 1) {

        # Dataframe with three columns
        contact_links <- tibble(
          href = "Base",
          link_text = "Found without tree search.",
          link = possible_contact_urls[which(future_map_int(possible_contact_urls, ~ url_exists(., timeout_thres = timeout_thres)) == 1)]
        )
      } else {

        # Data frame with three columns
        contact_links <- tibble(
          "href" = NA,
          "link_text" = "The website does not have contact page.",
          "link" = base_url
        )
      }
    } else {

      # contact link found
      df <- tibble(
        "href" = if_not_contact(href), # Don't make it lower case (URLs are case sensitive)
        "link_text" = if_not_contact(tolower(link_text)),
        "link" = base_url
      )

      contact_links <- df %>%
        filter(str_detect(tolower(link_text), "contact") |
          str_detect(tolower(href), "contact")) %>%
        filter(!is.na(href)) %>%
        distinct() %>%
        arrange(desc(str_detect(tolower(link_text), "contact") + str_detect(tolower(href), "contact")))

      # To make link formatting not off when page uses an absolute reference
      contact_links <- contact_links %>%
        mutate(link = url_absolute(href, base_url)) %>%
        select(href, link) %>%
        distinct(link, .keep_all = TRUE)

      return(contact_links)
    }
  }

  return(contact_links)
}

#' Find links and other information related to contact page
#'
#' @param base_url A base URL (the base part of the web address)
#'
#' @return If successful, the function returns a working base URL. If not, the function reports one of the following four error cases: "This link does not have contact page.", "This link has a PHP error.", "This link is flat (not tree structure).", or "This link is broken."
#' @importFrom glue glue
#' @importFrom dplyr if_else
#' @importFrom dplyr pull
#' @importFrom dplyr first
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace
#' @importFrom xml2 url_absolute
#' @export

find_contact_link <- function(base_url) {
  contact_links <- extract_contact_links(base_url)

  # Find cases where links are broken or links don't have contact pages
  if (NA %in% contact_links$href == TRUE) {

    # if_else is slightly faster than ifelse
    contact_url <- if_else(str_detect(contact_links$link_text, "have contact page"), # Contact page problem
      glue("This {contact_links$link} does not have contact page."),
      if_else(str_detect(contact_links$link_text, "PHP"), # PHP problem
        glue("This {contact_links$link} has a PHP error."),
        if_else(str_detect(contact_links$link_text, "(no tree structure)"), # Flat tree problem
          glue("This {contact_links$link} is flat (not tree structure)."),
          glue("This {contact_links$link} is broken.") # Broken URL problem
        )
      )
    )
  } else {
    contact_url <- contact_links %>%
      pull("link") %>%
      unique() %>%
      first()
  }

  # contact_url
  return(contact_url)
}

#' Extract contact page content
#'
#' @param contact_url An URL for an contact page
#'
#' @return contact page text (character vector)
#' @importFrom htm2txt gettxt
#' @export

extract_contact_page_content <- function(contact_url) {

  # Broken, flat, PHP error, contact page issues
  if (str_detect(contact_url, "tree search|is broken|is flat|PHP error|contact page") == TRUE) {

    # Output: dataframe
    contact_url_text <- contact_url
  }

  # Other cases
  else {
    contact_page <- gettxt(contact_url) #%>%
      #strip(digit.remove = FALSE) # more comprehensive and efficient text cleaning

    # Output: dataframe
    contact_url_text <- contact_page
  }

  return(contact_url_text)
}

#' Get contact page content
#'
#' @param base_url A base URL (the base part of the web address)
#'
#' @return contact_page A dataframe that contains email contact
#' @importFrom stringr str_detect
#' @export

get_contact_page_content <- function(base_url) {

  # Search prospective contact URLs
  contact_url <- find_contact_link(base_url)

  # Extract information
  contact_url_text <- extract_contact_page_content(contact_url)

  # Close all URL connections
  on.exit(closeAllConnections())

  contact_url_text <- contact_url_text %>%
    strsplit("\n") %>%
    unlist()

  email_contact <- unique(contact_url_text[str_detect(contact_url_text, pattern = "@")])

  contact_page <- data.frame("email_contact" = email_contact)

  return(contact_page)
}
