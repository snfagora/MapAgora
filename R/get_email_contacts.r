#' Extract contact webpage content
#'
#' @param page_url A webpage URL
#'
#' @return out A dataframe that contains email contact
#' @importFrom stringr str_detect
#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom httr content
#' @importFrom rvest read_html
#' @importFrom purrr is_empty
#' @export

get_email_contact_from_webpage <- function(page_url) {

  # Define the three email patterns
  email_pattern_1 <- "\\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}\\b"
  email_pattern_2 <- "\\b[A-Za-z0-9._%+-]+ at [A-Za-z0-9.-]+ dot [A-Za-z]{2,}\\b"
  email_pattern_3 <- "\\b[A-Za-z0-9._%+-]+\\[at\\][A-Za-z0-9.-]+\\[dot\\][A-Za-z]{2,}\\b"

  page_content <- content(GET(page_url), as = "text")

  webpage <- read_html(page_content)

  email1 <- regmatches(page_content, gregexpr(email_pattern_1, page_content))

  email2 <- regmatches(page_content, gregexpr(email_pattern_2, page_content))

  email3 <- regmatches(page_content, gregexpr(email_pattern_3, page_content))

  emails <- unlist(c(email1, email2, email3))

  emails <- unique(na.omit(emails))

  emails <- emails[emails != ""]

  if (is_empty(emails))
  {
    out <- data.frame(
      "page_url" = page_url,
      "email_contact" = NA
      )
  } else {

    out <- data.frame(
      "page_url" = page_url,
      "email_contact" = emails
    )
  }

  return(out)
}

#' Extract child links associated with email contacts
#'
#' @param base_url A website URL
#'
#' @return out A character vector that contains child links associated with email contacts (i.e., page_urls)
#' @importFrom stringr str_detect
#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom rvest read_html
#' @importFrom rvest html_attr
#' @importFrom rvest html_nodes
#' @importFrom urltools domain
#' @importFrom urltools suffix_extract
#' @importFrom RCurl url.exists
#' @importFrom glue glue
#' @export

get_contact_links_from_website <- function(base_url) {

  # make sure it's a base URL
  correct_base_url <- suffix_extract(domain(base_url))$host

  # make sure URL exists
  if (!url.exists(correct_base_url)) {

    page_urls <- NA

    return(page_urls)

  }

  # collect all relevant child links associated with the base url
  page_content <- content(GET(correct_base_url), as = "text")

  webpage <- read_html(page_content)

  links <- html_attr(html_nodes(webpage, "a"), "href")

  # empty links
  links <- links[links != ""]

  # not child links
  links <- links[!str_detect(links, "http")]

  # only child pages
  links <- unique(links[grepl("^\\/[A-Za-z]", links)])

  # only one depth
  links <- links[!grepl("\\/.*\\/.*", links)]

  page_urls <- glue("{correct_base_url}{links}")

  return(page_urls)

}
