#' Extract contact webpage content
#'
#' @param page_url A webpage URL
#'
#' @return out A dataframe that contains email contact
#' @importFrom stringr str_detect
#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom rvest read_html
#' @importFrom purrr is_empty
#' @export

get_email_contact_from_webpage <- function(page_url) {

  # Define the three email patterns
  email_pattern_1 <- "\\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}\\b"
  email_pattern_2 <- "\\b[A-Za-z0-9._%+-]+ at [A-Za-z0-9.-]+ dot [A-Za-z]{2,}\\b"
  email_pattern_3 <- "\\b[A-Za-z0-9._%+-]+\\[at\\][A-Za-z0-9.-]+\\[dot\\][A-Za-z]{2,}\\b"

  page_content <- content(GET(page_url), type = "text/html; charset=iso-8859-1")

  # make sure URL exists
  if (is.na(page_content)) {

    out <- data.frame(
      "page_url" = page_url,
      "email_contact" = NA
    )

    return(out)

  }

  webpage <- html_text(page_content)

  email1 <- regmatches(webpage, gregexpr(email_pattern_1, webpage))

  email2 <- regmatches(webpage, gregexpr(email_pattern_2, webpage))

  email3 <- regmatches(webpage, gregexpr(email_pattern_3, webpage))

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
#' @export

get_contact_links_from_website <- function(base_url) {
  # Check if the base_url starts with "http://" or "https://"
  if (!grepl("^https?://", base_url, ignore.case = TRUE)) {
    base_url <- paste0("http://", base_url)
  }

  # Extract the domain from the base_url
  domain_name <- sub("^https?://", "", base_url)
  domain_name <- sub("/.*", "", domain_name)

  # Create the contact URL
  contact_url <- paste0(base_url, "/contact")
  contact_url <- sub("//contact", "/contact", contact_url)

  # Return the base_url and contact_url
  contacts <- c(domain_name, contact_url)
  return(contacts)
}
