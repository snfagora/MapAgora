#' Get contact webpage content
#'
#' @param page_url A webpage URL
#'
#' @return out A dataframe that contains email contact
#' @importFrom stringr str_extract_all
#' @importFrom curl curl_fetch_memory
#' @importFrom httr content
#' @importFrom rvest read_html
#' @importFrom purrr is_empty
#' @importFrom glue glue
#' @export

get_email_contact_from_webpage <- function(page_url) {

  # Define the three email patterns
  email_pattern_1 <- "\\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}\\b"
  email_pattern_2 <- "\\b[A-Za-z0-9._%+-]+ at [A-Za-z0-9.-]+ dot [A-Za-z]{2,}\\b"
  email_pattern_3 <- "\\b[A-Za-z0-9._%+-]+\\[at\\][A-Za-z0-9.-]+\\[dot\\][A-Za-z]{2,}\\b"

  response <- curl::curl_fetch_memory(page_url)

  # Check if the request was successful
  if (response$status_code != 200) {
    out <- data.frame(
      "page_url" = page_url,
      "email_contact" = NA
    )
    return(out)
  }

  # Parse the raw content as HTML
  page_content <- read_html(response$content)

  # Extract the text from the HTML content
  webpage <- html_text(page_content)

  # make sure URL exists
  if (is_empty(page_content)) {

    out <- data.frame(
      "page_url" = page_url,
      "email_contact" = NA

    )

    return(out)

  }

  emails <- unlist(stringr::str_extract_all(webpage, glue("{email_pattern_1}|{email_pattern_2}|{email_pattern_3}")))

  emails <- tolower(unique(na.omit(emails)))

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

#' Get child links associated with email contacts
#'
#' @param base_url A website URL
#'
#' @return out A character vector that contains child links associated with email contacts (i.e., page_urls)
#' @export

get_contact_links_from_website <- function(base_url) {

  # Extract the domain from the base_url
  domain_name <- sub("^https?://", "", base_url)
  domain_name <- sub("/.*", "", domain_name)

  # Create the contact URL
  contact_url <- paste0(domain_name, "/contact")
  contact_url <- sub("//contact", "/contact", contact_url)

  # Create the contact URL
  contact_url2 <- paste0(domain_name, "/contact-us")
  contact_url2 <- sub("//contact", "/contact", contact_url2)

  # Return the contact_urls
  contacts <- c(contact_url1, contact_url2)
  return(contacts)
}

#' Get emails from a base url
#'
#' @param base_url A website URL
#' @importFrom memoise memoise
#'
#' @return out A dataframe that contains the page URLs and the email addresses appearing on these URLs
#' @importFrom furrr future_map_dfr
#' @export

get_emails_from_website <- function(base_url) {

  get_email_contact_from_webpage <- memoise::memoise(get_email_contact_from_webpage)

  get_contact_links_from_website <- memoise::memoise(get_contact_links_from_website)

  page_urls <- get_contact_links_from_website(base_url)

  out <- future_map_dfr(page_urls, get_email_contact_from_webpage)

  return(out)

  }
