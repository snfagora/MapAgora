#' Extract contact webpage content
#'
#' @param page_url A webpage URL
#'
#' @return out A dataframe that contains email contact
#' @importFrom stringr str_extract_all
#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom httr set_config
#' @importFrom httr status_code
#' @importFrom rvest html_text
#' @importFrom rvest read_html
#' @importFrom purrr is_empty
#' @export

get_email_contact_from_webpage <- function(page_url) {
  email_pattern <- "\\b[A-Za-z0-9._%+-]+(?:\\[@\\]|\\sat\\s)[A-Za-z0-9.-]+(?:\\[dot\\]|[.])(?:[A-Za-z]{2,}|[A-Za-z]{4,})\\b"

  # Set the timeout to 3 seconds
  httr::set_config(timeout(3))

  # Try to retrieve the page content and handle potential errors
  tryCatch({
    # Make the HTTP GET request with the specified timeout
    response <- GET(page_url, type = "text/html; charset=iso-8859-1")

    # Check the status of the response
    if (status_code(response) == 200) {
      # Extract the content from the response
      page_content <- content(response, as = "text")

      if (is_empty(page_content)) {
        return(data.frame("page_url" = page_url, "email_contact" = NA))
      }

      webpage <- html_text(page_content)

      emails <- unique(str_extract_all(webpage, email_pattern)[[1]])

      if (length(emails) == 0) {
        return(data.frame("page_url" = page_url, "email_contact" = NA))
      }

      out <- data.frame("page_url" = page_url, "email_contact" = emails)

      return(out)
    } else {
      # If the status code is not 200 (e.g., 404, 500), return NA
      return(data.frame("page_url" = page_url, "email_contact" = NA))
    }
  }, error = function(e) {
    # Handle any errors (e.g., invalid URLs, connection issues)
    return(data.frame("page_url" = page_url, "email_contact" = NA))
  })
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
