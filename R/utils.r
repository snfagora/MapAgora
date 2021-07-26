#' Check data availability for the individual source
#'
#' @param ein An Employment Identification Numbers
#' @param year A year in which a form was filed. The default value is 2019.
#' @param source The data source. Four options exist: "irs", "website", "twitter", "facebook."
#' @return A datafram that contains four columns: EIN, its data source, availability, and the index year when it became available. Data availability column is a dummy variable. 1 = data exist. 0 = data don't exist.
#' @export
#'

check_data_availability <- function(ein, year = 2019, source = c("irs", "website", "social_media")) {

  return <- ifelse(sum(class(get_990("311810938")) %in% c("XMLNode")) >= 1, 1, 0)

  if (source == "irs") {
    out <- data.frame(
      "EIN" = ein,
      "Source" = source,
      "Availability" = return,
      "Year" = year
      )
  }

  if (source == "website") {

    website <- get_value_990(get_990(ein = ein, year), "website")

    out <- data.frame(
      "EIN" = ein,
      "Source" = source,
      "Availability" = ifelse(is.na(website), 0, 1),
      "Year" = year)
  }

  if (source == "twitter") {

    website <- get_value_990(get_990(ein = ein, year), "website")

    out <- data.frame(
      "EIN" = ein,
      "Source" = source,
      "Availability" = ifelse(is.na(website), 0, ifelse(is.na(find_twitter_handle_from_org_page(website)), 0, 1),
       "Year" = year))

  }

  if (source == "facebook") {

    website <- get_value_990(get_990(ein = ein, year), "website")

    out <- data.frame(
      "EIN" = ein,
      "Source" = source,
      "Availability" = ifelse(is.na(website), 0, ifelse(is.na(find_twitter_handle_from_org_page(website)), 0, 1)),
      "Year" = year)

  }

  return(out)

}

#' Check data availability for the combined source
#'
#' @param ein An Employment Identification Numbers
#' @param year A year in which a form was filed. The default value is 2019.
#' @param source The data source. Four options exist: "irs", "website", "twitter", "facebook."
#' @importFrom purrr map_dfr
#' @return A datafram that contains three columns: EIN, its data source and availability. Data availability column is a dummy variable. 1 = data exist. 0 = data don't exist.
#' @export
#'

check_data_all <- function(ein, year = 2019) {

  sources <- c("irs", "website", "twitter", "facebook")

  out <- map_dfr(sources, ~check_data_availability(ein, year, .))

  return(out)
}
