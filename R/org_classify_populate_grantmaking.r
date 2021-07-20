#' Extract financial information from 990 forms
#'
#' @param xml_plucked A content element of the 990 XML file
#' @param variable A particular field in 990 forms
#'
#' @return If successful, the function returns the selected financial information from a 990 form in XML format (numeric data type).
#' @importFrom XML getNodeSet
#' @importFrom XML xmlValue
#' @importFrom readr parse_number
#' @export

extract_financial_info <- function(xml_plucked, variable){

  xml_plucked %>%
    getNodeSet(variable) %>%
    xmlValue() %>%
    parse_number()

}

#' Get financial details from 990 forms
#'
#' @param xml_root An XML root element associated with a particular organization
#'
#' @return If successful, the function returns a dataframe that contains information on an organization's "revenue," "assets," "liabilities," and "expenses."
#' @importFrom purrr pluck
#' @importFrom tibble tibble
#' @export

get_financial_details_990 <- function(xml_root){

  xml_plucked <- xml_root %>%
    pluck(2) # pick the second element on the list

  # Current year revenue
  revenue <- xml_plucked %>%
    extract_financial_info("//CYTotalRevenueAmt")

  # Assets
  assets <- xml_plucked %>%
    extract_financial_info("//TotalAssetsEOYAmt")

  # Liabilities
  liabilities <- xml_plucked %>%
    extract_financial_info("//TotalLiabilitiesEOYAmt")

  # Current year expenses
  expenses <- xml_plucked %>%
    extract_financial_info("//CYTotalExpensesAmt")

  financing <- tibble("Revenue" = revenue,
                      "Assets" = assets,
                      "Liabilities" = liabilities,
                      "Expenses" = expenses)

  return(financing)
}

#' Get financial details from 990 EZ forms
#'
#' @param xml_root An XML root element associated with a particular organization
#'
#' @return If successful, the function returns a dataframe that contains information on an organizatino's "revenue," "assets," "liabilities," and "expenses."
#' @importFrom purrr pluck
#' @importFrom tibble tibble
#' @export

get_financial_details_990ez <- function(xml_root){

  xml_plucked <- xml_root %>%
    pluck(2) # pick the second element on the list

  # Current year revenue
  revenue <- xml_plucked %>%
    extract_financial_info("//TotalRevenueAmt")

  # Assets
  assets <- xml_plucked %>%
    extract_financial_info("//Form990TotalAssetsGrp/EOYAmt")

  # Liabilities
  liabilities <- xml_plucked %>%
    extract_financial_info("//SumOfTotalLiabilitiesGrp/EOYAmt")

  # Current year expenses
  expenses <- xml_plucked %>%
    extract_financial_info("//TotalExpensesAmt")

  financing <- tibble("Revenue" = revenue,
                      "Assets" = assets,
                      "Liabilities" = liabilities,
                      "Expenses" = expenses)

  return(financing)
}

#' Check for grant-making activity from 990 forms
#'
#' @param xml_root An XML root element associated with a particular organization
#'
#' @return The function either returns the information on grant-making activity (numeric) or states that "This organization did not file ScheduleI."
#' @importFrom purrr pluck
#' @importFrom XML getNodeSet
#' @importFrom XML xmlSize
#' @export

check_for_grantmaking_activity_990 <- function(xml_root) {

  xml_plucked <- xml_root %>%
    pluck(2) # pick the second element on the list

  if ("IRS990ScheduleI" %in% names(xml_plucked) == TRUE) {
    grantmaking_flag1 <- xml_plucked %>%
  	  getNodeSet("//IRS990ScheduleI//CashGrantAmt") %>%
      xmlSize()

  	grantmaking_flag2 <- xml_plucked %>%
  	  getNodeSet("//TotalGrantOrContriPdDurYrAmt") %>%
  	  xmlSize()

  	return(max(grantmaking_flag1, grantmaking_flag2))

  } else {
    return(c("This organization did not file ScheduleI."))
    }

}

#' Filter null grant information
#'
#' @param variable A particular field in 990 forms
#'
#' @return The function either returns the numeric financial information or 0 (in the case of NULL).
#' @importFrom furrr future_map
#' @importFrom purrr reduce
#' @export

filter_null_grant_info <- function(variable){

  if (length(variable) != 0) {

    variable <- variable %>%
      future_map(xmlValue) %>%
      future_map(as.numeric) %>%
      reduce(`+`)} else {

        variable <- 0 # I intentionally made the function to return 0 in this case as it's not NA

      }

}

#' Standardize 990 flag
#'
#' @param flag_value A value associated with 990 flag
#'
#' @return The function makes the flag_value either 1 (flag_value == "true") or 0 (other cases plus when length(flag_value) == 0).
#' @importFrom furrr future_map
#' @importFrom purrr reduce
#' @export

standardize_990_flag <- function(flag_value) {
  if (length(flag_value) == 0) { flag_value <- 0 }
  if (flag_value == "true") {
    flag_value <- 1
  } else {
    flag_value <- 0}
  return(flag_value)
}

#' Get grant-making details from 990 forms
#'
#' @param xml_root An XML root element associated with a particular organization
#'
#' @return If successful, the function returns a dataframe of six columns that contains information the amount of various grants the organization received. If such information were absent in the data, then these columns would contain only NA values.
#' @importFrom XML getNodeSet
#' @importFrom furrr future_map
#' @importFrom tibble tibble
#' @export

get_grantmaking_details_990 <- function(xml_root) {

  xml_plucked <- xml_root %>%
    pluck(2) # pick the second element on the list

  if ("IRS990ScheduleI" %in% names(xml_plucked) == TRUE) {

    grantmaking_total <- xml_plucked %>%
      getNodeSet("//IRS990ScheduleI//CashGrantAmt") %>%
      filter_null_grant_info()

    grantmaking_individuals_total <- xml_plucked %>%
      getNodeSet("//IRS990ScheduleI//GrantsOtherAsstToIndivInUSGrp//CashGrantAmt")  %>%
      filter_null_grant_info()

    grantmaking_individuals_cnt <- xml_plucked %>%
      getNodeSet("//IRS990ScheduleI//GrantsOtherAsstToIndivInUSGrp//RecipientCnt") %>%
      filter_null_grant_info()

    grantmaking_501c3_cnt <- xml_plucked %>%
      getNodeSet("//IRS990ScheduleI//Total501c3OrgCnt") %>%
      future_map(xmlValue) %>%
      standardize_990_flag()

    grantmaking_other_org_cnt <- xml_plucked %>%
      getNodeSet("//IRS990ScheduleI//TotalOtherOrgCnt") %>%
      future_map(xmlValue) %>%
      standardize_990_flag()

    other_grantmaking <- xml_plucked %>%
      getNodeSet("//TotalGrantOrContriPdDurYrAmt") %>%
      future_map(xmlValue)

    if (length(other_grantmaking) == 0) { other_grantmaking <- 0 } # For the same reason above, I think that this should be 0 rather than NA. NA (explicit missing value) should indicate missing values.

    grantmaking_details <- tibble(
      grantmaking_total = grantmaking_total,
      grantmaking_individuals_total = grantmaking_individuals_total,
      grantmaking_orgs_total = grantmaking_total - grantmaking_individuals_total,
      grantmaking_501c3_cnt = grantmaking_501c3_cnt,
      grantmaking_other_org_cnt = grantmaking_other_org_cnt,
      grantmaking_individuals_cnt = grantmaking_individuals_cnt,
      other_grantmaking = other_grantmaking
    )

    return(grantmaking_details)
  }

  else {
    grantmaking_details <- tibble(
      grantmaking_total = NA,
      grantmaking_individuals_total = NA,
      grantmaking_orgs_total = NA,
      grantmaking_501c3_cnt = NA,
      grantmaking_other_org_cnt = NA,
      grantmaking_individuals_cnt = NA,
      other_grantmaking = NA)

    return(grantmaking_details)
  }

}
