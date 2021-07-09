#' Import machine-readable data from 990 forms filed with the IRS
#'
#' @param year A year in which a form was filed.
#'
#' @return If successful, the function returns a XML file it contains the 990 forms filed in a particular year.
#' @importFrom glue glue
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr mutate
#' @export

import_idx <- function(year){

  # avoid non-numeric year argument
  if (!is.numeric(year)) {
    stop("year argument should be numeric")
  }

  else {
    # url vector
    ## the value range for the year variable is between 2011 and 2020
    url_vars <- glue("https://s3.amazonaws.com/irs-form-990/index_{year}.json")

    # select url and import data
    assign(glue("idx_{year}"), fromJSON(url_vars)[[1]] %>% mutate(IRS_year = year))

    out <- get(glue("idx_{year}"))

    message(glue("successfully importing year {year} data. Please assign this outcome to an object named idx_{year}."))

    return(out)
  }
}

#' Index table of all available IRS filings
#'
#' A dataset containing the AWS server information for available IRS filings. This index
#' is refactored from the AWS index files. The AWS files are arranged by year the IRS
#' processed the file. These are arranged by tax year.
#'
#' @format A data frame with 3235913 rows and 14 variables:
#' \describe{
#'   \item{EIN}{IRS Employer Identification Number}
#'   \item{TaxPeriod}{Corresponds to YYYMM of the tax period end date}
#'   \item{DLN}{Download Number}
#'   \item{FormType}{Version of the 990 Form filed}
#'   \item{URL}{location on the AWS server of the XML parsed filing}
#'   \item{SubmittedOn}{Submission date}
#'   \item{ObjectId}{id of the filing on the server}
#'   \item{OrganizationName}{name of organization}
#'   \item{LastUpdated}{date last updated}
#'   \item{IRS_year}{Calendar year the IRS processed the filing. Corresponds to AWS index year.}
#'   \item{TaxPeriodBeginDt}{Start of the filings tax period}
#'   \item{TaxPeriodEndDt}{End of the filings tax period}
#'   \item{Tax_Year}{Year of the tax filing}
#'   \item{ReturnTs}{timestamp of the tax return}
#' }
"irs_index"


#' Data fields available from IRS 990 forms
#'
#' A dataset containing the variable names, associated Form 990 XML locations, Form 990 actual locations, and info
#' for retrievable 990 Fields
#'
#' @format A data frame with 32 rows and 9 variables:
#' \describe{
#'   \item{package_variable}{name to use in this package to retrieve the field}
#'   \item{category}{a top-level variable the variable belongs to}
#'   \item{subcategory}{a subcategory the variable belongs to}
#'   \item{XML_990}{the XML location for the parsed 990 form}
#'   \item{Form_990}{the Part and Line number of the form location for this XML}
#'   \item{XML_990EZ}{the XML location for the parsed 990 form}
#'   \item{Form_990EZ}{the Part and Line number of the form location for this XML}
#'   \item{XML_990PF}{the XML location for the parsed 990 form}
#'   \item{Form_990PF}{the Part and Line number of the form location for this XML}
#' }
"irs_fields"


#' Get the Amazon Web Server URL associated with a particular Employment Identification Numbers
#'
#' @param ein An Employment Identification Numbers
#' @param year A year in which a form was filed. The default IRS year is 2019.
#'
#' @return If successful, the function returns the Amazon Web Server URL associated with a particular Employment Identification Numbers.
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr arrange
#' @importFrom glue glue
#' @export

get_aws_url <- function(ein, year = 2019) {

  # Turn search parameter into character vector
  ein <- ifelse(!is.character(ein), as.character(ein), ein)

  # Some organizations have two object IDs

   obj_id <- irs_index %>%
    filter(EIN == ein, Tax_Year == year) %>%
    arrange(TaxPeriodEndDt) %>%
    select(ObjectId)


  # Glue search parameter and the rest of the URL together

  # This organization filed an extra return that year
  if (nrow(obj_id) == 2) {
    message(glue("This organization filed two returns in {year}. This is rare and usually means they filed an extra, final return before terminating the organization."))
    message(glue("Try irs_index %>% filter(EIN == '{ein}', Tax_Year == {year}) for more information."))
  }

  if (nrow(obj_id) == 0) {
    message(glue("Could not locate a filing for EIN = {ein} in {year}"))
  } else {
    glue("http://s3.amazonaws.com/irs-form-990/{obj_id[1,]}_public.xml")
  }

}


#' Get the Amazon Web Server URL associated with a particular Employment Identification Numbers
#'
#' @param ein An Employment Identification Numbers (EIN)
#' @param form An IRS document form. The default is NULL. There are three other options: "990", "990PF", "990EZ"
#' @param year A year in which a form was filed. The default IRS year is 2019.
#' @param tax_period A tax period (year-month). The default tax period is 2019.
#' @param move_global Whether moving the XML file, which contains the 990 forms filed in a particular year, to the user's global environment. The default value is TRUE.
#' @param multiple_tax_period This is only applied to the case when an EIN is associated with multiple tax periods. If you want to extract the latest tax period's latest submission, set this argument to FALSE. If you want to extract the latest submission from the multiple tax periods, set this argument to TRUE. The default value is FALSE.
#'
#' @return If a single tax period was identified, the function returns the Amazon Web Server URL associated with a particular Employment Identification Numbers. If multiple tax periods were identified, the function returns either a vector that only includes the URL (multiple_tax_period= FALSE) or a dataframe that includes the URL as well as related tax periods (multiple_tax_period = TRUE).
#' @importFrom dplyr group_by
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom glue glue
#' @export

get_aws_url_from_server <- function(ein, year = 2019, form = NULL, move_global = TRUE, tax_period = 2019, multiple_tax_period = FALSE) {

  message(glue("The IRS filing year is {year}."))

  # Turn search parameter into character vector
  ein <- ifelse(!is.character(ein), as.character(ein), ein)

  # Some organizations have two object IDs

  if (!exists(glue("idx_{year}"))) {

    if (move_global == TRUE) {

      assign(glue("idx_{year}"), import_idx(year), envir = globalenv()) # The global environment

    } else {

      assign(glue("idx_{year}"), import_idx(year)) # The default is the current environment

      }

  }

  # Some organizations have two object IDs

  idx <- get(glue("idx_{year}"))

  if (is.null(form)) {

    obj_id <- idx %>%
      filter(EIN == ein)

  } else {

    obj_id <- idx %>%
      filter(FormType == form) %>%
      filter(EIN == ein)

  }

  df <- obj_id %>%
    select(ObjectId, TaxPeriod, SubmittedOn)

  # One tax period

  n_tp <- length(unique(df$TaxPeriod))

  if (n_tp >= 2) {message(glue("Multiple tax periods are found."))}

  if (n_tp == 1) {

    # Filter tax period
    df %>%
      mutate(TaxPeriod_ch = as.character(TaxPeriod)) %>%
      filter(str_detect(TaxPeriod_ch, as.character(tax_period))) # tax_period is a new argument

    # Select the latest submitted one
    out <- df %>%
      dplyr::arrange(desc(SubmittedOn)) %>%
      dplyr::slice(1)

    # Glue search parameter and the rest of the URL together
    return(glue("http://s3.amazonaws.com/irs-form-990/{out$ObjectId}_public.xml"))

  }

  # Multiple tax periods

  if (n_tp > 1) {

    # Multiple outputs: The latest submission from each tax period

    if (multiple_tax_period == TRUE) {

      source <- df %>%
        group_by(TaxPeriod) %>%
        dplyr::arrange(desc(SubmittedOn)) %>%
        dplyr::top_n(1)

      url <- glue("http://s3.amazonaws.com/irs-form-990/{source$ObjectId}_public.xml")

      out <- data.frame(TaxPeriod = unique(source$TaxPeriod),
                        URL = url)

      return(out)

    } else {

      # Single output: The latest submission from the latest tax period

      out <- df %>%
        dplyr::arrange(desc(SubmittedOn), desc(TaxPeriod)) %>%
        dplyr::slice(1)

      return(glue("http://s3.amazonaws.com/irs-form-990/{out$ObjectId}_public.xml"))

    }


  }

}

#' Get the XML root element associated with a particular Employment Identification Numbers
#'
#' @param ein An Employment Identification Numbers
#' @param year A year in which a form was filed. The default value is 2019.
#' @return If successful, the function returns the XML root element associated with a particular Employment Identification Numbers
#' @importFrom XML xmlTreeParse
#' @importFrom XML xmlRoot
#' @export

get_990 <- function(ein, year = 2019) {

  ## lookup AWS object
  xml_root <- get_aws_url(ein, year) %>%
    ## get page and parse xml
    xmlTreeParse() %>%
    ## get root
    xmlRoot()

  return(xml_root)

}

#' Get the name of the organization associated with a particular Employment Identification Numbers
#'
#' @param ein An Employment Identification Numbers
#'
#' @return If successful, the function returns the name of the organization associated with a particular Employment Identification Numbers
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr slice
#' @export

get_organization_name_990 <- function(ein) {

  organization_name <- irs_index %>%
    filter(EIN == ein) %>%
    select(OrganizationName) %>%
    slice(1)

  return(organization_name)
}


#' Get Employer Identification Numbers (EINs) associated with foundations
#'
#'
#' @return If successful, the function returns the EINs associated with foundations.
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @export

get_foundation_ein <- function(){

  foundation_ein <- irs_index %>%
    filter(FormType == "990PF") %>%
    pull(EIN)

  return(foundation_ein)

}

#' Standardize the website URL of an organization
#'
#' @param raw_website The raw website URL of an organization
#'
#' @return If successful, the function returns the standardized version of the raw website URL.
#' @import rex
#' @importFrom urltools url_parse
#' @importFrom glue glue_data
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom stringr str_replace_all
#' @export

standardize_url <- function(raw_website){

  # valid_cars comes from https://cran.r-project.org/web/packages/rex/vignettes/url_parsing.html
  valid_chars <- rex(except_some_of(".", "/", " ", "-"))

  re <- rex(
    start,
    # protocol identifier (optional) + //
    group(list("http", maybe("s")) %or% "ftp", "://"),
    # user:pass authentication (optional)
    maybe(non_spaces,
          maybe(":", zero_or_more(non_space)),
          "@"),
    #host name
    group(zero_or_more(valid_chars, zero_or_more("-")), one_or_more(valid_chars)),
    #domain name
    zero_or_more(".", zero_or_more(valid_chars, zero_or_more("-")), one_or_more(valid_chars)),
    #TLD identifier
    group(".", valid_chars %>% at_least(2)),
    # server port number (optional)
    maybe(":", digit %>% between(2, 5)),
    # resource path (optional)
    maybe("/", non_space %>% zero_or_more()),
    end
  )

  # Lower case and remove whitespace
  raw_website <- raw_website %>% tolower() %>% trimws()

  # Standardize NAs
  na_tested <- replace(raw_website, raw_website %in% c("na", "n/a", "none"), NA)

  if (is.na(na_tested) == TRUE) return(NA)

  if (is.na(na_tested) == FALSE) {

    if (grepl(re, na_tested) == TRUE) {
      return(na_tested)
    }

    else {

      # Standardize more fuzzy cases
      url_fixed <- na_tested %>%
        url_parse()

      if (sum(is.na(url_fixed)) == 5) {

        out <- url_fixed %>%
          mutate(scheme = "http://") %>%
          glue_data("{scheme}{domain}")

        return(out)

      }

      else {
        out <- url_fixed %>%
          mutate(path = str_replace_all(path, "http|/| ", "")) %>%
          mutate(path = replace(path, path %in% c("gmail.com", "hotmail.com"), NA)) %>%
          filter(is.na(path) == FALSE) %>%
          mutate(scheme = "http://") %>%
          glue_data("{scheme}{path}")

        return(out)
      }

    }

  }
}

#' Clean the descriptions of the programs run by a particular organization
#'
#' @param program_desc Program descriptions
#' @param text_length_threshold The length of the minimum words to filter the program descriptions
#'
#' @return If successful, the function returns the cleaned descriptions of the programs run by a particular organization
#' @importFrom purrr map_int
#' @export

clean_program_desc <- function(program_desc, text_length_threshold) {

  if (length(program_desc) > 0) {
    length_check <- map_int(
      program_desc,
      function(x) {
        nchar(x) > text_length_threshold
      }
    )

    program_desc <- program_desc[length_check]
  } else {
    program_desc <- NA
  }

  return(program_desc[!duplicated(program_desc)])
}

#' Simplify ifelse process
#'
#' @param var target variable (likely character type)
#'
#' @return if the var length is 0 then the function returns NA, otherwise it returns the input parameter.
#' @export

ifnotNA <- function(var) {

  if (length(var) != 0) {

    return(var) } else {

      return(NA)

    }

}

#' Get 990 filing type
#'
#' @param xml_root An XML root element associated with a particular organization
#'
#' @importFrom purrr pluck
#' @importFrom purrr map_chr
#' @importFrom XML getNodeSet
#' @export

get_filing_type_990 <- function(xml_root) {

  xml_plucked <- xml_root %>%
    pluck(1) # pick the second element on the list

  filing_type <- xml_plucked %>% getNodeSet("//ReturnType | //ReturnTypeCd") %>% map_chr(xmlValue)
  if (filing_type != "990EZ" & filing_type != "990PF") {
    filing_type <- "990" #standardize other names to 990
  }

  return(ifnotNA(filing_type))

}

#' Get concrete information from 990 forms
#'
#' @param xml_root An XML root element associated with a particular organization
#' @param type A type of concrete information. It should be either "website" (website URL), "mission_desc," (mission statement) or "program_desc" (program description)
#' @param text_length_threshold The length of the minimum words associated with a particular organization. This value is used to filter the program descriptions. The default value is 50.
#'
#' @return Depending on the type parameter, the function returns either a website URL, a mission statement, or a program description(s).
#' @importFrom purrr pluck
#' @importFrom purrr map_chr
#' @importFrom XML getNodeSet
#' @export

get_value_990 <- function(xml_root, type =
                            c("website",
                              "mission_desc",
                              "program_desc"),
                          text_length_threshold = 50) {

  filing_type <- get_filing_type_990(xml_root) # need form type to know where to look or text

  xml_plucked <- xml_root %>%
    pluck(2) # pick the second element on the list

  # Outcomes
  if (type == "website") {
    website <- xml_plucked %>% getNodeSet("//WebsiteAddressTxt") %>% map_chr(xmlValue)
    return(ifnotNA(website) %>% standardize_url)
  }

  if (type == "mission_desc") {
    if (filing_type == "990EZ") {
      mission_desc <- xml_plucked %>% getNodeSet("//PrimaryExemptPurposeTxt") %>% map_chr(xmlValue)
    } else {
      mission_desc <- xml_plucked %>% getNodeSet("//MissionDesc") %>% map_chr(xmlValue)
    }
    return(ifnotNA(mission_desc))
  }

  if (type == "program_desc") {
    if (filing_type == "990EZ") {
      program_desc <- xml_plucked %>% getNodeSet("//DescriptionProgramSrvcAccomTxt") %>% map_chr(xmlValue) %>%
      clean_program_desc(text_length_threshold)
    } else {
      program_desc <- xml_plucked %>% getNodeSet("//Desc") %>% future_map_chr(xmlValue) %>%
      clean_program_desc(text_length_threshold)
    }
    return(ifnotNA(program_desc))
  }

}

#' Get concrete information from 990 forms
#'
#' @param xml_root An XML root element associated with a particular organization
#' @param irs_variable A type of concrete information. It should correspond to a package_variable in the package's irs_fields table
#'
#' @return Depending on the type parameter, the function returns the value of the 990 form for the given variable or else NA.
#' @importFrom purrr pluck
#' @importFrom purrr map_chr
#' @importFrom XML getNodeSet
#' @importFrom glue glue
#' @importFrom stringr str_detect
#' @importFrom stringr str_split
#' @importFrom tibble tibble
#' @export

get_single_value_990 <- function(xml_root, irs_variable) {
  xml_plucked <- xml_root %>%
    pluck(2) # pick the second element on the list
  filing_type <- get_filing_type_990(xml_root) # need form type to know where to look or text

  xml_field <- irs_fields %>% filter(package_variable == irs_variable) %>% select(glue("XML_{filing_type}"))

  if (is.na(xml_field)) {
    return(NA)
  }

  if (str_detect(xml_field,"<br>")) {
    subfields <- tibble(xml_field = str_split(xml_field,"<br>")[[1]]) %>% rowwise() %>%
      mutate(value = ifnotNA(xml_plucked %>% getNodeSet(xml_field) %>% map_chr(xmlValue)))
    form_value <- sum(as.numeric(subfields$value),na.rm = T)
  } else {
    form_value <- xml_plucked %>% getNodeSet(xml_field) %>% map_chr(xmlValue)
  }
  return(ifnotNA(form_value))
}

#' Get all financial fields for a given org
#'
#' @param xml_root An XML root element associated with a particular organization
#'
#' @return A data frame with 30 financial fields
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr rowwise
#' @importFrom dplyr mutate
#' @importFrom tidyr spread
#' @export

get_all_financial_data <- function(xml_root) {
  financial_data <- irs_fields %>% filter(category == "financial") %>%
    select(package_variable) %>%
    rowwise() %>% mutate(val = get_single_value_990(xml_root,package_variable)) %>%
    spread(package_variable, val)

  return(financial_data)
}


#' Get concrete information from Schedule R
#'
#' @param ein An Employment Identification Numbers
#' @param year A year in which a form was filed. The default value is 2019.
#' @return The function returns either concrete information from Schedule R (likely a character vector) or states that such information is not present.
#' @importFrom purrr pluck
#' @importFrom furrr future_map_chr
#' @importFrom stringr str_detect
#' @importFrom XML getNodeSet
#' @importFrom XML xmlChildren
#' @importFrom glue glue
#' @export

get_scheduleR <- function(ein, year = 2019) {

  parsed_xml <- get_990(ein, year) %>%
    pluck(2)

  # ScheduleR is present
  if (sum(str_detect(
    names(parsed_xml),
    "ScheduleR"
  )) == 1) {

    # Go down to the children level
    parsed_scheduleR <- parsed_xml %>%
      xmlChildren()

    # Select ScheduleR and go to the node EIN
    out <- parsed_scheduleR$IRS990ScheduleR %>%
      getNodeSet("//EIN") %>%
      future_map_chr(xmlValue)

    # ScheduleR is not present
  } else {
    out <- glue("{ein} did not file ScheduleR.")
  }

  return(ifnotNA(out))
}

#' Get concrete information from Schedule O
#'
#' @param ein An Employment Identification Numbers
#' @param year A year in which a form was filed. The default value is 2019.
#' @return The function returns either concrete information from Schedule O (likely a character vector) or states that such information is not present.
#' @importFrom purrr pluck
#' @importFrom furrr future_map_chr
#' @importFrom stringr str_detect
#' @importFrom XML getNodeSet
#' @importFrom XML xmlChildren
#' @importFrom glue glue
#' @export

get_scheduleO <- function(ein, year = 2019) {

  parsed_xml <- get_990(ein, year) %>%
    pluck(2)

  # ScheduleO is present
  if (sum(str_detect(
    names(parsed_xml),
    "ScheduleO"
  )) == 1) {

    # Go down to the children level
    parsed_scheduleO <- parsed_xml %>%
      xmlChildren()

    # Text
    text <- parsed_scheduleO$IRS990ScheduleO %>%
      getNodeSet("//ExplanationTxt") %>%
      future_map_chr(xmlValue)

    # What the text is about
    ref <- parsed_scheduleO$IRS990ScheduleO %>%
      getNodeSet("//FormAndLineReferenceDesc") %>%
      future_map_chr(xmlValue)

    out <- glue("From {ref}: {text}")

    # ScheduleO is not present
  } else {
    out <- glue("{ein} did not file ScheduleO.")
  }

  return(out)

}

#' Get various information about an organization
#'
#' @param ein An Employment Identification Numbers (EIN)
#' @param type Different types of information users can get about an organization: "base," "extended," "unnested," and "combined". A base type output includes an organization's EIN, standardized website URL, its mission statement and program descriptions. An extended version includes information on an organization's schedule R and O documents (list columns). An unnested version transforms the information on schedule R and O documents as character vectors, not as list columns. A combined version joins the base and extended outputs together.
#' @param year A year in which a form was filed. The default value is 2019.
#' @return The function returns either concrete information from Schedule O (likely character) or states that such information is not present.
#' @importFrom tibble tibble
#' @importFrom tidyr unnest
#' @importFrom dplyr left_join
#' @export

example_function_for_single_org <- function(ein, type = c("base", "extended", "unnested", "combined"), year = 2019) {

  # XML root
  xml_root <- get_990(ein, year)

  # website
  website <- get_value_990(xml_root, "website")

  # mission statement
  mission_desc <- get_value_990(xml_root, "mission_desc")

  # program description
  program_desc <- get_value_990(xml_root, "program_desc")

  # ScheduleR
  schedule_r <- get_scheduleR(ein, year)

  # ScheduleO
  schedule_o <- get_scheduleO(ein, year)

  # put results together as a data.frame

  ## Unnested columns
  out <- tibble(
    "EIN" = ein,
    "website_address" = website,
    "mission_descriptions" = mission_desc,
    "program_descriptions" = program_desc
  )

  ## Nested columns
  nest_out <- tibble(
    "EIN" = ein,
    "ScheduleR" = schedule_r,
    "ScheduleO" = schedule_o
  )

  # Outputs
  if (type == "base") return(out)
  if (type == "extended") return(nest_out)
  if (type == "unnested") return(nest_out %>%
                                   unnest(ScheduleR) %>%
                                   unnest(ScheduleO))
  if (type == "combined") return(out %>% left_join(nest_out))

}
