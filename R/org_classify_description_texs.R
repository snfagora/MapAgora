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
        idx <- fromJSON(url_vars)[[1]] %>%
            mutate(IRS_year = year)

        message(glue("successfully importing year {year} data. Please assign this outcome to an object named idx."))

        return(idx)
    }
}

#' Get the Amazon Web Server URL associated with a particular Employment Identification Numbers
#'
#' @param ein An Employment Identification Numbers
#' @param form An IRS document form. The default is NULL. There are three other options: "990", "990PF", "990EZ"
#' @param year A year in which a form was filed. The default IRS year is 2019.
#' @param move_global Whether moving the XML file, which contains the 990 forms filed in a particular year, to the user's global environment. The default value is TRUE.
#'
#' @return If successful, the function returns the Amazon Web Server URL associated with a particular Employment Identification Numbers.
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom glue glue
#' @export

get_aws_url <- function(ein, year = 2019, form = NULL, move_global = TRUE) {

    message(glue("The IRS filing year is {year}."))

    # Turn search parameter into character vector
    ein <- ifelse(!is.character(ein), as.character(ein), ein)

    if (exists("idx") == FALSE | # If you create idx object for the first time
        (exists("idx") == TRUE & year != 2019) # If you want to overwrite the existing idx object
    ) {

        if (move_global == TRUE) {


            assign("idx", import_idx(year), envir = .GlobalEnv)

        } else {

            idx <- import_idx(year)

        }

    }

    # Some organizations have two object IDs

    if (is.null(form)) {

        obj_id <- idx %>%
            filter(EIN == ein) %>%
            select(ObjectId)

    } else {

        obj_id <- idx %>%
            filter(FormType == form) %>%
            filter(EIN == ein) %>%
            select(ObjectId)

    }

    # Glue search parameter and the rest of the URL together

    # This tax report was amended
    if (nrow(obj_id == 2)) {

        # glue("http://s3.amazonaws.com/irs-form-990/{obj_id[1,]}_public.xml") # Pre-amended report

        glue("http://s3.amazonaws.com/irs-form-990/{obj_id[2,]}_public.xml") # Post-amended IRS report
    }

    # This tax report was not amended
    if (nrow(obj_id == 1)) {
        glue("http://s3.amazonaws.com/irs-form-990/{obj_id[1,]}_public.xml")
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
#' @param idx A XML file it contains the 990 forms filed in a particular year. An outcome of import_idx() function.
#' @param ein An Employment Identification Numbers
#'
#' @return If successful, the function returns the name of the organization associated with a particular Employment Identification Numbers
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @export

get_organization_name_990 <- function(idx, ein) {

    organization_name <- idx %>%
        filter(EIN == ein) %>%
        select(OrganizationName)

    return(organization_name)
}


#' Get Employer Identification Numbers (EINs) associated with foundations
#'
#' @param idx A XML file it contains the 990 forms filed in a particular year. An outcome of import_idx() function.
#'
#' @return If successful, the function returns the EINs associated with foundations.
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @export

get_foundation_ein <- function(idx){

    foundation_ein <- idx %>%
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

    filing_type <- xml_plucked %>% getNodeSet("//ReturnTypeCd") %>% map_chr(xmlValue)
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
