#' Check whether the href attribute contains "about" or not
#'
#' @param href An href attribute
#'
#' @return Either an href attribute (YES) or NA (NO)
#' @importFrom stringr str_detect
#' @export

if_not_about <- function(href) {
  if ((TRUE %in% (href %>%
    tolower() %>%
    str_detect("about|who"))) &
    (any(href %>%
      tolower() %>%
      str_detect("about|who") == TRUE))) {
    return(href)
  } else {
    return(NA)
  }
}


#' Check if a url exists with httr
#' @param url A URL to check
#' @param non_2xx_return_value what to do if the site exists but the
#'        HTTP status code is not in the `2xx` range. Default is to return `TRUE`.
#' @param quiet if not `FALSE`, then every time the `non_2xx_return_value` condition
#'        arises a warning message will be displayed. Default is `TRUE`.
#' @param timeout_thres timeout in seconds for httr attempt
#'
#' @return A boolean value to indicate whether a website is reachable
#'
#' @importFrom httr GET
#' @importFrom httr HEAD
#' @importFrom httr status_code
#' @importFrom httr config
#' @importFrom purrr safely
#' @export

url_exists <- function(url,
                       non_2xx_return_value = FALSE,
                       quiet = TRUE,
                       timeout_thres = 10) {
  sHEAD <- safely(HEAD)
  sGET <- safely(GET)

  # Try HEAD first since it's lightweight
  res <- sHEAD(url, config(
    ssl_verifypeer = FALSE,
    timeout = timeout_thres,
    followlocation = TRUE
  ))

  if (is.null(res$result) || ((status_code(res$result) %/% 200) != 1)) {
    res <- sGET(url, config(
      ssl_verifypeer = FALSE,
      timeout = timeout_thres,
      followlocation = TRUE
    ))

    if (is.null(res$result)) {
      return(FALSE)
    } # or whatever you want to return on "hard" errors
    if (((status_code(res$result) %/% 200) != 1)) {
      if (!quiet) warning(sprintf("Requests for [%s] responded but without an HTTP status code in the 200-299 range", x))
      return(non_2xx_return_value)
    }
    return(TRUE)
  } else {
    return(TRUE)
  }
}


#' Extract links and other information related to about page
#'
#' @param base_url A base URL (the base part of the web address)
#' @param timeout_thres A timeout threshold. The default value is 10 seconds.
#'
#' @return If successful, the function returns a dataframe of two columns ("href", "link"). If not successful, the function returns a dataframe of three columns ('href', 'link_text', link'). In this dataframe, href should be NA and link_test should inform one of the following five error cases: "Found without tree search.", "This website is broken.", "The website is flat (no tree structure).", "PHP error", or "The website does not have about page."
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

extract_about_links <- function(base_url, timeout_thres = 10) {

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
    about_links <- tibble(
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

      about_pages <- page_list %>%
        filter(grepl("about", tolower(title)) | grepl("about", tolower(pageUriSEO))) %>%
        select(pageUriSEO)

      # Dataframe with three columns
      about_links <- tibble(
        href = "Base",
        link_text = "The website is built by Wix.",
        link = about_pages <- glue("{base_url}/{about_pages}")
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
      about_links <- tibble(
        href = NA,
        link_text = "The website is flat (no tree structure).",
        link = base_url
      )
    }

    # Dataframe with three columns
    else if (prod(c(is.na(if_not_about(tolower(href))), is.na(if_not_about(tolower(link_text))))) > 0) {

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

      possible_about_urls <- c(
        glue("{base_url}about"), # About case 1
        glue("{base_url}about-us"), # About case 2
        glue("{base_url}who-we-are")
      ) # Who we are case

      # Check whether a request for the specific URL works without error
      if (sum(future_map_int(possible_about_urls, ~ url_exists(., timeout_thres = timeout_thres))) >= 1) {

        # Dataframe with three columns
        about_links <- tibble(
          href = "Base",
          link_text = "Found without tree search.",
          link = possible_about_urls[which(future_map_int(possible_about_urls, ~ url_exists(., timeout_thres = timeout_thres)) == 1)]
        )
      } else {

        # Data frame with three columns
        about_links <- tibble(
          "href" = NA,
          "link_text" = "The website does not have about page.",
          "link" = base_url
        )
      }
    } else {

      # about link found
      df <- tibble(
        "href" = if_not_about(href), # Don't make it lower case (URLs are case sensitive)
        "link_text" = if_not_about(tolower(link_text)),
        "link" = base_url
      )

      about_links <- df %>%
        filter(str_detect(tolower(link_text), "about|who") |
          str_detect(tolower(href), "about|who")) %>%
        filter(!is.na(href)) %>%
        distinct() %>%
        arrange(desc(str_detect(tolower(link_text), "about|who") + str_detect(tolower(href), "about|who")))

      # To make link formatting not off when page uses an absolute reference
      about_links <- about_links %>%
        mutate(link = url_absolute(href, base_url)) %>%
        select(href, link) %>%
        distinct(link, .keep_all = TRUE)

      return(about_links)
    }
  }

  return(about_links)
}

#' Find links and other information related to about page
#'
#' @param base_url A base URL (the base part of the web address)
#'
#' @return If successful, the function returns a working base URL. If not, the function reports one of the following four error cases: "This link does not have about page.", "This link has a PHP error.", "This link is flat (not tree structure).", or "This link is broken."
#' @importFrom glue glue
#' @importFrom dplyr if_else
#' @importFrom dplyr pull
#' @importFrom dplyr first
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace
#' @importFrom xml2 url_absolute
#' @export

find_about_link <- function(base_url) {
  about_links <- extract_about_links(base_url)

  # Find cases where links are broken or links don't have about pages
  if (NA %in% about_links$href == TRUE) {

    # if_else is slightly faster than ifelse
    about_url <- if_else(str_detect(about_links$link_text, "have about page"), # About page problem
      glue("This {about_links$link} does not have about page."),
      if_else(str_detect(about_links$link_text, "PHP"), # PHP problem
        glue("This {about_links$link} has a PHP error."),
        if_else(str_detect(about_links$link_text, "(no tree structure)"), # Flat tree problem
          glue("This {about_links$link} is flat (not tree structure)."),
          glue("This {about_links$link} is broken.") # Broken URL problem
        )
      )
    )
  } else {
    about_url <- about_links %>%
      pull("link") %>%
      unique() %>%
      first()
  }

  # about_url
  return(about_url)
}

#' Extract about page content
#'
#' @param about_url An URL for an about page
#'
#' @return About page text (character vector)
#' @importFrom htm2txt gettxt
#' @importFrom textclean strip
#' @export

extract_about_page_content <- function(about_url) {

  # Broken, flat, PHP error, about page issues
  if (str_detect(about_url, "tree search|is broken|is flat|PHP error|about page") == TRUE) {

    # Output: dataframe
    about_url_text <- about_url
  }

  # Other cases
  else {
    about_page <- gettxt(about_url) %>%
      strip(digit.remove = FALSE) # more comprehensive and efficient text cleaning

    # Output: dataframe
    about_url_text <- about_page
  }

  return(about_url_text)
}

#' Get about page content
#'
#' @param base_url A base URL (the base part of the web address)
#'
#' @return about_page A dataframe that contains about page text
#' @export

get_about_page_content <- function(base_url) {

  # Search prospective about URLs
  about_url <- find_about_link(base_url)

  # Extract information
  about_url_text <- extract_about_page_content(about_url)

  # Close all URL connections
  on.exit(closeAllConnections())

  about_page <- data.frame("about_page" = about_url_text)

  return(about_page)
}


#' Extract all about page texts for a given organization
#'
#' @param org_url A URL (the web address of the organization of interest
#'
#' @return about_texts A dataframe that contains the combined extracted text from the
#' homepage and about pages, the shared header of the pages if found, and the shared
#' footer if found.
#'
#' @importFrom dplyr rowwise
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr distinct
#' @importFrom purrr possibly
#'
#' @export
get_all_texts <- function(org_url) {
  safe_parse_by_length <- possibly(parse_by_length, otherwise = NA)

  x <- extract_about_links(org_url) %>%
    mutate(base_url = org_url)
  x <- rbind(x, data.frame(base_url = org_url, url = org_url)) # add homepage as well
  x <- x %>%
    rowwise() %>%
    mutate(txt = safe_parse_by_length(url))
  x <- x %>%
    filter(!is.na(txt)) %>%
    distinct(txt, .keep_all = TRUE)

  if (nrow(x) == 0) {
    return(data.frame(body = NA, header = NA, footer = NA))
  } else {
    about_texts <- combine_texts(x$txt)
    return(about_texts)
  }
}

#' Extract significant text from html source
#'
#' @param url A URL (the web address to extract text from)
#' @param sentence_threshold an integer signifying the minimum word length for an included sentence
#'
#' @return extracted_text The text found on a page
#'
#' @importFrom dplyr rowwise
#' @importFrom xml2 read_html
#' @importFrom purrr possibly
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom stringr str_split
#' @importFrom XML htmlTreeParse
#' @importFrom XML xpathApply
#' @importFrom graphics text
#' @export
parse_by_length <- function(url, sentence_threshold = 5) {
  response <- GET(url, config(ssl_verifypeer = FALSE, timeout = 10, followlocation = TRUE))
  possible_read <- possibly(read_html, otherwise = NA)
  pg <- possible_read(response)

  if (is.na(pg)) {
    return(NA)
  }

  html <- htmlTreeParse(pg, useInternal = TRUE)
  txt <- xpathApply(html, "//body//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::ul)][not(ancestor::form)]", xmlValue)
  txt <- unlist(txt)
  txt <- gsub("\r|\n|\t", "", txt)
  txt <- trimws(txt, "both")
  txt <- txt[which(txt != "")]
  extracted_text <- tibble(text = txt)
  extracted_text <- extracted_text %>%
    rowwise() %>%
    mutate(sl = length(unlist(str_split(text, " ")))) %>%
    filter(sl > sentence_threshold)

  extracted_text <- paste0(extracted_text$text, collapse = " ")

  return(extracted_text)
}

#' Combine multiple webpage texts into one shared text with headers and footers removed
#'
#' @param x A vector of texts
#'
#' @return combined_text A data frame with three columns. Header is the shared header
#' text. Footer is the shared footer text. Body is the concatenated text with header and
#' footer removed
#'
#'
#' @importFrom stringi stri_replace_first_fixed
#' @importFrom stringi stri_replace_last_fixed
#'
#' @export
combine_texts <- function(x) {
  x <- unique(x)

  if (length(x) == 1) {
    combined_text <- data.frame(body = x, header = NA, footer = NA)
    return(combined_text)
  }

  header <- find_header(x)
  footer <- find_footer(x)

  if (!is.na(header)) {
    x <- stri_replace_first_fixed(x, header, "") # remove header from the beginning
  }

  if (!is.na(footer)) {
    x <- stri_replace_last_fixed(x, footer, "") # remove footer from the end
  }

  x <- trimws(x, "both") # trim white space

  combined_text <- data.frame(body = as.character(paste(x, collapse = " ")), header = header, footer = footer)
  return(combined_text)
}

#' Find a common header string in a set of texts
#'
#' @param x A vector of texts
#' @param threshold Fraction of the texts required to share the common header string
#'
#' @return header A string that is the longest common header string for all the texts.
#'
#' @importFrom stringr str_split
#' @importFrom stringr str_length
#'
#' @export
find_header <- function(x, threshold = 0.9) {
  idx <- which(str_length(x) == max(str_length(x)))[1] # use longest page as comparison to maximize chance of having header/footer
  pg <- x[idx]
  pgs <- x[-idx]
  words <- str_split(pg, " ")[[1]]

  header <- NA
  header_searching <- 1
  i <- 1

  while (header_searching & i <= length(words)) {
    test_header <- paste(words[1:i], collapse = " ")
    header_searching_quotient <- sum(startsWith(pgs, test_header)) / length(pgs)

    if (header_searching_quotient >= threshold) {
      header <- test_header
      i <- i + 1
    } else {
      header_searching <- 0
    }
  }

  return(header)
}

#' Find a common footer string in a set of texts
#'
#' @param x A vector of texts
#' @param threshold Fraction of the texts required to share the common footer string
#'
#' @return footer A string that is the longest common footer string for all the texts.
#'
#' @importFrom stringr str_split
#' @importFrom stringr str_length
#'
#' @export
find_footer <- function(x, threshold = 0.9) {
  idx <- which(str_length(x) == max(str_length(x)))[1] # use longest page as comparison to maximize chance of having header/footer
  pg <- x[idx]
  pgs <- x[-idx]
  words <- str_split(pg, " ")[[1]]

  footer <- NA
  footer_searching <- 1
  i <- length(words)

  while (footer_searching & i > 0) {
    test_footer <- paste(words[i:length(words)], collapse = " ")
    footer_searching_quotient <- sum(endsWith(pgs, test_footer)) / length(pgs)

    if (footer_searching_quotient >= threshold) {
      footer <- test_footer
      i <- i - 1
    } else {
      footer_searching <- 0
    }
  }

  return(footer)
}
