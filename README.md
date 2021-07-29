
# MapAgora

<!-- badges: start -->
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![lifecycle](man/figures/lifecycle-stable.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

**MapAgora** is an R package that helps researchers and practitioners to collect the most comprehensive data on nonprofit organizations in the U.S. Nonprofit organizations are vital in understanding civic activities and their economic, political, and social causes and consequences in the U.S. Despite their importance, it is difficult to analyze their characteristics systematically at a large scale because information about these organizations is scattered across different sources, such as their tax reports, websites, and social media posts. **MapAgora** addresses all of these problems. It helps researchers and practitioners systematically collect information on nonprofit organizations in the U.S. based on nonprofit organizations' tax reports, websites, and social media posts with little technical understanding of the underlying unstructured data formats.

## Install the current development version from GitHub

The package is available on GitHub and can be installed in the following way:

```r
if (!require("devtools")) install.packages("devtools")

install_github("snfagora/MapAgora")
```

## Use cases

These use cases explain the best workflow for using the functions in the package to extract information from a particular data source (IRS/websites/social media posts).

### IRS filings

IRS data provide comprehensive demographic and financial information on civic organizations.

#### Load the IRS index

To begin, load the IRS index for a selected year.

```r
# load 2018 index file from the IRS Amazon Web Server
idx_2018 <- import_idx(2018)
```

#### Get the AWS URL for the filing of interest

Digitized 990 data are available as XML files in an Amazon Web Server (AWS) repository. The yearly index file organizes these files. The year, in this case, refers to the year the IRS processed the file. For example, all the files in the 2018 index were processed in 2018. However, the tax filing itself can be from any previous date. This discrepancy makes it challenging to locate a specific filing for a given organization and year.

To remedy this, we rebuilt a comprehensive index file for this package. This index, included in the package, contains the additional fields `TaxPeriodBeginDt,` `TaxPeriodEndDt,` and (importantly) `Tax_Year.` `Tax_Year` refers to the tax year of the 990 form the organization submitted, but `IRS_Year` refers to the year the IRS converted the submission into digitized form. Note that organizations can choose to adhere to either a calendar or a fiscal year. `TaxPeriodBeginDt` and `TaxPeriodEndDt` refer to the beginning and end dates of that filing's year.

The location of a given digitized filing can be found with the `get_aws_url` function. This function returns the location of the XML file in the AWS repository.

```r
# this organization's 2018 990 filing can be found here
aws_url <- get_aws_url("061553389", 2018)
```

#### Get the XML file for a given AWS URL

To parse fields from filings, first load an XML file of interest. Note that because this function calls `get_aws_url()`, there is no need to find the URL location. The first step for most uses of this package is `get_990()`.

```r
## load an XML for this organization's 2018 filing.
xml_root <- get_990("221405099", year = 2018)

## see name of this organization
organization_name <- get_organization_name_990("221405099")
```

#### Get a specific field from a filing

Nonprofit organizations can file different versions of the 990 form depending on their specific status. Knowledge of the specific form type is not required to extract values with this package, but the type of a given form can be seen with the following:

```r
## see form type of a filing
filing_type <- get_filing_type_990(xml_root)
```

Available parsed fields can be seen in the `irs_fields` table. The `package_variable` column lists the variable names to be used in functions. Other columns show both the XML path and the physical form location for that variable. These variables are grouped by category and subcategory.

```r
## see available variables
names(irs_fields)

## get total revenue for this org
revenue_total <- get_single_value_990(xml_root, "revenue_total")
```

#### See related entities

Organizations report related entities on Form 990 Schedule R. The EINs of related organizations can be found with `get_scheduleR()`.

```r
## see related EINs for a given EIN and given tax year filing.
related_eins <- get_scheduleR("061553389", 2018)
```

This function returns multiple values if multiple related organizations are reported and indicates if no related organizations are reported.

#### Extract mission and description texts

Organizations report descriptive information about their primary mission and main activities in 990 filings. The concatenated responses to these fields can be extracted.

```r
# mission statement
mission_desc <- get_value_990(xml_root, "mission_desc")
```

```r
# program description
program_desc <- get_value_990(xml_root, "program_desc")
```

### About pages from websites

To help track the information on nonprofits not captured by their tax filings, we developed code to extract the text that organizations use to describe themselves on their websites.

#### Find About Pages

The most common place to find informative text about an organization is on its website, particularly its About Page. The About Page often contains information about the group’s mission, notable activities, and history.

There is no single standard way to find an About Page, but many common patterns predominate. Our strategy is to begin by extracting all the links from an organization’s home page. Then, we look for links containing the string "about" or "who." If we fail to find these patterns, we try to identify other less direct common patterns.

Websites can have multiple About Pages, often nested under a common heading. We first need to identify all About Pages for a selected website.

```r
# find this site's about pages
about_pages <- extract_about_links("http://www.ibew567.com")
```

#### Extract About Text

Websites typically contain a great deal of code that is not directly displayed in the use case. In addition, displayed text often includes menus, headers, footers, or other small text snippets. Using natural language processing (NLP) can help clean up the text, but it is tedious and often produces low-quality output.

Our approach is to filter out text that is not in sentences with at least a certain threshold to extract salient text. Also, when extracting text from multiple pages, we look for and remove shared header and footer language to avoid duplicating it in the final corpus.

```r
# find the combined text of about pages and home page
org_web_text <- get_all_texts("http://www.ibew567.com")
```

### Social media handles

Social media posts help to investigate the public presence of organizations. However, there is no standard way of identifying the social media handles belonging to a given organization.

Here, we approach this problem by extracting social media handles from an organization's website. As a secondary strategy, we provide the functionality to search for social media handles using the Bing search application programming interface (API).

#### Twitter handle

Twitter handles are identified on a page by extracting all links on the page and then filtering for possible Twitter handles. If a handle can be found, some link cleanup is attempted before returning it.

```r
# get MoveOn's twitter handle
tw_handle <- find_twitter_handle_from_org_page("https://moveon.org")
```

#### Facebook page

The `find_facebook_page_from_org_page()` function works similarly but looks for a Facebook page instead of a handle.

```r
# get MoveOn's Facebook page
fb_page <- find_facebook_page_from_org_page("https://moveon.org")
```

## Contributing 

* If you would like to report bugs, suggest features, or leave comments, please [create issues](https://github.com/snfagora/MapAgora/issues).

* If you would like to contribute code, please fork the source code, modify, and issue a [pull request](https://docs.github.com/en/github/collaborating-with-issues-and-pull-requests/creating-a-pull-request-from-a-fork).

## Acknowledgements

We acknowledge support from the SNF Agora Institue and P3 Lab at the Johns Hopkins University. 
