---
title: 'MapAgora: R Package for Collecting U.S. Nonprofit Organization Data'
tags:
  - R package
  - nonprofit organizations
  - data analysis
authors:
  - name: Milan de Vries
  - orcid: 0000-0001-7363-8089
    affiliation: 1
  - name: Jae Yeon Kim
    orcid: 0000-0002-6533-7910
    affiliation: 2
affiliations:
 - name: Associate Research Scholar, SNF Agora Institute, Johns Hopkins University
   index: 1
 - name: Assistant Research Scholar, SNF Agora Institute, Johns Hopkins University
   index: 2
date: June 2021
bibliography: paper.bib

---

# Summary

As Tocqueville stressed in *Democracy in America* (1835), voluntarism is a defining characteristic of American democracy. From this perspective, nonprofit organizations are vital to understanding civic activities and their economic, political, and social consequences in the US. Despite their importance, it is difficult to analyze them systematically at *scale* because information about these organizations is scattered across various sources, such as their tax reports, websites, and social media posts. The data in these sources is not only stored in diverse, unstructured formats (e.g., XML, HTML, JSON) but also created and organized in complex and largely inconsistent ways.

**MapAgora** addresses all of the problems identified above. It helps researchers and practitioners systematically collect information on nonprofit organizations in the US based on these organizations' wide range of administrative and digital trace data with little demand for technical understanding of the underlying unstructured data formats and their documentations. On the administrative data side, the package parses the tax reports these organizations filed with the US Internal Revenue Service (i.e., IRS 990 filings). On the digital trace data side, the package extracts how these organizations describe themselves based on their websites and social media posts.

The potential applications of this unprecedented big data on the study of the US nonprofit landscape are numerous. For instance, analysts can easily link the physical locations of these organizations with other administrative data, such as the Census or commercial/political surveys. They might also be interested in leveraging the detailed descriptions of the organizations' missions and activities to automatically classify these organizations based on some latent variables (e.g., racial justice organizations).

The package is available on GitHub and can be installed in the following way:

```r
# Install devtools
if(!require(devtools)) install.packages("devtools")

# Install MapAgora
devtools::install_github("snfagora/MapAgora")
```

# Statement of need

**MapAgora**  is an R package that helps researchers and practitioners collect the most comprehensive data on nonprofit organizations in the US. As summarized in Table 1, there are many proprietary and even open-source databases and tools that explore IRS data. However, if researchers and practitioners are interested in linking these data with nonprofit organizations' digital traces, such as their websites and social media posts, they must confront many additional technical challenges. `MapAgora` reduces these steps and offers a flexible tool that can fit into the various needs of researchers and practitioners.

| Name | Developer | Data sources | Paywalled? | Bulk Download? |
| --- | --- | --- | --- | --- |
| [Encyclopedia of Associations](https://www.gale.com/databases/gale-directory-library) | GALE | IRS | Yes | No |
| [Guidestar](https://www.guidestar.org/) | Candid | IRS | Yes | No |
| [National Center for Charitable Statistics](https://nccs.urban.org/) | Urban Institute | IRS | No | Yes |
| [Nonprofit Data Search](http://www.opensecrets.org/dark-money//explore-our-reports) | OpenSecrets | IRS | No | Yes |
| [Nonprofit Explorer](https://projects.propublica.org/nonprofits/) | ProPublica | IRS | No | Yes |
| [990 XML Reader] (https://github.com/jsfenfen/990-xml-reader) | Jacob Fenton / ProPublica | IRS | No | Yes |
| [open990](https://www.open990.org/contact/) | Applied Nonprofit Research | IRS | No | Yes |
| [Open Data for Nonprofit Research](https://lecy.github.io/Open-Data-for-Nonprofit-Research/) | Jesse Lecy (Arizona State University) and Nathan Grasse (Carleton University) | IRS | No | Yes |
| **MapAgora** | Milan de Vries and Jae Yeon Kim (SNF Agora Institute, Johns Hopkins University) | IRS, websites, social media posts | No | Yes |

Table 1. Comparison with existing databases and data tools for collecting US nonprofit organizations.

**MapAgora**  is designed for researchers and practitioners interested in systematically examining nonprofit organizations in the US. It can be used to create nonprofit datasets that can be used directly for analysis and linked with other administrative datasets, such as the Census or commercial/political surveys. It can also be used to classify nonprofit organizations based on the text data collected from tax reports, websites, and social media posts using natural language processing and machine learning [@ma2020automated].

# Use cases

These use cases explain the best workflow for using the functions in the package to extract information from a particular data source (IRS/websites/social media posts).

## IRS filings

IRS data provide comprehensive demographic and financial information on civic organizations.

### Load the IRS index

To begin, load the IRS index for a selected year.

```r
# load 2018 index file from the IRS Amazon Web Server
idx_2018 <- import_idx(2018)
```

### Get the AWS URL for the filing of interest

Digitized 990 data are available as XML files in an Amazon Web Server (AWS) repository. The yearly index file organizes these files. The year, in this case, refers to the year the IRS processed the file. For example, all the files in the 2018 index were processed in 2018. However, the tax filing itself can be from any previous date. This discrepancy makes it challenging to locate a specific filing for a given organization and year.

To remedy this, we rebuilt a comprehensive index file for this package. This index, included in the package, contains the additional fields `TaxPeriodBeginDt,` `TaxPeriodEndDt,` and (importantly) `Tax_Year.` `Tax_Year` refers to the tax year of the 990 form the organization submitted, but `IRS_Year` refers to the year the IRS converted the submission into digitized form. Note that organizations can choose to adhere to either a calendar or a fiscal year. `TaxPeriodBeginDt` and `TaxPeriodEndDt` refer to the beginning and end dates of that filing's year.

The location of a given digitized filing can be found with the `get_aws_url` function. This function returns the location of the XML file in the AWS repository.

```r
# this organization's 2018 990 filing can be found here
aws_url <- get_aws_url("061553389", 2018)
```

### Get the XML file for a given AWS URL

To parse fields from filings, first load an XML file of interest. Note that because this function calls `get_aws_url()`, there is no need to find the URL location. The first step for most uses of this package is `get_990()`.

```r
## load an XML for this organization's 2018 filing.
xml_root <- get_990("221405099", year = 2018)

## see name of this organization
organization_name <- get_organization_name_990("221405099")
```

### Get a specific field from a filing

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

### See related entities

Organizations report related entities on Form 990 Schedule R. The EINs of related organizations can be found with `get_scheduleR()`.

```r
## see related EINs for a given EIN and given tax year filing.
related_eins <- get_scheduleR("061553389", 2018)
```

This function returns multiple values if multiple related organizations are reported and indicates if no related organizations are reported.

### Extract mission and description texts

Organizations report descriptive information about their primary mission and main activities in 990 filings. The concatenated responses to these fields can be extracted.

```r
# mission statement
mission_desc <- get_value_990(xml_root, "mission_desc")
```

```r
# program description
program_desc <- get_value_990(xml_root, "program_desc")
```

## About pages from websites

To help track the information on nonprofits not captured by their tax filings, we developed code to extract the text that organizations use to describe themselves on their websites.

### Find About Pages

The most common place to find informative text about an organization is on its website, particularly its About Page. The About Page often contains information about the group’s mission, notable activities, and history.

There is no single standard way to find an About Page, but many common patterns predominate. Our strategy is to begin by extracting all the links from an organization’s home page. Then, we look for links containing the string "about" or "who." If we fail to find these patterns, we try to identify other less direct common patterns.

Websites can have multiple About Pages, often nested under a common heading. We first need to identify all About Pages for a selected website.

```r
# find this site's about pages
about_pages <- extract_about_links("http://www.ibew567.com")
```

### Extract About Text

Websites typically contain a great deal of code that is not directly displayed in the use case. In addition, displayed text often includes menus, headers, footers, or other small text snippets. Using natural language processing (NLP) can help clean up the text, but it is tedious and often produces low-quality output.

Our approach is to filter out text that is not in sentences with at least a certain threshold to extract salient text. Also, when extracting text from multiple pages, we look for and remove shared header and footer language to avoid duplicating it in the final corpus.

```r
# find the combined text of about pages and home page
org_web_text <- get_all_texts("http://www.ibew567.com")
```

## Social media handles

Social media posts help to investigate the public presence of organizations. However, there is no standard way of identifying the social media handles belonging to a given organization.

Here, we approach this problem by extracting social media handles from an organization's website. As a secondary strategy, we provide the functionality to search for social media handles using the Bing search application programming interface (API).

### Twitter handle

Twitter handles are identified on a page by extracting all links on the page and then filtering for possible Twitter handles. If a handle can be found, some link cleanup is attempted before returning it.

```r
# get MoveOn's twitter handle
tw_handle <- find_twitter_handle_from_org_page("https://moveon.org")
```

### Facebook page

The `find_facebook_page_from_org_page()` function works similarly but looks for a Facebook page instead of a handle.

```r
# get MoveOn's Facebook page
fb_page <- find_facebook_page_from_org_page("https://moveon.org")
```

# Package design principles

**MapAgora** has been designed with several key principles in mind.

_Principle 1_. **User-First**. Most data about nonprofit organizations exists in specialized, technical structures. IRS filings, for instance, can be made on three different versions of the same form (i.e., Form 990, 990-EZ, and 990-N; for more information, see this [IRS guideline](https://www.irs.gov/instructions/i990)), depending on the organization's gross income and total assets. We develop the package by focusing on the kind of variables that a user would want to extract from the data sources. In other words, the functions in the package do not assume that users understand the underlying data structures or their documentation. If users would like to know about an organization's revenue, knowing the target field is sufficient.

_Principle 2_. **Linkage**. Part of the motivation to create this package is that nonprofit data stems from multiple sources. The package aims to make both extracting and linking data from these different sources easy and fast. As such, the package is suitable for building a relational database based on these diverse data sources.

_Principle 3_. **Modularity**. We acknowledge that researchers and practitioners have various needs to use these data sources to solve their problems. We assume that building the most comprehensive dataset based on these data sources is not a desirable data collection method for most of these users, given the technical and administrative costs involved. With these time and other resource constraints in mind, we built functions that allow users to extract the specific subset of the data they need.

# Acknowledgements

We gratefully acknowledge the financial and administrative support from the SNF Agora Institute and P3 Lab at the Johns Hopkins University.

# References
