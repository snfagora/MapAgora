---
title: 'MapAgora: Collecting U.S. Nonprofit Organization Data based on Government, Website, and Social Media Data Sources'
tags:
  - R package
  - nonprofit organizations 
  - data analysis
authors:
  - name: Milan de Vries
    affiliation: 1
  - name: Jae Yeon Kim
    orcid: 0000-0002-6533-7910
    affiliation: 2
affiliations:
 - name: Associate Research Scholar, SNF Agora Institute, Johns Hopkins University 
   index: 1
 - name: PhD Candidate, Department of Political Science, University of California, Berkeley
   index: 2
date: May 2021
bibliography: paper.bib

---

# Summary

Nonprofit organizations are vital in understanding civic activities and their economic, political, and social causes and consequences in the U.S. Despite their importance, it is difficult to analyze their characteristics systematically at a large scale because information about these organizations is scattered across different sources, such as their tax reports, websites, and social media posts. The related challenge is the information in these data sources is stored in diverse, unstructured formats such as XML (Extensible Markup Language), HTML (HyperText Markup Language), and JSON (JavaScript Object Notation).

**MapAgora** addresses all of these problems. It helps researchers and practitioners systematically collect information on nonprofit organizations in the U.S. based on nonprofit organizations' tax reports, websites, and social media posts with little technical understanding of the underlying unstructured data formats and their documentations. The package enables collecting the basic information on nonprofit organizations based on the tax reports these organizations filed with the U.S. Internal Revenue Service (i.e., IRS 990 filings). It also helps users extract how these organizations describe themselves based on their websites and social media posts. The potential applications of this big data on U.S. nonprofits are numerous. For instance, analysts can easily link the physical locations of these organizations with the other administrative data such as the Census or commercial/political surveys. They might also be interested in using the detailed descriptions of the organizations' missions and activities to automatically classify these organizations based on some latent variables (e.g., racial justice organizations).

The package is available on GitHub and can be installed in the following way:

```{r}
# Install devtools 
if(!require(devtools)) install.packages("devtools")

# Install MapAgora 
devtools::install_github("snfagora/MapAgora")
```

# Statement of need

**MapAgora** is an R package that helps researchers and practitioners to collect the most comprehensive data on nonprofit organizations in the U.S. [Nonprofit Open Data Collectieve](https://nonprofit-open-data-collective.github.io/) also provide open datasets and tools, which support research on the nonprofit sector. The [National Center for Charitable Statistics](https://nccs-data.urban.org/data.php?ds=bmf) at the Urban Institute share their data archives on the IRS 990 filings. However, these open datasets and tools are limited to the IRS 990 filings. If researchers and practitioners are interested in linking these data with other digital traces, such as websites and social media posts, they need to deal with many technical challenges. `MapAgora` reduces these steps and offers a flexible tool that can fit into the various needs of researchers and practitioners.

**MapAgora** is designed for researchers and practitioners interested in systematically examining nonprofit organizations in the U.S. One application is creating nonprofit datasets that can be used directly for analysis and linked with other administrative datasets such as the Census or commercial/political surveys. The other application is classifying the nonprofit sector based on the text data collected from tax reports, websites, and social media posts using natural language processing and machine learning [@ma2020automated].

# IRS filings 

# About pages from websites 

# Social media handles

# Package design 

**MapAgora** has been designed with several key principles in mind.

_Principle 1_. **User-First**. Most data about nonprofit organizations exist in specialized, technical structures. IRS filings, for instance, can be made on three different versions of the same form (i.e., Form 990, 990-EZ, and 990-N, for more information, see this [IRS guideline](https://www.irs.gov/instructions/i990)) depending on the organization's gross income and total assets. We develop the functions in this package, focusing on the kind of variables that a user would want to extract from the IRS data source. In other words, the functions do not assume that users understand the underlying data structures and their documents. If users would like to know about an organization's revenue, knowing the target field is sufficient.  

_Principle 2_. **Connectivity**. Part of the motivation of this package is the data about nonprofits stem from multiple sources. The package aims to make both extracting and linking data from these different sources easy and fast. As such, the package is suitable to build a relational database based on these diverse data sources. 

_Principle 3_. **Modularity**. We acknowledge that researchers and practitioners have different needs to use these data sources to solve their problems. We assume that building the most comprehensive dataset based on these data sources is not a desirable data collection method for most of these users. With that time and other resource constraints in mind, we built functions that allow users to extract the specific subset of the data they need for their interest.

# Acknowledgements

We acknowledge financial and administrative support from the SNF Agora Institute and P3 Lab at the Johns Hopkins University.

# References
