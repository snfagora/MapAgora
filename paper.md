---
title: 'MapAgora: An R Package for Collecting Data on Nonprofit Organizations in the U.S.'
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
date: 19 April 2021
bibliography: paper.bib

---

# Summary

Nonprofit organizations are vital in understanding civic activities and their economic, political, and social causes and consequences in the U.S. Despite their importance, it is difficult to analyze their characteristics systematically at a large scale because information about these organizations is scattered across different sources, such as their tax reports, websites, and social media posts. The related challenge is the information in these data sources is stored in unstructured formats such as XML (Extensible Markup Language), HTML (HyperText Markup Language), and JSON (JavaScript Object Notation).

**MapAgora** addresses all of these problems. It helps researchers and practitioners collect information on nonprofit organizations in the U.S. based on nonprofit organizations' tax reports, websites, and social media posts with little technical understanding of the underlying unstructured data formats. The package enables collecting the basic information on nonprofit organizations based on the tax reports these organizations annually filed with the U.S. Internal Revenue Service (called IRS 990 filings). It also helps to extract how these organizations describe themselves based on their websites and social media posts. The physical locations of these organizations can be easily linked with the other administrative data such as the Census, and the rich text data can be used to classify these organizations using natural language processing and machine learning.

The package is available on GitHub and can be installed in the following way:

```{r}
# Install devtools 
if(!require(devtools)) install.packages("devtools")

# Install MapAgora 
install_github("snfagora/MapAgora")
```

# Statement of need

**MapAgora** is an R package that helps researchers and practitioners to collect the most comprehensive data on nonprofit organizations in the U.S. [Nonprofit Open Data Collectieve](https://nonprofit-open-data-collective.github.io/) also provide open datasets and tools, which support research on the nonprofit sector. The [National Center for Charitable Statistics](https://nccs-data.urban.org/data.php?ds=bmf) at the Urban Institute shared their data archives on the IRS 990 filings. However, these open datasets and tools are limited to the IRS 990 filings. If researchers and practitioners are interested in linking these data with other digital traces, such as websites and social media posts, they need to deal with many technical challenges. `MapAgora` reduces these steps and offers a flexible tool that can fit into the various needs of researchers and practitioners.

**MapAgora** is designed for researchers and practitioners interested in systematically examining nonprofit organizations in the U.S. One application is creating the nonprofit dataset and linking that with other administrative datasets such as the Census. The other application is classifying the nonprofit sector based on the text data collected from tax reports, websites, and social media posts [@ma2020automated].

# IRS filings 

# About pages from websites 

# Social media handles

# Package design 

**MapAgora** has been designed wit several key principles in mind.

_Principle 1_.

_Principle 2_.

_Principle 3_.

# Acknowledgements

We acknowledge support from the SNF Agora Institue and P3 Lab at the Johns Hopkins University. 

# References
