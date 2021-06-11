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
date: June 2021
bibliography: paper.bib

---

# Summary

As Tocqueville stressed in his *Democracy in America* (1835), voluntarism is one of the defining characteristics of American democracy [@skocpol2000nation]. From that perspective, nonprofit organizations are vital in understanding civic activities and their economic, political, and social consequences in the U.S. Despite their importance, it is difficult to analyze them systematically at *scale* because information about these organizations is scattered across various sources, such as their tax reports, websites, and social media posts. Not only the information in these data sources is stored in diverse and unstructured formats (e.g., XML, HTML, JSON), but also created and organized in complex and largely inconsistent ways.

**MapAgora** addresses all of the problems identified above. It helps researchers and practitioners systematically collect information on nonprofit organizations in the U.S. based on these organizations' wide range of administrative and digital trace data with little technical understanding of the underlying unstructured data formats and their documentations. On the administrative data side, the package parses the tax reports these organizations filed with the U.S. Internal Revenue Service (i.e., IRS 990 filings). On the digital trace data side, the package extracts how these organizations describe themselves based on their websites and social media posts.

The potential applications of this unprecedented big data on the study of U.S. nonprofit landscape are numerous. For instance, analysts can easily link the physical locations of these organizations with the other administrative data such as the Census or commercial/political surveys. They might also be interested in leveraging the detailed descriptions of the organizations' missions and activities to automatically classify these organizations based on some latent variables (e.g., racial justice organizations).

The package is available on GitHub and can be installed in the following way:

```{r}
# Install devtools 
if(!require(devtools)) install.packages("devtools")

# Install MapAgora 
devtools::install_github("snfagora/MapAgora")
```

# Statement of need

**MapAgora** is an R package that helps researchers and practitioners to collect the most comprehensive data on nonprofit organizations in the U.S. As summarized in Table 1, there are many proprietary and even open source databases and tools that explore IRS data. However, if researchers and practitioners are interested in linking these data with nonprofit organizations' digital traces, such as their websites and social media posts, they need to deal with many additional technical challenges. `MapAgora` reduces these steps and offers a flexible tool that can fit into the various needs of researchers and practitioners. 

| Name | Developer | Data sources | Paywalled? | Bulk Download? |
| --- | --- | --- | --- | --- | 
| [Encyclopedia of Associations](https://www.gale.com/databases/gale-directory-library) | GALE | IRS | Yes | No | 
| [Guidestar](https://www.guidestar.org/) | Candid | IRS | Yes | No |
| [National Center for Charitable Statistics](https://nccs.urban.org/) | Urban Institute | IRS | No | Yes | 
| [Nonprofit Data Search](http://www.opensecrets.org/dark-money//explore-our-reports) | OpenSecrets | IRS | No | Yes | 
| [Nonprofit Explorer](https://projects.propublica.org/nonprofits/) | ProPublica | IRS | No | Yes |
| [open990](https://www.open990.org/contact/) | Applied Nonprofit Research | IRS | No | Yes |  
| [Open Data for Nonprofit Research](https://lecy.github.io/Open-Data-for-Nonprofit-Research/) | Jesse Lecy (Arizona State University) and Nathan Grasse (Carleton University) | IRS | No | Yes | 
| **MapAgora** | Milan de Vries and Jae Yeon Kim (Johns Hopkins SNF Agora Institute) | IRS, websites, social media posts | No | Yes |  

Table 1. Comparison with existing databases and data tools on the U.S. nonprofit organizations.

**MapAgora** is designed for researchers and practitioners interested in systematically examining nonprofit organizations in the U.S. One application is creating nonprofit datasets that can be used directly for analysis and linked with other administrative datasets such as the Census or commercial/political surveys. The other application is classifying the nonprofit sector based on the text data collected from tax reports, websites, and social media posts using natural language processing and machine learning [@ma2020automated].

# Use cases 

These use cases explain the best workflow to use the functions in the package to extract information from a particular data source.

## IRS filings 

## About pages from websites 

## Social media handles

# Package design principles  

**MapAgora** has been designed with several key principles in mind.

_Principle 1_. **User-First**. Most data about nonprofit organizations exist in specialized, technical structures. IRS filings, for instance, can be made on three different versions of the same form (i.e., Form 990, 990-EZ, and 990-N, for more information, see this [IRS guideline](https://www.irs.gov/instructions/i990)) depending on the organization's gross income and total assets. We develop the package focusing on the kind of variables that a user would want to extract from the IRS data source. In other words, the functions in the package do not assume that users understand the underlying data structures and their documentation. If users would like to know about an organization's revenue, knowing the target field is sufficient.  

_Principle 2_. **Linkage**. Part of the motivation of this package is the data about nonprofits stem from multiple sources. The package aims to make both extracting and linking data from these different sources easy and fast. As such, the package is suitable to build a relational database based on these diverse data sources. 

_Principle 3_. **Modularity**. We acknowledge that researchers and practitioners have various needs to use these data sources to solve their problems. We assume that building the most comprehensive dataset based on these data sources is not a desirable data collection method for most of these users, given the technical and administrative cost involved. With that time and other resource constraints in mind, we built functions that allow users to extract the specific subset of the data they need for their interest.

# Acknowledgements

We gratefully acknowledge the financial and administrative support from the SNF Agora Institute and P3 Lab at the Johns Hopkins University.

# References
