# Introduction

This script helps you transform pages of PDF tables into a nice CSV file, specifically made for the Transaction History PDFs that you can request from the GCash App's transaction history.

## Getting Started

### Prerequisites

Requires the following packages:
* rJava (requires JDK with your system)
* tabulizer
* tidyverse
* data.table
* lubridate

```
install.packages('rJava)
install.packages('tabulizer')
install.packages('tidyverse')
install.packages('data.table')
install.packages('lubridate')
```

### Obtaining GCash Transaction History
1. Download GCash app in [Android](https://play.google.com/store/apps/details?id=com.globe.gcash.android&hl=en) or [iOS](https://itunes.apple.com/ph/app/gcash/id520020791?mt=8)
2. As of v5.31.0:558, go to Home > Transaction History to email the details to email account. Please check if you need to be a Verified User with a Verified Email Account for that one. Take note transactions need 24 hours before they're made available.
3. Download the PDF. Might land in your Spam folder. Take note of password in their instructions and the number of pages in the document.

### User Input

Take note to change the following fields at the end of the file.

```
setwd("INPUT FOLDER HERE")
ExtractTables("INSERT FILE WITH FILE EXT HERE",
              "INSERT PASSWORD HERE",
              "INSERT NUMBER OF PAGES HERE, NO QUOTATION MARKS")
```

## Built With

* [R](https://www.r-project.org/) - Used the v3.5.2 package
* [RStudio](https://www.rstudio.com/) - IDE for R

## Authors

* **Joshua Koa**

## License

This project is licensed under the MIT License - see the [LICENSE.md](https://github.com/joshuactk/GCash-Transaction-History-Extraction/blob/master/LICENSE.txt) file for details.

## Acknowledgments

* rOpenSci team for their work on the [bindings for Tabula](https://github.com/ropensci/tabulizer/), also check their documentation [here to learn more](https://cran.r-project.org/web/packages/tabulizer/tabulizer.pdf)
