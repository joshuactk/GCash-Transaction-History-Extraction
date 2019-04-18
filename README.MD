# Introduction

This script helps you transform pages of PDF tables into a nice CSV file, specifically made for the Transaction History PDFs that you can request from the GCash App's transaction history.

## Getting Started

### Prerequisites

Requires the following packages:
* rJava (requires JDK with your system)
* tabulizer
* tidyr
* dplyr

```
install.packages('rJava)
install.packages('tabulizer')
install.packages('tidyr')
install.packages('dplyr')
```

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

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details.

## Acknowledgments

* rOpenSci team for their work on the [bindings for Tabula](https://github.com/ropensci/tabulizer/), also check their documentation [here to learn more](https://cran.r-project.org/web/packages/tabulizer/tabulizer.pdf)
