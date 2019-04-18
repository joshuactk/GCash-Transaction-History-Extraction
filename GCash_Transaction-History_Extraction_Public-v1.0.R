###############################################################################
# GCash Transaction History Extraction v1.0
# 
# 1. Please install tabulizer, tidyr, dplyr and their respective dependencies
#    - install.packages('rJava)
#    - install.packages('tabulizer')
#    - install.packages('tidyr')
#    - install.packages('dplyr')
# 2. Go to User Input at end of file to change required fields
# 3. Feel free to toggle the parameters in the functions to suit your needs
# 
###############################################################################

# Setup
library('tabulizer')

# Function for Tying These Together
ExtractTables <- function(pdffile,pword,pages){
  if(pages==1){
    srctxt = extract_tables(pdffile, password = pword,
                            page = 1, area = list(c(67.45,1,830.53,595)), # area params special to 1st pg
                            guess = "FALSE", method = "decide", output = "data.frame")
    a = srctxt[[1]]
    names(a) = c("Date and Time", "Description", "Reference No.", "Debit", "Credit", "Balance")
    a = CleanRows(a)
    a = a %>% tidyr::separate(`Date and Time`, c('Date','Time'), sep=" ", remove=FALSE) %>%
      dplyr::mutate(Currency = "PHP")
    a[is.na(a)] = ""
    write.csv(a,file="output.csv",row.names=FALSE)
  }
  else if(pages>1){
    # Wrap up the first page
    srctxt1 = extract_tables(pdffile, password = pword,
                             page = 1, area = list(c(67.45,1,830.53,595)),
                             guess = "FALSE", method = "decide", output = "data.frame")
    a = srctxt1[[1]]
    names(a) = c("Date and Time", "Description", "Reference No.", "Debit", "Credit", "Balance")
    a = CleanRows(a)
    # Then focus on the subsequent pages
    srctxt2 = extract_tables(pdffile, password = pword,
                             page = 2:pages, area = list(c(11,1,830.53,595)), # changed top param in area
                             columns = list(c(100,295,375,450,525)), # special for subsequent pages
                             guess = "FALSE", method = "decide", output = "data.frame")
    i = 1
    while(i!=pages){
      b = srctxt2[[0+i]]
      names(b) = c("Date and Time", "Description", "Reference No.", "Debit", "Credit", "Balance")
      b = CleanRows(b)
      if(i==1){
        c = b
      }
      else{
        c = merge(c,b, all=TRUE, sort=FALSE)
      }
      i = i + 1
    }
    z = merge(a,c, all=TRUE, sort=FALSE)
    z = z %>% tidyr::separate(`Date and Time`, c('Date','Time'), sep=" ", remove=FALSE) %>%
      dplyr::mutate(Currency = "PHP")
    z[is.na(z)] = ""
    write.csv(z,file="output.csv",row.names=FALSE)
  }
  else{
    print("Nothing")
  }
}

# Function for Fixing the Rows that Spawn from Really Long Descriptions
CleanRows <- function(var){
  w = 0
  for(i in 1:nrow(var)) {
    if(is.na(var$`Reference No.`[i])
       && !(var$Description[i] %in% c("ENDING BALANCE", "Total Debit", "Total Credit"))) {
      var$Description[i-1] = paste(var$Description[i-1],var$Description[i])
      v = i
      w = c(w,v)
    }
  }
  var = var[-c(w),]
  rm(v,w,i)
  return(var)
}

# User Input
setwd("INPUT FOLDER HERE")
ExtractTables("INSERT FILE WITH FILE EXT HERE",
              "INSERT PASSWORD HERE",
              "INSERT NUMBER OF PAGES HERE, NO QUOTATION MARKS")