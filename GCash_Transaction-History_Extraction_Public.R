###############################################################################
# GCash Transaction History Extraction v1.1
# 
# 1. Please install the packages and their respective dependencies
#    - install.packages('rJava)
#    - install.packages('tabulizer')
#    - install.packages('tidyverse')
#    - install.packages('data.table')
#    - install.packages('lubridate')
# 2. Go to User Input at end of file to change required fields
# 3. Feel free to toggle the parameters in the functions to suit your needs
# 
###############################################################################

# Setup
library('tabulizer')
library('tidyverse')
library('data.table')
library('lubridate')

#  Supporting Functions
CleanDt <- function(text) { # fix Date and Time
  text = substr(text, 2, nchar(text))
  text = gsub("[[:punct:]]", "-", text)
  text = paste(substr(text,1,10),
               paste(substr(text,12,13), substr(text,15,16), sep=":"),
               substr(text,18,19),
               sep=" ")
}
CleanDesc <- function(text) { # fix the Description
  text = gsub("\\W", " ", text)
}
CleanX <- function(text) { # removes the X on the first line of subsequent pages
  substr(text, 2, nchar(text))
}

# Major Functions
FixHeadersInQuestion <- function(target) {
  HeadersInQuestion = colnames(target) # Gets the wrong headers to fix
  i = 1 
  while(i!=7){
    if(i==1){
      HeadersInQuestion[i] = CleanDt(HeadersInQuestion[i])
    }
    if(i==2){
      HeadersInQuestion[i] = CleanDesc(HeadersInQuestion[i])
    }
    if(i>=3) {
      HeadersInQuestion[i] = CleanX(HeadersInQuestion[i])
    }
    i = i + 1
  }
  target = rbind(HeadersInQuestion,target)
}
PrepareForCleaning <- function(pdf_file, pass_word, pages) {
  if(pages==1){ # If 1 page, do 1 page workflow
    srctxt = extract_tables(pdf_file, password = pass_word,
                            page = 1, area = list(c(67.45,1,830.53,595)),
                            columns = list(c(125,295,355,425,475)), # special for first page
                            guess = "FALSE", method = "decide", output = "data.frame")
    a = srctxt[[1]]
    a = a[-c(1:2),]
    names(a) = c("Date and Time", "Description", "Reference No.", "Debit", "Credit", "Balance")
    a = a[a$Description!="Total Debit",]
    a = a[a$Description!="Total Credit",]
    a = a[a$Description!="ENDING BALANCE",]
  }
  else if(pages>1){ # If more than 1 page, do multi-page workflow
    # Do similarly for 1st page
    srctxt1 = extract_tables(pdf_file, password = pass_word,
                             page = 1, area = list(c(67.45,1,830.53,595)),
                             columns = list(c(125,295,355,425,475)), # special for first page
                             guess = "FALSE", method = "decide", output = "data.frame")
    a = srctxt1[[1]]
    a = a[-c(1:2),]
    names(a) = c("Date and Time", "Description", "Reference No.", "Debit", "Credit", "Balance")
    
    # Then work on the subsequent pages
    srctxt2 = extract_tables(pdf_file, password = pass_word, page = 2:pages,
                             guess = "TRUE", method = "decide", output = "data.frame")
    i = 1
    while(i!=pages){
      b = srctxt2[[0+i]]
      b = FixHeadersInQuestion(b)
      
      names(b) = c("Date and Time", "Description", "Reference No.", "Debit", "Credit", "Balance")
      # Merging Loop
      if(i==1){
        c = b
      }
      else{
        c = do.call("rbind", list(c,b))
      }
      i = i + 1
    }
    z = do.call("rbind", list(a,c))
    print(z)
    z = z[z$Description!="Total Debit",]
    z = z[z$Description!="Total Credit",]
    z = z[z$Description!="ENDING BALANCE",]
  }
}
CleanRows1 <- function(target) { # Setup for cleaning
  target = target %>% mutate(t0=ifelse(`Date and Time`=="", 1, 0)                     # switches
                            ,t1=lag(t0,n=1)
                            ,t2=lag(t0,n=2)
                            ,t3=lag(t0,n=3)
                            ,t4=lag(t0,n=4)
                            ,t5=lag(t0,n=5)
                            ,t6=lag(t0,n=6))
  target[is.na(target)] <- 0
  target = target %>% mutate(a1=ifelse((t0==1 & t1==0 & t2==0) |                      # switch rules for 3-line issues
                                       (t0==1 & t1==1 & t2==0) |
                                       (t0==1 & t1==0 & t2==1 & t3==0 & t4==1 & t5==0 & t6==0), 1, 0)
                            ,a2=ifelse((t0==0 & t1==1 & t2==0) |
                                       (t0==0 & t1==1 & t2==1), 1, 0)
                            ,a3=ifelse( t0==1 & t1==0 & t2==1 , 1, 0)
                            ,g1=lead(a1,n=0)                                          # rules for trigger
                            ,g2=lead(a2,n=1)
                            ,g3=lead(a3,n=2)
                            ,x1=ifelse( g1==1 & g2==1 & g3==1, 1, 0)                  # trigger
                            ,x2=lag(x1,n=1)
                            ,x3=lag(x1,n=2))
  target = target %>% mutate(b1=ifelse((t0==1 & t1==0 & t2==0) |                      # switch rules for 5-line issues
                                       (t0==1 & t1==0 & t2==1)                        , 1, 0)
                            ,b2=ifelse((t0==1 & t1==1 & t2==0 & t3==0) |
                                       (t0==1 & t1==1 & t2==0 & t3==1)                , 1, 0)
                            ,b3=ifelse((t0==0 & t1==1 & t2==1 & t3==0 & t4==0) |
                                       (t0==0 & t1==1 & t2==1 & t3==0 & t4==1)        , 1, 0)
                            ,b4=ifelse((t0==1 & t1==0 & t2==1 & t3==1 & t4==0 & t5==0) |
                                       (t0==1 & t1==0 & t2==1 & t3==1 & t4==0 & t5==1), 1, 0) 
                            ,b5=ifelse((t0==1 & t1==1 & t2==0 & t3==1 & t4==1 & t5==0), 1, 0)
                            ,h1=lead(b1,n=0)                                          # rules for trigger
                            ,h2=lead(b2,n=1)
                            
                            ,h3=lead(b3,n=2)
                            ,h4=lead(b4,n=3)
                            ,h5=lead(b5,n=4)
                            ,y1=ifelse( h1==1 & h2==1 & h3==1 & h4==1 & h5==1, 1, 0)  # trigger
                            ,y2=lag(y1,n=1)
                            ,y3=lag(y1,n=2)
                            ,y4=lag(y1,n=3)
                            ,y5=lag(y1,n=4))
}
CleanRows2 <- function(target) { # CLeaning
  target = target %>% select(-c(t0,t1,t2,t3,t4,t5,t6,
                                a1,a2,a3,
                                g1,g2,g3,
                                b1,b2,b3,b4,b5,
                                h1,h2,h3,h4,h5)
  )
  
  j=1 # merge the 3-line transactions
  seq = (as.numeric(target %>% summarise(ans=sum(x1==1)))+1)
  seq[is.na(seq)] <- 1
  
  while (j != seq) {
    df1 = target %>% filter(x1==1) %>% slice(n=j)
    df2 = target %>% filter(x2==1) %>% slice(n=j)
    df3 = target %>% filter(x3==1) %>% slice(n=j)
    df = rbind(df1,df2,df3)
    df[is.na(df)] <- " "
    df = rbindlist(list(df, setDT(df)[, lapply(.SD, paste, collapse='')])) #adds a fourth observation
    df = df[4]
    target = rbind(target,df)
    j = j + 1
  }
  
  k=1 # merge the 5-line transactions
  seq = (as.numeric(target %>% summarise(ans=sum(y1==1)))+1)
  seq[is.na(seq)] <- 1
  
  while (k != seq) {
    df1 = target %>% filter(y1==1) %>% slice(n=k)
    df2 = target %>% filter(y2==1) %>% slice(n=k)
    df3 = target %>% filter(y3==1) %>% slice(n=k)
    df4 = target %>% filter(y4==1) %>% slice(n=k)
    df5 = target %>% filter(y5==1) %>% slice(n=k)
    df = rbind(df1,df2,df3,df4,df5)
    df[is.na(df)] <- " "
    df = rbindlist(list(df, setDT(df)[, lapply(.SD, paste, collapse='')])) #adds a fourth observation
    df = df[6]
    target = rbind(target,df)
    k = k + 1
  }
  
  target[is.na(target)] <- 0
  target = target %>% mutate(len=str_count(x1))
  
  target = target %>%  # Re-organizing the cleaned rows
    mutate(`Date and Time` = ymd_hm(`Date and Time`)) %>%
    arrange(`Date and Time`) %>% 
    filter((x1==0 & x2==0 & x3==0 & y1==0 & y2==0 & y3==0 & y4==0 & y5==0) | len > 1) %>% 
    select(-c(x1,x2,x3,y1,y2,y3,y4,y5,len))
}

# Consolidated Function
ExtractTxnHistory2CSV <- function(pdf_file, pass_word, pages) {
  target = PrepareForCleaning(pdf_file, pass_word, pages)
  print(target)
  
  target = CleanRows1(target)
  print("Preparing to clean 3/5-liner issues")
  target = CleanRows2(target)
  print("3/5-liner issues cleaned")
  
  target = target %>% 
    mutate(`Date and Time` = as.character(`Date and Time`)) %>%
    separate(`Date and Time`, c('Date','Time'), sep=" ", remove=FALSE) %>%
    mutate(Currency = "PHP")
  target[is.na(target)] = ""
  print("Finalizing output")
  
  write.csv(target,file="output.csv",row.names=FALSE)
  print("Saved")
}

# User Input
setwd("INPUT FOLDER HERE")
ExtractTxnHistory2CSV("INSERT FILE WITH FILE EXT HERE",                  # "transaction_history.pdf"
                      "INSERT PASSWORD HERE",                            # "surname_last-4-digits-of-mobtel"
                      "INSERT NUMBER OF PAGES HERE, NO QUOTATION MARKS") # 2