library(rvest)
library(tidyverse)
library(stringr)

## scraper function
ScrapeThePage <- function(theURL){
  
  reviews <- theURL %>% 
    html_nodes(".userPost") 
  
  nReviews <- length(reviews)
  
  comments <- reviews %>%
    html_nodes(".comment") %>%
    html_text() %>%
    as.data.frame(stringsAsFactors=F) %>%
    slice(seq(2,2*nReviews,2)) %>%
    rename(comment = ".") %>%
    mutate(comment = str_replace(comment,"Hide Full Comment",""),
           comment = str_replace(comment,"Comment:",""))
  
  regexp <- "[[:digit:]]+"
  
  stars <- reviews %>%
    html_nodes(".current-rating") %>%
    html_text() %>%
    as.data.frame(stringsAsFactors = F) %>%
    rename(value=".") %>%
    mutate(value = as.numeric(str_extract(value, regexp)))
  
  ratingCategory <- reviews %>%
    html_nodes(".category") %>%
    html_text() %>%
    as.data.frame() %>%
    rename(variable=".") 
  
  ratingDF <- data.frame(stars = stars$value, 
                         variable = ratingCategory$variable) %>%
    mutate(rowID = rep(1:nReviews,each=3)) %>%
    spread(variable,stars)
  
  reviewerInfo <- reviews %>%
    html_nodes(".reviewerInfo") %>%
    html_text() %>%
    as.data.frame(stringsAsFactors=F) %>%
    rename(info = ".") %>%
    mutate(info = str_replace(info,"Reviewer:",""))
  
  
  conditionInfo <- reviews %>%
    html_nodes(".conditionInfo") %>%
    html_text() %>%
    as.data.frame() %>%
    rename(condition=".") %>%
    mutate(condition = substring(condition, 19))
  
  helpful <- reviews %>%
    html_nodes(".helpful") %>%
    html_text() %>%
    as.data.frame() %>%
    rename(helpful=".") %>%
    mutate(helpful = as.numeric(str_extract(helpful, regexp)))
  
  date <- reviews %>%
    html_nodes(".date") %>%
    html_text() 
  
  
  ## collect everything
  
  theReviews <- data.frame(
    reviewer = reviewerInfo$info,
    select(ratingDF,-rowID),
    conditionInfo,
    comments,
    date,
    helpful,
    stringsAsFactors = F
  )
  
}

##
## Put the whole thing into a function 
##

## @knitr ScrapeWebMDFunction
ScrapeDrugWebMD <- function(baseURL){
  
  GetNumberReviews <- read_html(paste0(baseURL,'&pageIndex=0&sortby=3&conditionFilter=-1')) %>%
    html_nodes(".postPaging") %>%
    html_text() 
  
  NumberReviews <- as.numeric(str_split(GetNumberReviews[1]," ")[[1]][4])
  ReviewPerPage <- 5
  NumberPages <- floor(NumberReviews/ReviewPerPage)  
  eps = NumberPages - (NumberReviews/ReviewPerPage)
  if (eps==0){NumberPages <- NumberPages-1}
  
  
  ## run the scraper
  allReviews <- NULL
  
  for (thePageIndex in 0:NumberPages){
    
    #for (thePageIndex in 0:10){
    pageURL <- read_html(paste0(baseURL,'&pageIndex=',thePageIndex,'&sortby=3&conditionFilter=-1'))
    theReviews <- ScrapeThePage(pageURL)
    
    allReviews <- bind_rows(allReviews,
                            theReviews)
  }
  
  return(allReviews)
  
}

## Celexa
theURL <- 'https://www.webmd.com/drugs/drugreview-8603-Celexa-oral.aspx?drugid=8603&drugname=Celexa-oral'
theReviews <- ScrapeDrugWebMD(theURL)
saveRDS(theReviews,'Ghislene/data/Celexa.rds')

## Prozac
theURL <-'https://www.webmd.com/drugs/drugreview-6997-Prozac-oral.aspx?drugid=6997&drugname=Prozac-oral'
theReviews_prozac <- ScrapeDrugWebMD(theURL)
saveRDS(theReviews_prozac,'Ghislene/data/Prozac.rds')

## Lexapro
theURL <- 'https://www.webmd.com/drugs/drugreview-63990-Lexapro+oral.aspx?drugid=63990&drugname=Lexapro+oral'
theReviews_lexapro <- ScrapeDrugWebMD(theURL)
saveRDS(theReviews_lexapro,'Ghislene/data/Lexapro.rds')

## Zoloft
theURL <- 'https://www.webmd.com/drugs/drugreview-35-Zoloft+oral.aspx?drugid=35&drugname=Zoloft+oral'
theReviews_zoloft <- ScrapeDrugWebMD(theURL)
saveRDS(theReviews_zoloft,'Ghislene/data/Zoloft.rds')

## Luvox
theURL <- 'https://www.webmd.com/drugs/drugreview-1089-Luvox-oral.aspx?drugid=1089&drugname=Luvox-oral'
theReviews_luvox <- ScrapeDrugWebMD(theURL)
saveRDS(theReviews_luvox,'Ghislene/data/Luvox.rds')

## Paxil
theURL <- 'https://www.webmd.com/drugs/drugreview-6968-Paxil-oral.aspx?drugid=6968&drugname=Paxil-oral'
theReviews_paxil <- ScrapeDrugWebMD(theURL)
saveRDS(theReviews_paxil,'Ghislene/data/Paxil.rds')








