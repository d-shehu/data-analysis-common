library(dplyr)
library(lubridate)
library(stringr)
library(rvest)
library(urltools)
library(xml2)

# TODO: Obviously need a better way to handle this as we added more
# sources both public and private. Possibly having a handler that
# gets passed in by the caller.
KnownSources <- c("medium", "towardsdatascience", "analyticsvidhya")

# Extract source, author, title
# Let's start with the basic approach. Assume the title is the highest
# level (Hx) of a document.
fnExtractTitleFromHTML <- function(doc){
    tryCatch({
         # Start with the title (which may extraneous stuff. Will pick the 1st.
        titles <- doc %>% rvest::html_nodes("title") %>% rvest::html_text()
        # Otherwise try getting it from the headings
        if(length(titles) == 0){
            titles <- doc %>% rvest::html_nodes("h1") %>% rvest::html_text()
        }
        # Try level 2 headings
        if(length(titles) == 0){
            titles <- doc %>% rvest::html_nodes("h2") %>% rvest::html_text()
        }
        # And level 3 headings
        if(length(titles) == 0){
            titles <- doc %>% rvest::html_nodes("h3") %>% rvest::html_text()
        }
        # Let's assume that if the title has other information (author) that's 
        # delimited by a pipe that te 1st element will be the title
        lsTitleTokens <- titles %>% head(1) %>% str_split("\\|", simplify = TRUE)
        lsTitleTokens[1,1]
    },
    error = function(e){
        print(paste("Error while trying to parse title from:", url))
        print(e)
        ""
    })
}

fnGetPublishInfoAVSource <- function(doc){
    tryCatch({
        # Combines the author and the date. Scrub newline, other whitespaces.
        sPublishInfo <- trimws(doc %>% rvest::html_nodes(".publish-info") %>% html_text())
        # Split author and date
        lsPublishInfo <- str_split(sPublishInfo, "—", simplify = TRUE)
        if(length(lsPublishInfo) == 2){
            list(author=lsPublishInfo[[1]], published=lsPublishInfo[[2]])
        }
        else{
            list(author="", published="")
        }
    },
    error = function(e){
        print(paste("Error while trying to parse publish info:", e))
        list(author="", published="")
    })
}

fnGetAuthorFromHTML <- function(source, doc){
    if(source == "medium" | source == "towardsdatascience"){
        doc %>% rvest::html_nodes(".pw-author-name") %>% html_text()
    }
    else if(source == "analyticsvidhya"){
        fnGetPublishInfoAVSource(doc)$author
    }
    else{
        print(paste("Unknown source:", source))
        character(0)
    }
}

fnGetDateFromDateStr <- function(sDate){
    tryCatch({
        publishedDate <- mdy(sDate) 
        if(is.na(publishedDate)){
            # Try it by adding this year's. Some articles on towardsdatascience
            # have the shorthand date format
            publishedDate <- mdy(paste(dateStr, format(Sys.Date(), "%Y")))
        }
        # Still not able to parse?
        if(is.na(publishedDate)){
            print(paste("Error parsing published date:", dateStr))
        }
        publishedDate
    },
    error = function(e){
        print(paste("Error while trying to parse date:", sDate))
        NA
    })
}

fnGetPublishedDateFromHTML <- function(source, doc){
    if(source == "medium" | source == "towardsdatascience"){
        sDate <- doc %>% 
            rvest::html_nodes(".pw-published-date") %>% 
            html_text()
        fnGetDateFromDateStr(sDate)
    }
    else if(source == "analyticsvidhya"){
        fnGetDateFromDateStr(fnGetPublishInfoAVSource(doc)$published)
    }
    else{
        print(paste("Unknown source:", source))
        character()
    }
}

# Mostly a copy/paste from the Data Wrangling project
fnExtractSourceFromURL <- function(url){
    # Returned undefined if can't parse
    source <- NA
    
    # Decompose URL into sub-parts
    tryCatch(expr = {
            # Let's assume URL is of form http(s)://*<domain>.<suffix>/*
            # Source is going to be the domain (NPR, CNN, etc.)
            urlParts <- suffix_extract(domain(url))
            domain <- urlParts$domain
            subdomain <- urlParts$subdomain
            # Prefer domain over subdomain
            if (domain %in% KnownSources){
                source <- domain
            }
            # But try both
            else if (subdomain %in% KnownSources){
                source <- subdomain
            }
        },
        error = function(e){
          # If can't parse let's return undefined
          print(parse("Can't parse url:", url))
          print(paste("Error:", e))
        }
    )
    
    return (source)
}

fnGetMetaData <- function(source, url){
    tryCatch({
        doc <- rvest::read_html(url)
        title <- fnExtractTitleFromHTML(doc)
        author <- fnGetAuthorFromHTML(source, doc)
        published <- fnGetPublishedDateFromHTML(source, doc)

        list(link = url, source = source, title = title, author = author, published = published)
    },
    error = function(e){
        print(paste("Error while trying to parse meta data from:", url))
        print(e)
        
        list(link = url, source = source, title = "", author = "", published = "")
    })
}

# Returns a tibble with just the paragraphs and paragraph number
# Accepts a link or the HTML itself
fnGetParagraphsFromHTML <- function(html){
   tibble(text = rvest::read_html(html) %>% 
            rvest::html_nodes("p") %>% 
            html_text()) %>%
        mutate(paragraph_num = row_number()) 
}

fnDownloadHTML <- function(url, file){
    tryCatch({
        write_html(rvest::read_html(url), file)
    },
    error = function(e){
        print(paste("Error while trying to download:", url))
        print(e)
        FALSE
    })
}