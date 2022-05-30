fnGetNotebooksDir <- function(){
    notebookDir <- paste(getwd(), "notebook", sep=.Platform$file.sep)
    return(notebookDir)
}

# Wrap some "directory" / S3 "prefix" functions. This holds
# the "raw" data, i.e. the articles,docs as obtained from online
# source.
fnGetDocsPath <- function(kbParentPath){
    notebookDir <- paste(kbParentPath, "docs", sep="/")
    return(notebookDir)
}

# Holds the path to the "parsed" article, i.e. the article
# has been converted from HTML/format to a tidy text and
# is stored in parquet file.
fnGetParsedPath <- function(kbParentPath){
    notebookDir <- paste(kbParentPath, "parsed", sep="/")
    return(notebookDir)
}

# Holds the path to the "processed" article, i.e. the article
# has been converted to the constituent tokens (words, phrases, etc)
# Format is also going to be a parquet file.
fnGetProcessedPath <- function(kbParentPath){
    notebookDir <- paste(kbParentPath, "processed", sep="/")
    return(notebookDir)
}

fnGetMetaDataName <- function(){
    return("metadata.parquet")
}