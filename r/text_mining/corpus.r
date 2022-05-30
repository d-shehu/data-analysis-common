# Functionality for defining and managing a corpus of documents.
# Not necessarily standalone but can be called from other projects

library(arrow) # Should be moved to data.r
library(methods)
library(lubridate)
library(uuid)

source(fnGetPath(getwd(), "notebooks", "r", "common", "utils", 
                 "files.r"))
source(fnGetPath(getwd(), "notebooks", "r", "common", "utils", 
                 "s3.r"))

source(fnGetPath(getwd(), "notebooks", "r", "common", "text_mining", 
                 "analysis.r"))
source(fnGetPath(getwd(), "notebooks", "r", "common", "text_mining", 
                 "parse.r"))
source(fnGetPath(getwd(), "notebooks", "r", "common", "text_mining", 
                 "process.r"))


classCorpusEntry <- setRefClass("corpus_entry", 
    fields = list(
        source = "character",
        link = "character",
        author = "character",
        title = "character",
        description = "character",
        created = "Date",
        last_updated = "Date",
        oid = "character",
        id = "character",
        keywords = "list",
        tags = "list"
    ),
    methods = list(
        # Helper constructor that accepts only those fields that are required
        initEntry = function(inSource, inLink, inAuthor, inTitle, inCreated){
            source <<- inSource
            link <<- inLink
            author <<- inAuthor
            title <<- inTitle
            created <<- inCreated
            #last_updated <<- NULL
            description <<- ""
            oid <<- ""
            id <<- UUIDgenerate(use.time = TRUE)
            keywords <<- list()
            tags <<- list()
        },
        # Uniqueness is defined as the combination of source, link, author, title
        entryExists = function(corpus, inSource, inLink, inAuthor, inTitle){
            corpus %>% 
                filter(source == inSource & link == inLink & 
                        author == inAuthor & title == inTitle) %>%
                select(id)
                
        },
        getNameFromID = function(){
            paste0(id, ".dat")
        },
        toTibble = function(){
            ret <- tibble(`source` = source, `link` = link, `author` = author, `title` = title, `description` = description, 
                    `created` = created, `last_updated` = last_updated, `oid` = oid, `id` = id)
            
            if(length(keywords) > 0){
                ret <- ret %>% mutate(`keywords` = keywords)
            }
            if(length(tags) > 0){
                ret <- ret %>% mutate(`tags` = tags)
            }

            return(ret)
        },
        fromTibble = function(dfRow){
            source <<- dfRow["source"]
            link <<- dfRow["link"]
            author <<- dfRow["author"]
            title <<- dfRow["title"]
            description <<- dfRow["description"]
            created <<- ymd(dfRow["created"])
            if(!is.na(dfRow["last_updated"])){
                last_updated <<- ymd(dfRow["last_updated"])
            }
            oid <<- dfRow["oid"]
            id <<- dfRow["id"]
            if(!is.na(dfRow["keywords"])){
                keywords <<- dfRow["keywords"]
            }
            if(!is.na(dfRow["tags"])){
                tags <<- dfRow["tags"]
            }
        }
    )
)

# This uses "S3" buckets for storing the corpos and related data.
# To facilitate portability across environments, we will allow
# "user" or "operator" to set the following environment variables
# to enable use of S3 buckets.
classCorpus <- setRefClass("corpus", 
    fields = list(
        domain = "character",
        bucket = "character",
        user = "character",
        corpusName = "character",
        prefix = "character",
        entries = "list"
    ),
    methods = list(
        initCorpus = function(inDomain, inBucket, inUser, inCorpusName, doCreate = TRUE) {
            user <<- inUser
            corpusName <<- inCorpusName
            entries <<- list()

            # Convention is to code as fully qualified domain name
            bucket <<- paste("s3", inBucket, inDomain, sep = ".")
            prefix <<- paste(user, corpusName, sep = "/")

            if(doCreate){
                fnCreateBucket(bucket)
                fnCreatePrefix(prefix, bucket)
            }
            # Fetch meta data if it exists
            readFromStore()
        },
        addEntry = function(file, lsMeta){
            tryCatch({
                # All source data lives in "docs" prior to processing
                docsPrefix <- fnGetDocsPath(prefix)

                # Create the "docs" prefix if it does not exist in the bucket
                fnCreatePrefix(docsPrefix, bucket)

                # Create the entry and add it to the corpus
                entry <- classCorpusEntry()
                entry$initEntry(lsMeta$source, lsMeta$link, lsMeta$author, lsMeta$title, lsMeta$published)
                
                # Store the document
                fnPutFile(file, entry$getNameFromID(), bucket, docsPrefix)

                # Append if everything went well
                entries <<- append(entries, entry)

                # TODO: a more robust solution would check that the file was
                # actually written by looking at the length and signature.
                TRUE
            },
            error = function(e){
                print(paste("Error storing text to S3 bucket: ", e))
                FALSE
            })
        },
        getBody = function(inEntry){
            docsPrefix <- fnGetDocsPath(prefix)

            # Given the entry fetch the document and return the body (paragraphs)
            fnGetParagraphsFromHTML(fnGetData(bucket, docsPrefix, inEntry$getNameFromID()))
        },
        readFromStore = function(){
            tryCatch({
                # Read the corpus from S3
                localFile <- tempfile()
                fnGetFile(bucket, prefix, fnGetMetaDataName(), localFile)
                if(file.exists(localFile)){
                    df <- read_parquet(localFile)
                    apply(df, 1, function(row) {
                        entry <- classCorpusEntry()
                        entry$fromTibble(row)
                        entries <<- append(entries, entry)
                    })
                    TRUE # TODO: check that the entries were read
                }
                else{
                    print(paste("Error: not able to read meta data file for corpus:", corpusName))
                    FALSE
                }
            },
            error = function(e){
                print(paste("Error reading corpus from S3 bucket: ", e))
                FALSE
            })
        },
        getMetadataDF = function(){
            # Gather entries into a data frame
            dfAllEntries <- tibble()
            for (entry in corpus$entries) {
                dfAllEntries <- bind_rows(dfAllEntries, entry$toTibble())
            }
            return(dfAllEntries)
        },
        writeToStore = function(){
            tryCatch({
                localFile <- tempfile()
                # Write the corpus to file
                write_parquet(getMetadataDF(), localFile)
                # Store the document in S3
                fnPutFile(localFile, fnGetMetaDataName(), bucket, prefix)
            },
            error = function(e){
                print(paste("Error writing corpus to S3 bucket: ", e))
                FALSE
            })
        },
        getKeywords = function(){
            dfAllTerms <- tibble()
            # Combine all the keywords from all the entries so 
            # we can do td-idf and more sophisticated keyword extraction
            # that isn't limited 
            for(entry in entries){
                dfTerms <- fnGetTerms(fnGetSanitizedSentences(fnGetSentencesFromText(corpus$getBody(entry))))
                dfTerms$id <- entry$id
                dfAllTerms <- bind_rows(dfAllTerms, dfTerms)
            }
            dfAllTerms %>% fnGetTopKeywords(5)
        },
        getRelatedTerms = function(){
            dfAllRelatedTerms <- tibble()
            # Combine all the keywords from all the entries so 
            # we can do td-idf and more sophisticated keyword extraction
            # that isn't limited 
            for(entry in entries){
                dfRelatedTerms <- fnGetPairwiseStats(fnGetSanitizedSentences(fnGetSentencesFromText(corpus$getBody(entry))), FALSE)
                dfRelatedTerms$id <- entry$id
                dfAllRelatedTerms <- bind_rows(dfAllRelatedTerms, dfRelatedTerms)
            }
            dfAllRelatedTerms
        }
    ) # end methods
)

