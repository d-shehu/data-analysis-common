
# Collecting text from a URL can get complicated
# especially if we consider authentication, different
# encoding fomats (HTML, JSON, XML, etc) as well as
# markup itself. For the 1st iteration let's work with
# HTML explicitly and the rvest library.
# To speed up data processing, we should store the soure
# locally.

source(fnGetPath(getwd(), "notebooks", "r", "common", "text_mining", 
                 "corpus.r"))
source(fnGetPath(getwd(), "notebooks", "r", "common", "text_mining", 
                 "selenium.r"))
source(fnGetPath(getwd(), "notebooks", "r", "common", "text_mining", 
                 "parse.r"))
source(fnGetPath(getwd(), "notebooks", "r", "common", "utils", 
                 "files.r"))

classCollector <- setRefClass("collector", 
    fields = list(
        corpus = "corpus",
        selServer = "list",
        selClient = "remoteDriver",
        port = "integer"
    ),
    methods = list(
        initCollector = function(inPort, inCorpus){
            corpus <<- inCorpus
            port <<- inPort
        },
        # Lazy load the Selenium driver to avoid issues with timeouts
        initSelenium = function(){
            # When selenium is idle it periodically needs to be reset
            if(!is.null(selServer) & !is.null(selClient)){
                fnCleanupSelenium(selServer, selClient)
            }
            lsSel <- fnGetSelenium(port, TRUE)
            selServer <<- lsSel$server
            selClient <<- lsSel$client
        },
        acquireDoc = function(url){
            tryCatch({
                # Get a temporary file name locally as aws.s3 requires files
                localFile <- tempfile()

                # Get the source from the URL
                source <- fnExtractSourceFromURL(url)

                # Get the text from the source
                lsMeta <- fnGetMetaData(source, url)

                # Save to temp files using selenium and then pass along to the corpus
                fnDownloadHTML(url, localFile)

                # Meta data was parsed from the source but could have also been passed in
                corpus$addEntry(localFile, lsMeta)
            })
        }
    ) # end methods
) # classCollector