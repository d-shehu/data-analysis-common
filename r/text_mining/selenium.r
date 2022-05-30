library(RSelenium)

# This is derived from my Data Wrangling class. The code is not shared as this
# implementation is a generalization of that code which I will keep static 
# to ensure chain of custody.

# Please note that Selenium is a bit painful to use as it's not entirely stable 
# and requires periodic resetting. 
# Function accepts a selenium port and doInit which actually initializes selenium
# by downloading some dependencies.
fnGetSelenium <- function(port, doInit=F){
  
    # Using headless Firefox browser since that's better supported
    selClientServer <- rsDriver(browser = "firefox", 
                    port = port, 
                    check = doInit, 
                    verbose = T, 
                    #Run the Browser headless
                    extraCapabilities = 
                    list("moz:firefoxOptions" = list(
                        args = list("--headless", 
                                    "--no-sandbox",
                                    "--disable-dev-shm-usage",
                                    "--window-size=1920x1080",
                                    "start-maximised"))
                    ))
    
    # Establish a connect to the client which will be used to interact with the websites
    selServer <- selClientServer[["server"]]
    selClient <- selClientServer[["client"]]

    return (list(server=selServer, client=selClient))
}

# Check the status of Selenium
fnIsSeleniumReady <- function(selClient){
    bRet = FALSE

    tryCatch({
        bRet <- selClient$getStatus()$ready
    },
    error = function(err){
        # Ignoring error
    })

    return (bRet)
}

# Close connection and cleanup
fnCleanupSelenium <- function(selServer, selClient){
  
    # Selenium session may self-terminate.
    tryCatch({
        selClient$close()
        selServer$stop()

        # Sleep a few seconds (best guess) to give selenium time to cleanup.
        Sys.sleep(3)
    },
    error = function(err){
        # Ignoring error. Server and client may be down already.
    })
  
    # Now do the cleanup as necessary
    tryCatch({  
        # Occasionally what happens is that selenium will "time out", i.e. close
        # the session bu the R object will stick around as will the sys process.
        # So let's add a kill process function that should remove orphaned/zombie.
        pid <- selServer$process$get_pid()

        # See if the process is still alive and if so kill it
        print("Checking client service ...")
        while (system(paste("ps aux | grep", pid, "| grep -v grep")) == 0){
            print("Killing client service: ", pid)
            system(paste("kill" , pid))
            Sys.sleep(2)
        }
        
        # Sometimes the parent process is also kept alive. Try to kill it also.
        # Obviously this assumes 1 session per host/environment. 
        print("Checking parent service ...")
        sService <- "ps aux | grep '[c]hromedriver' | awk -F ' ' 'FNR == 1 {print $2}'"
        pid <- system(sService)
        while(pid > 0){
            print(paste("Killing driver Selenium pid: ", pid))
            system(paste("kill", pid))
            # Wait a few seconds before trying again
            Sys.sleep(2)
            pid <- system(sService)
        }
    },
    error = function(err){
        print(paste("Couldn't clean up connection to Selenium driver and client"))
    })
}

fnGetHTMLFromSelClient <- function(selClient, url, doScroll=FALSE){

  tryCatch({
    selClient$navigate(url)
    
    if (doScroll){
      # For live feeds, there doesn't seem to be a mechanism to determine
      # how much data is there or even if the feed has been exhausted.
      # So let's scroll up to 10 times with each time fetching the next "batch".
      for (i in 1:10) {
        print(paste("Scrolling to 'batch'", i))
        selClient$executeScript("window.scrollTo(0, document.body.scrollHeight);")
        Sys.sleep(3)
        # Alternative way to scroll.
        #webElem <- selClient$findElement("css", "body")
        #webElem$sendKeysToElement(list(key = "end"))
      }
    }
    
    selClient$getPageSource(header = TRUE)[[1]] %>% read_html()
  },
  error = function(e){
    print(paste("Error getting HTML from Selenium client: ", e))
    return(character(0))
  })
  
  return (html)
}

fnSaveHTMLToFile <- function(selClient, url, filepath){
  # Now let's write out an individual articles in raw form (HTML). 
  tryCatch({
    print(paste("Reading from:", url))

    # Use Selenium to grab the article
    selClient$navigate(url)
    pageSource <- selClient$getPageSource()

    print(paste("Saving to:", filepath))
    # TODO: is it always just the first element?
    write(pageSource[[1]], filepath)

    # TODO: no explicit error checking and we assume document was written
    # if there was no exception.
    TRUE
  },
  error = function(e){
    print(paste("Error while saving webpage to file:", e))
    FALSE
  })
}