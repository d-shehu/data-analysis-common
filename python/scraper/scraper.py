from fake_useragent import UserAgent
from IPython.display import Image

from selenium import webdriver
from selenium.webdriver import FirefoxOptions
from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys

from selenium.common.exceptions import NoSuchElementException

from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver import ChromeOptions
from selenium.webdriver.chrome.service import Service as ChromiumService
from webdriver_manager.chrome import ChromeDriverManager
from webdriver_manager.core.os_manager import ChromeType

import time

class Scraper:
    def __init__(self, width = 1920, height = 1080):
        # Firefox seems to work better so defaulting to it.
        self.width = width
        self.height = height
        self.browser = self.getFirefoxBrowser(width, height)
        self.original_window = self.browser.current_window_handle

    def __del__(self):
        if self.browser is not None:
            print("Closing old selenium browser instance")
            self.browser.quit()
        else:
            print("Warning: nothing to cleanup as browser not initialized!")

    # Function to construct browser object
    def getFirefoxBrowser(self, width, height):
        browser = None
        try:
            print("Initializing Firefox driver...")
            ua = UserAgent(browsers=['Firefox'])
            user_agent = ua.random # Randomize user agent to avoid getting locked out
            print("Firefox user agent: ", user_agent)
        
            opts = FirefoxOptions()
            opts.add_argument('--headless')
            opts.add_argument(f'--width={width}')
            opts.add_argument(f'--height={height}')
            opts.add_argument(f'user-agent={user_agent}')
            browser = webdriver.Firefox(options=opts)
            browser.maximize_window()
        except Exception as e:
            print("Unable to load browser service: ", e)
            
        return browser

    # TODO: fix width and height parameter
    def getChromeBrowser(self, width, height):
        ua = UserAgent(browsers=['chrome'])
        userAgent = ua.random
        print("Chrome user agent: ", userAgent)
        agentOverride = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/83.0.4103.53 Safari/537.36'
        
        service=ChromiumService(ChromeDriverManager(chrome_type=ChromeType.CHROMIUM).install())
        options = ChromeOptions()
        options.add_argument("--headless")
        options.add_argument("--disable-blink-features=AutomationControlled")
        options.add_argument(f'user-agent={userAgent}')
        options.add_experimental_option("excludeSwitches", ["enable-automation"])
        options.add_experimental_option('useAutomationExtension', False)
        browser = webdriver.Chrome(service=service,options=options)
        browser.maximize_window()
        browser.execute_cdp_cmd('Network.setUserAgentOverride', 
                                {"userAgent": agentOverride})
        browser.execute_script("Object.defineProperty(navigator, 'webdriver', {get: () => undefined})")
        return browser

    def loadPage(self, url):
        isLoaded = False
        
        try:
            if self.browser is not None:
                self.browser.get(url)
                isLoaded = True
            else:
                print("Error: browser service is not initialized")
        except Exception as e:
            print(f"Error: unexpected exception while loading {url}")
            print(e)

        return isLoaded

    def refreshPage(self):
        self.browser.refresh()

    def getCurrentPage(self):
        url = ""
        
        try:
            if self.browser is not None:
                url = self.browser.current_url
        except Exception as e:
            print("Error: unable to get current page url")
            print(e)
            
        return url

    def findPopups(self, wait=1, numPopups=1):
        listPopups = []
        try:
            # Wait for one or more popups
            WebDriverWait(self.browser, timeout).until(EC.number_of_windows_to_be(numPopups+1))

            for window_handle in self.browser.window_handles:
                if window_handle != self.original_window:
                    listPopups.append(window_handle)
        except Exception as e:
            print("Error: unable to scan for popups due to: ", e)

        return listPopups
            
        
    # Assume there are only 2 windows, main and new one
    def waitForPageURL(self, timeout=5, closeNew=True, switchToOriginal=True):
        urlNew = ""
        
        # Assume new tab or window has been loaded
        try:
            start = time.time()
            # Wait for the new tab to open. There should only be 2.
            WebDriverWait(self.browser, timeout).until(EC.number_of_windows_to_be(2))
    
            # Loop through until we find the new tab handle or time out
            while (time.time() - start) < timeout:
                for window_handle in self.browser.window_handles:
                    if window_handle != self.original_window:
                        self.browser.switch_to.window(window_handle)
                        urlNew=self.getCurrentPage()
                        if urlNew != "" and urlNew != "about:blank":
                            break # Found url?
                            
                # Wait a bit more for new tab to load
                if urlNew != "" and urlNew != "about:blank":
                    time.sleep(1)
        except Exception as e:
            print("Error: unexpected exception while waiting for page to load: ", e)

        # Close the newly opened tab or window?
        if closeNew:
            for window_handle in self.browser.window_handles:
                if window_handle != self.original_window:
                    self.browser.switch_to.window(window_handle)
                    self.browser.close()

        # Switch back to the original
        if switchToOriginal:
            self.browser.switch_to.window(self.original_window)

        # If blank assume page couldn't load
        if urlNew == "about:blank":
            print("Warning: new page loaded was blank.")
            urlNew = ""

        return urlNew
        
    def waitForURLToChange(self, url, timeout=5):
        WebDriverWait(self.browser, timeout).until(EC.url_changes(url))
        
    def waitForElementToLoadByXPath(self, elem, timeout=5, suppressError=False):
        return self.waitForElementToLoad(elem, By.XPATH, timeout, suppressError)
        
    def waitForElementToLoad(self, elem, byType=By.ID, timeout=5, suppressError=False):
        isLoaded = False
        
        if self.getElement(elem, byType, timeout, suppressError) is not None:
            isLoaded = True
            
        return isLoaded

    def getElementByXPath(self, elem, timeout=5, suppressError=False):
        return self.getElement(elem, By.XPATH, timeout, suppressError)
        
    def getElement(self, elem, byType=By.ID, timeout=5, suppressError=False):
        matchingElement = None
        
        if self.browser is not None:
            # TODO: timer resolution may not be super accurate.
            elapsedTime = 0
            startTime = time.time()
            while matchingElement is None and (elapsedTime < timeout):
                # print("Waiting for element to load ...")
                try:
                    matchingElement = self.browser.find_element(byType, elem)
                except NoSuchElementException: # still loading?
                    time.sleep(1)
                except Exception as e: # other exception?
                    if not suppressError:
                        print(f"Error: unexpected exception while getting {elem}: ", e)
                elapsedTime = time.time() - startTime
                
            if matchingElement is None and elapsedTime >= timeout:
                if not suppressError:
                    print(f"Error: timed out waiting for {elem} to load. Elapsed time: ", elapsedTime)
        else:
            if not supressError:
                print("Error: browser service is not initialized")

        return matchingElement

    def getElementFromElementByXPath(self, parent, elem, timeout=5, suppressError=False):
        return self.getElementFromElement(parent, elem, By.XPATH, timeout, suppressError)
        
    def getElementFromElement(self, parent, elem, byType=By.ID, timeout=5, suppressError=False):
        matchingElement = None

        if self.browser is not None:
            # TODO: timer resolution may not be super accurate.
            elapsedTime = 0
            startTime = time.time()
            print("getElementFromElement enter...")
            print("Timeout: ", timeout)
            while matchingElement is None and (elapsedTime < timeout):
                # print("Waiting for element to load ...")
                try:
                    matchingElement = parent.find_element(byType, elem)
                except NoSuchElementException: # still loading?
                    time.sleep(1)
                except Exception as e: # other exception?
                    if not suppressError:
                        print(f"Error: unexpected exception while getting {elem}: ", e)
                        
                elapsedTime = time.time() - startTime

            print("getElementFromElement exit...")
            if matchingElement is None and elapsedTime >= timeout:
                if not suppressError:
                    print(f"Error: timed out waiting for {elem} to load. Elapsed time: ", elapsedTime)
        else:
            if not supressError:
                print("Error: browser service is not initialized")

        return matchingElement
            

    def getElementsByXPath(self, elem, timeout=5, suppressError=False):
        return self.getElements(elem, By.XPATH, timeout, suppressError)
        
    def getElements(self, elem, byType=By.ID, timeout=5, suppressError=False):
        matchingElements = None

        if self.browser is not None:
            # TODO: timer resolution may not be super accurate.
            elapsedTime = 0
            keepSearching = True
            matchCount = 0
            startTime = time.time()
            while keepSearching and (elapsedTime < timeout):
                try:
                    matchingElements = self.browser.find_elements(byType, elem)
                except Exception as e:
                    if not suppressError:
                        print(f"Error: unexpected exception while getting {elem}: ", e)
                    break

                # Give more time for elements to load
                if (matchingElements is None or len(matchingElements) == 0):
                    time.sleep(1)
                else:
                    # Found more then iterate again
                    newCount = len(matchingElements)
                    keepSearching = newCount > matchCount
                    matchCount = newCount
                    if keepSearching:
                        time.sleep(1)

                    
                elapsedTime = time.time() - startTime
                
            if matchingElements is None and elapsedTime >= timeout:
                if not suppressError:
                    print(f"Error: timed out waiting for {elem} to load. Elapsed time: ", elapsedTime)
        else:
            if not supressError:
                print("Error: browser service is not initialized")

        return matchingElements
        
    # Scroll over div parent control to get all the items in the list. Useful if the view
    # does not show all child elements and so elements are not loaded.
    def getElementsInScrollableDivByXPath(self, xpathScrollable, timeout=5, suppressError=False):
        # Assume the specified parent element is scrollable
        # Fetch 1st set of visible elements
        matchingElements = self.getElementsByXPath(xpathScrollable, timeout, suppressError)
        
        hasMore = True
        while matchingElements is not None and hasMore:
            matchCount = len(matchingElements)
            if matchCount > 0:
                # Scroll last element into view. Assume this expands the list to include everything
                # before plus new elements previously hidden.
                lastElement = matchingElements[-1]
                self.executeScriptOnElement(lastElement, "arguments[0].scrollIntoView();", timeout)
                
                # Get potentailly new elements
                matchingElements = self.getElementsByXPath(xpathScrollable, timeout, suppressError)
            else:
                print("Error: matched 0 scrollable div elements.")
                break

            # If scrolling reveals more elements keep iterating
            hasMore = len(matchingElements) > matchCount

        return matchingElements
        
    def setElementText(self, elem, text, byType=By.ID):
        result = False
        
        try:
            if self.browser is not None:
                #match = self.browser.find_element(byType, elem)
                match = self.getElement(elem, byType)
                match.clear()
                match.send_keys(text)
                result=True
            else:
                print("Error: browser service is not initialized")
        except:
            print(f"Error: unable to access {elem}. Can't find the field by {byType}.")

        return result

    def clickOnElementByXPath(self, elem, timeout=5):
        return self.clickOnElement(elem, By.XPATH, timeout)
        
    def clickOnElement(self, elem, byType=By.ID, timeout=5):
        result = False
        lastException = None

        if self.browser is not None:
            # Retry if there is a transient loading issue with DOM
            elapsedTime = 0
            startTime = time.time()
            while not result and elapsedTime < timeout:
                try:
                    # TODO: handle if button is obscured by popup
                    self.getElement(elem, byType).click()
                    result=True
                # Save last exception in case there is a non transient error
                except Exception as e:
                    lastException = str(e)
                finally:
                    time.sleep(1) # sleep to give time to load DOM, etc.
                    elapsedTime = time.time() - startTime
                
        if not result:
            print(f"Error: unable to click on element: {elem}")
            if lastException is not None:
                print(e)
            
        return result

    def executeScriptOnElement(self, element, script, timeout = 5):
        executed = False
        result = None
        lastException = None
        
        elapsedTime = 0
        startTime = time.time()
        while not executed and elapsedTime < timeout:
            try:
                result = self.browser.execute_script(script, element)
                executed = True
            except Exception as e:
                lastException = str(e)
            finally:
                time.sleep(1) # sleep to give time to load DOM, etc.
                elapsedTime = time.time() - startTime

        if not executed:
            print(f"Error: unable to execute {script} on {element}") 
            if lastException is not None:
                print(lastException)

        return result

    
    def showBrowserInNotebook(self):
        screenshot = self.browser.get_full_page_screenshot_as_png() # saves screenshot of entire page
        display(Image(screenshot, width=self.width, height=self.height))
