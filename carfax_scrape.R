library(rvest)
library(RSelenium)
library(data.table)
setwd("C:/Users/john.caddell/Desktop/R web scrapping")

rD <- rsDriver(browser = 'chrome', phantomver = NULL, extraCapabilities = list(chromeOptions = list(useAutomationExtension = FALSE))) 

remDr <- rD$client

carfax <- 'https://www.carfax.com/value/'

remDr$navigate(carfax)

zip = c("10996")
#input zip
zip_input <- remDr$findElement(using = "name", "zip")
zip_input$sendKeysToElement(list(zip))

car1 = c("1FAHP35N39W268027")

car_details = function(cars,zip){
  #input zip
  zip_input <- remDr$findElement(using = "name", "zip")
  zip_input$sendKeysToElement(list(zip))
  #input vin
  vin_input <- remDr$findElement(using = "name", "vin")
  vin_input$sendKeysToElement(list(cars))
  #submit
  remDr$findElement(using = "id", 'btnGetCarfax')$clickElement()
  
  #test tryCatch to make sure page is loaded
  webElem1 <-NULL
  webElem2 <-NULL
  while(is.null(webElem1) & is.null(webElem2)){
    webElem1 <- tryCatch({remDr$findElement(using = 'class name', value = "vehicle-info__retail")},
                        error = function(e){NULL})
    webElem2 <- tryCatch({remDr$findElement(using = 'class name', value = "error-page-text")},
                        error = function(e){NULL})
    #loop until element with name <value> is found in <webpage url>
  }
  print("out of loop")
  
  #get webpage details
  tmp <- read_html(remDr$getPageSource()[[1]])
  #if error grab car details
  z <- tmp %>%
    html_nodes(xpath = '//*[@class="error-page-text error-page-model-make-year-vin"]') %>% 
    html_text()
  error = z
  #if it had the data error will be empty
  if(length(error) == 0){
        error = "NA"
        #find retail value
        z <- tmp %>%
          html_nodes("h1") %>% 
          html_text()
        retail = z[2]
        
        #trade-in value, private sale value, car details
        z <- tmp %>%
          html_nodes("h2") %>% 
          html_text()
        trade = z[1]
        private = z[2]
        car = z[3]
        
        #get car condition
        z <- tmp %>%
          html_nodes("span") %>% 
          html_text()
        trim = z[6]
        condition = z[7]
        mileage = z[8]
  }else{
        retail = "NA"
        trade = "NA"
        private = "NA"
        car = "NA"
        trim = "NA"
        condition = "NA"
        mileage = "NA"
  }

  carDF = data.frame(car = car, retail = retail, trade = trade, 
                     private = private, trim = trim, condition = condition,
                     mileage = mileage, error = error, stringsAsFactors = FALSE)
  #navigate back to search page
  remDr$navigate(carfax)

  return(carDF)
}



carData = data.frame()

for(i in 1:25){
  temp = car_details(vin[i],zip)
  carData = rbind(carData, temp)
}

#fill car variable with those observations that threw an error
carData$car[carData$error != "NA"] = carData$error[carData$error != "NA"]

#split into into three cols
temp = str_split_fixed(carData$car, " ", 3)
#make new variables
carData = carData %>% mutate(year = temp[,1], make = temp[,2], model = temp[,3])

#bask in glory
carData
