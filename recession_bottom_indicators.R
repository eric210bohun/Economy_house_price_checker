# Use of the below government statistics to determine when Sheffield housing market reaches a new equilibrium point
# unemployment stops decreasing
# house prices stop decreasing
# UK GDP stops falling
# CPI stabilizes 

# load libraries    
library(tidyverse)
library(rvest)
library(readxl)
library(gmailr)
library(lubridate)
library(gridExtra)
library(ggthemes)
library(httr)
library(stringr)
library(gdata)
#gm_auth()
#GMAILR_OAUTH_CLIENT="C:/Users/eric2/Desktop/Projects/gmailR_jason_file_location/client_secret_921351603278-785sgkibfgkgqp8pcunqn7nv6jiuqnkv.apps.googleusercontent.com.json"
#gm_auth_configure(key = "921351603278-785sgkibfgkgqp8pcunqn7nv6jiuqnkv.apps.googleusercontent.com", secret = "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", path = "C:\\Users\\eric2\\AppData\\Local/gmailr/gmailr/client_secret_921351603278-785sgkibfgkgqp8pcunqn7nv6jiuqnkv.apps.googleusercontent.com.json")
gm_auth_configure(path = "C:\\Users\\eric2\\AppData\\Local/gmailr/gmailr/client_secret_921351603278-785sgkibfgkgqp8pcunqn7nv6jiuqnkv.apps.googleusercontent.com.json")
# load data tibble
load(file = "C:/Users/eric2/Desktop/Projects/bottom/data1.RData")

# define urls as objects
  #employment rate - quarterly - percent in employment 16-64 - Mar-May latest on 21.07.20
url1 <- "https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/employmentandemployeetypes/timeseries/lf24/lms"
  #GDP - quarterly - millions £ - April on 21.07.20 - .highcharts-halo
url2 <- "https://www.ons.gov.uk/economy/grossdomesticproductgdp/timeseries/abmi/qna"
  #OR USE ESTIMATED QUARTERLY GDP? 
#url2 <- "https://www.ons.gov.uk/economy/grossdomesticproductgdp/timeseries/abmi/pn2"
  #consumer price index - monthly - percentage change - ALL ITEMS - June on 21.07.20
url3 <- "https://www.ons.gov.uk/economy/inflationandpriceindices/timeseries/l55o/mm23"
  #UK mean house prices - £ inclues 3 month lag QUARTERLY
url4 <- "https://www.ons.gov.uk/economy/inflationandpriceindices/bulletins/housepriceindex/previousReleases"
  #Sheffield mean house prices - £ inclues 3 month lag QUARTERLY
url5 <- "https://www.gov.uk/housing-local-and-community/land-registration"
  #Would Bank of England Interest base rate LIVE DATA
url6 <- "https://www.bankofengland.co.uk/boeapps/database/Bank-Rate.asp#"
  #Real UK House Prices
url10 <- "https://fred.stlouisfed.org/series/QGBR628BIS"

#URL1 - quarterly unemployment - currently running six months behind today's date
h1 <- read_html(url1)
p1 <- h1 %>% html_nodes("table") %>% html_text()

#URL2 - quarterly GDP - running as intended
h2 <- read_html(url2)
p2 <- h2 %>% html_nodes("table") %>% html_text()

#URL3 - CPIH - inflation - running as intended - but what dates are inflation available for?? THREE MONTH LAG??
h3 <- read_html(url3)
p3 <- h3 %>% html_nodes("table") %>% html_text()

#URL4 - UK house prices - running but little confidence in it (b/c of the actions based on materials below - see meanUKhouseurlpartxls)
# extract nodes from html object (code from cattle_scrape_EC.R also has a dynamic URL scrape)
h7 <- read_html(url4)
p7 <- h7 %>% html_nodes("h2 a") %>% html_text()
# extract current year
year_no <- parse_number(p7[1])
# calculate the current month for scrape url
month_poss <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
month <- as.character(na.omit(str_match(parse_character(p7[1]), month_poss)))
# apply dynamic url
url4.1 <- paste("https://www.ons.gov.uk/economy/inflationandpriceindices/bulletins/housepriceindex/", month, year_no, sep = "")
h4 <- read_html(url4.1)
p4 <- h4 %>% html_nodes("div a") %>% html_attr("href")

#URL5 - Sheffield House prices - running and the below section has been adjusted to allow for data capture given the publication of non-Sheff house data hyperlinks
# extract nodes from html object 
h8 <- read_html(url5)
p8 <- h8 %>% html_nodes("div a") %>% html_attr("href")
if(str_detect(p8[64], "uk-house-price-index-data-downloads-") == TRUE) Sheff_house_tail <- p8[64]
if(str_detect(p8[64], "uk-house-price-index-data-downloads-") == FALSE) ifelse(str_detect(p8[65])==TRUE, Sheff_house_tail <- p8[65], Sheff_house_tail <- p8[66])
url5.1 <- paste("https://www.gov.uk", Sheff_house_tail, sep = "")
h5 <- read_html(url5.1)
p5 <- h5 %>% html_nodes("div a") %>% html_attr("href")

#URL6 - BOE interest rates
h6 <- read_html(url6)
p6 <- h6 %>% html_nodes("table") %>% html_text()

#URL10 - Real UK House prices - CPI deflated
h10 <- read_html(url10)
p10 <- h10 %>% html_nodes("table") %>% html_text()


#          --------    actions upon materials downloaded from URLS   ---------  
# download spreadsheet from latest national house price web page
meanUKhouseurlpartxls <- p4[84]
url4.2 <- paste("https://www.ons.gov.uk",meanUKhouseurlpartxls, "&format=csv", sep = "")
download.file(url4.2, "C:/Users/eric2/Desktop/Projects/bottom/LatestNatHouse.csv")
# extraction of latest  national house price
Nat_house_data <- (read_csv("C:/Users/eric2/Desktop/Projects/bottom/LatestNatHouse.csv", col_names = FALSE))

# download spreadsheet from latest Sheffield house price web page
## there is an error with url 5.2 -> perhaps URL5/5.1 are scraping incorrectly resulting in 30/11/23 the p5[45] only extracted the following which wasn't enough to extract the latest Sheff house price data: #using-or-publishing-our-price-paid-data"
url5.2 <- p5[45]
download.file(url5.2, "C:/Users/eric2/Desktop/Projects/bottom/LatestSheffHouse.csv")
# extraction of latest  Sheffield house price
Sheff_house_data <- (read_csv("C:/Users/eric2/Desktop/Projects/bottom/LatestSheffHouse.csv", col_names = FALSE))
Sheff_house_data <- Sheff_house_data %>% filter(X2 == "Sheffield")

# process the BOE table character strings
time_warp <- trimws(p6[1])
time_warp <- str_remove_all(time_warp, "\r")
time_warp <- str_remove_all(time_warp, "\n")
time_warp <- str_remove_all(time_warp, "\t")
time_warp <- str_remove(time_warp, "Date ChangedRate")
time_warp_d <- str_extract_all(time_warp,"[0-9]{1,2} [A-Z][a-z][a-z] [0-9]{1,2}")
time_warp_r <- str_remove_all(time_warp,"[0-9]{1,2} [A-Z][a-z][a-z] [0-9]{1,2}")
time_warp_r <- str_replace_all(time_warp_r, "  ", " ")
time_warp_r <- strsplit(time_warp_r, split = " ")
time_warp_r <- lapply(time_warp_r, function(x) x[x!=""])
x <- today()
y <- as.Date.POSIXct(as.POSIXlt(today())-92*86400)
time_warp_d <- list2DF(time_warp_d, nrow = 0L)
time_warp_r <- list2DF(time_warp_r, nrow = 0L)
time_warp_d <- rename.vars(time_warp_d, from = "", to = "Rate_Change_Date")
time_warp_r <- rename.vars(time_warp_r, from = "", to = "BOE_Rate")
time_warp_d <- tibble(time_warp_d)
time_warp_d <- strptime(time_warp_d[[1]], "%d %b %y", tz = "GMT")
y <- strptime(y, "%Y-%m-%d", tz = "GMT")
x <- strptime(x, "%Y-%m-%d", tz = "GMT")
time_warp_r <- tibble(time_warp_r)
time_warp_r <- as.numeric(time_warp_r[[1]])

# process the St Louis Federal Reserve Economic Data RE UK Real Property Prices
Real_UK <- trimws(p10)
Real_UK <- str_extract(Real_UK,"[0-9][0-9][0-9].[0-9][0-9][0-9][0-9]")

# wrangle html text strings into numeric objects
p1.1 <- p1 %>% str_replace_all(pattern=" ", repl="")
employment <- as.numeric(str_extract(p1.1[2],"\\d\\d.\\d$"))
p2.1 <- p2 %>% str_replace_all(pattern=" ", repl="")
GDP <- as.numeric(str_extract(p2.1[2],"\\d*$"))
p3.1 <- p3 %>% str_replace_all(pattern=" ", repl="")
CPI <- ifelse(str_detect(p3.1[2], "-\\d{1,2}.\\d$"), as.numeric(str_extract(p3.1[2],"-\\d{1,2}.\\d$")), as.numeric(str_extract(p3.1[2],"\\d{1,2}.\\d$")))
# data1 -> Sheffield_mean-house_price needs to lose one decimal place
ShefMeanHouse <- as.numeric(Sheff_house_data$X4[nrow(Sheff_house_data)])
UKMeanHouse <- as.numeric(Nat_house_data$X2[nrow(Nat_house_data)])
# the following allows for scraping historical interest rates - just swap x for y (y = a date three months ago)
if(x >  time_warp_d[1]) BaseRate <- as.numeric(time_warp_r[1])
if(x <  time_warp_d[1]) BaseRate <- time_warp_r[max(which(time_warp_d > y)) + 1]

# add a row with the scraped data to data file
data1 <- add_row(data1,"Employment_Rate" = employment, "CPIH" = CPI, "GDP" = GDP, "Sheffield_Mean_House_Price" = ShefMeanHouse, "UK_Mean_House_Price" = UKMeanHouse,"Interest_Rate" = BaseRate,"Date" = today(), "Real_UK_House" = Real_UK)

# association check - Sheffield House Prices with the other variables
#association_of_other_variables_with_Sheffield_house_prices <- data %>% summarize(cor(data$Mean_Sheffield_House_Prices, data$GDP), cor(data$Mean_Sheffield_House_Prices, data$`%_in_employment`), cor(data$Mean_Sheffield_House_Prices,data$`%_CPI_changes`), cor(data$Mean_Sheffield_House_Prices, data$`Mean_Eng/Wal_House_Prices`))
#sink("~/projects/bottom/HousePriceAssociationCheck.txt")
#print(association_of_other_variables_with_Sheffield_house_prices)
#sink()

# Tweaks to the data for plotting
#DFtall <- data1 %>% pivot_longer(names_to = "Mean_House_Prices", values_to = "Value", 4:5)
#data2 <- data1 %>% add_column("Inflation_Decimal" =data1$CPIH*1/100)
#data2 <- data2 %>% add_column("Inf_Adj_UK" = (data2$UK_Mean_House_Price * (1-data2$Inflation_Decimal)))
#data2 <- data2 %>% add_column("Inf_Adj_Sheff" = (data2$Sheffield_Mean_House_Price * (1-data2$Inflation_Decimal)))

# data plots with trendlines
g1 <- data1 %>% ggplot() +
        geom_line(data = data1, aes(x = Date, y = Sheffield_Mean_House_Price), color = "blue") +
        geom_line(data = data1, aes(x = Date, y = UK_Mean_House_Price), color = "red") + 
        scale_x_date() + theme_economist() + 
        annotate("text", x = as.Date("2020-12-01"), y = as.numeric(270000), label = "UK Mean House Prices") + annotate("text", x = as.Date("2022-09-01"), y = as.numeric(190000), label = "Sheffield Mean House Prices") + annotate("text", x = as.Date("2020-09-01"), y = as.numeric(300000),size = unit(3, "pt"), label = "Source: UK Office for National Statistics") +
        xlab('Historic / Scrape Dates') +
        ylab('Mean House Prices £')
#g1 <- data1 %>% ggplot(aes(x=`Date`, y=`Sheffield_Mean_House_Price`)) +
#       geom_point() + geom_smooth(method = "loess", se = FALSE) +
#       scale_x_date() + theme_economist() + xlab("Scrape Date - Mean Sheff House Price") + ylab("£ in thousands")
#g2 <- data1 %>% ggplot(aes(x=`Date`, y=`UK_Mean_House_Price`)) +
#       geom_point() + geom_smooth(method = "loess", se = FALSE) +
#       scale_x_date() + theme_economist() + xlab("Scrape Date - Mean UK House Price") + ylab("£ in thousands")
g2 <- data1 %>% ggplot(aes(x=`Date`, y=`Interest_Rate`)) +
      geom_point() + geom_smooth(method = "gam", se = FALSE) +
      scale_x_date(name = "Bank of England Base Interest Rate") + annotate("text", size = unit(3, "pt"), x = as.Date("2020-11-01"), y = as.numeric(6), label = "Source: Bank of England") + scale_y_continuous(limits=c(0,6), breaks=seq(1,6), name = "Percentage Base Rate %") + theme_economist_white()
g3 <- data1 %>% ggplot(aes(x=`Date`, y=`Employment_Rate`)) +
       geom_point() + geom_smooth(method = "loess", se = FALSE) +
       scale_x_date() + annotate("text", x = as.Date("2021-05-01"), y = as.numeric(76.5), size = unit(3, "pt"), label = "Source: UK Office for National Statistics") + theme_economist_white() + xlab("Scrape Date / Historic Quarterly UK % in Employment") + ylab("Percent")
g4 <- data1 %>% ggplot(aes(x=`Date`, y=`CPIH`)) +
       geom_point() + geom_smooth(method = "loess", se = FALSE) +
       scale_x_date() + annotate("text", x = as.Date("2021-02-01"), y = as.numeric(9.5), size = unit(3, "pt"), label = "Source: UK Office for National Statistics") + theme_economist_white() + xlab("CPIH price changes monthly - inflation/deflation") + ylab("Percent change in CPIH")
g5 <- data1 %>% ggplot(aes(x=`Date`, y=GDP)) +
       geom_point() + geom_smooth(method = "loess", se = FALSE) +
       scale_x_date() + annotate("text", x = as.Date("2022-09-01"), y = as.numeric(460000), size = unit(3, "pt"), label = "Source: UK Office for National Statistics") + theme_economist_white() + xlab("Scrape Date / Historic Quarterly GDP") + ylab("£ in millions")
#g8 <- data2 %>% ggplot() + 
#      geom_line(data = data2, aes(x = Date, y = Inf_Adj_Sheff), color = "blue") +
#      geom_line(data = data2, aes(x = Date, y = Inf_Adj_UK), color = "red") + 
#      scale_x_date() + theme_economist() + 
#      annotate("text", x = as.Date("2020-12-01"), y = as.numeric(270000), label = "UK Mean CPI Deflated Prices") + annotate("text", x = as.Date("2022-09-01"), y = as.numeric(175000), label = "Sheffield Mean CPI Deflated Prices") +
#      xlab('Historic / Scrape Dates') +
#      ylab('House Prices Adj for Inflation £')
g8 <- data1 %>% ggplot(aes(x=`Date`, y=`Real_UK_House`)) + 
      geom_smooth(method = "loess", se = TRUE, color = "red") +
      scale_x_date() + theme_economist() + 
      annotate("text", x = as.Date("2022-01-01"), y = as.numeric(110), label = "UK Mean CPI Deflated (Real) House Prices") + annotate("text", x = as.Date("2020-09-01"), y = as.numeric(124), size = unit(3, "pt"), label = "Source: St Louis Federal Reserve Economic Data") +
      xlab('Historic / Scrape Dates') +
      ylab('Real House Prices: 100 = 2010 Mean UK Price')

g6 <- grid.arrange(g1, g8)
g7 <- grid.arrange(g2, g3, g4, g5, nrow = 2)

# save both plots
ggsave("C:/Users/eric2/Desktop/Projects/House_prices.png", plot = g6)
ggsave("C:/Users/eric2/Desktop/Projects/GDP_CPI_employment.png", plot = g7)

# set up of email alert
email_eric <- gm_mime() %>%
  gm_from("eric210bohun@gmail.com") %>%
  gm_to("eric210bohun@gmail.com") %>%
  gm_text_body("Check out what's happening to the economy - the .txt attachment measures correlations between A)Sheffield/UK mean house prices (and the same inflation adjusted) and B)other indicators - in order, left to right on the .txt as follows: GDP; %_in_employment;%_CPIH_changes; Mean_UK_House_Prices; BOE_base_interest_rate") %>%
  gm_subject("Economic Indicators RE House Prices") %>%
  gm_attach_file("C:/Users/eric2/Desktop/Projects/House_prices.png") %>%
  gm_attach_file("C:/Users/eric2/Desktop/Projects/GDP_CPI_employment.png")
#  gm_attach_file("~/projects/bottom/HousePriceAssociationCheck.txt")
gm_send_message(email_eric)

# save newly acquired data in the data object
save(data1, file = "C:/Users/eric2/Desktop/Projects/bottom/data1.RData")