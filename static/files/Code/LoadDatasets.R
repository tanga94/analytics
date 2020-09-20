## This is the script that loads all of the data into R. Most of it is from online and will update automatically 
# when you press the knit button in the template. The Nam data you will have to update it in the excel spreadsheet thats
# I've linked this to


options('getSymbols.yahoo.finance' = FALSE) 
options('getSymbols.warning.4.0' = FALSE) # These two lines are just to activate the Yahoo Finance Plugin
av_api_key("<VCPSMXRJDF1YVZ1M>") # This line activates the Alpha Vantage Plugin
quandl_api_key("cdh-CwnECTSwYYy24YZ7") 

todays_date <- Sys.Date()  # This just creates an object with today's date which I will use throughout this script
current_year <- format(Sys.Date(), "%Y")
next_year <- format(Sys.Date() + years(1), "%Y")



prices <- c("CHRIS/CME_C1",'CHRIS/CME_GC1','CHRIS/CME_W1','CHRIS/ICE_B1') %>%
          tq_get(get  = "quandl",
                 from = ymd(todays_date - years(1)),
                 to = todays_date) %>% 
          select(date,symbol, settle) %>% 
          spread(symbol, settle) %>% 
          rename(Date=date, Maize = `CHRIS/CME_C1`, Gold = `CHRIS/CME_GC1`,
                 Wheat = `CHRIS/CME_W1`, Crude = `CHRIS/ICE_B1`) %>% 
          mutate_if(is.numeric, round,3) %>% 
          drop_na()
          
    

# The code chunk above just created a dataset for commodity prices from Yahoo Finance using their ticker codes. I put it into a neat format that updates continuously

currencies <- c("USD/ZAR", "EUR/ZAR",'GBP/ZAR') %>%   
              tq_get(get = "alphavantage", av_fun = "FX_DAILY", outputsize='full') %>% 
              select(timestamp,symbol,close) %>% 
              spread(symbol,close) %>% 
              rename(Date=timestamp, `EUR-ZAR`=`EUR/ZAR`,`GBP-ZAR`=`GBP/ZAR`,`USD-ZAR`=`USD/ZAR`) %>% 
              mutate(Date=as.Date(Date)) %>% 
              filter(Date >= todays_date - years(1)) %>% 
              gather(Ticker,Value,-Date) %>% 
              mutate_if(is.numeric, round, 3) 

# This part above just created a dataset for ZAR Exchange Rates from Alpha Vantage using their ticker codes. Also updates continously 

sa_inflation <- tq_get(c('ZAFCPICORMINMEI','ZAFCPIALLMINMEI'),
                       from = ymd(todays_date) - years(7),
                       to = todays_date,
                       get = 'economic.data') %>% 
                spread(symbol, price) %>% 
                rename(Core_ind = ZAFCPICORMINMEI, Headline_ind = ZAFCPIALLMINMEI, Date=date) %>% 
                mutate(Headline = (Headline_ind/lag(Headline_ind, 12) - 1)*100) %>% 
                mutate(Core = (Core_ind/lag(Core_ind, 12) - 1)*100) %>% 
                select(Date,Headline,Core) %>% 
                slice(tail(row_number(), 49)) %>% 
                gather(Key, Rate, -Date) %>% 
                mutate_if(is.numeric, round, 3)


# This part just created a dataset for SA CPI and then I calculate year-on-year inflation from there.

sa_industrial <- rdb(ids = 'SARB/MRDEI/voofpr_ma') %>% 
                 select(period,series_name,value) %>% 
                 spread(series_name,value) %>% 
                 rename(Date=period,Index = `Volume of production - Manufacturing (2015=100)`) %>% 
                 mutate(Growth = (Index/lag(Index, 12) - 1)*100) %>% 
                 drop_na(Growth) %>% 
                 slice(tail(row_number(), 49)) %>% 
                 mutate(Date = as.Date(Date)) %>% 
                 select(-Index) %>% 
                 mutate_if(is.numeric, round,2)

# This part created a dataset for SA Industrial Production Index and then I calculated year-on-year growth from there


psce_data <- read_excel(path = 'Data/Namibia Dataset.xlsx', col_names = T, sheet = 'PSCE - LineGraphs') %>%              mutate(Date = as.Date(Date)) %>% 
             mutate_if(is.numeric, round, 3) %>% 
             filter(Date >= head(tail(Date, 31), 1)) 


credit <- read_excel(path = 'Data/Namibia Dataset.xlsx', col_names = T, sheet = 'PSCE - BarCharts') %>% 
          mutate(Date = as.Date(Date))

# Over here, I created dataframes which I will use for all of the PSCE Charts (from Excel Spreadsheet)

# Next, I will create a dataframe for Namibia CPI from Excel spreadsheet, then I calculate core and headline inflation, then the rest just cleans everything up. I have coded it in a way that it will only plot the last 49 rows in the excel spreadsheet (which is the last 4 years of inflation)

nam_inflation <- read_excel(path = 'Data/Namibia Dataset.xlsx', col_names = T, sheet = 'Namibia CPI') %>%                    mutate(Date = as.Date(Date)) %>% 
                 mutate(`Core Inflation` = (Core/lag(Core, 12) - 1)*100) %>% 
                 mutate(`Headline Inflation` = (Headline/lag(Headline, 12) - 1)*100) %>% 
                 select(Date,`Core Inflation`,`Headline Inflation`) %>% 
                 mutate(`12 Month MA (Headline)` = zoo::rollapplyr(`Headline Inflation`,12, 
                                                    mean, partial = TRUE)) %>% 
                 rename(Headline=`Headline Inflation`, Core=`Core Inflation`) %>% 
                 filter(Date >= head(tail(Date, 49), 1)) %>% 
                 mutate_if(is.numeric, round, 2) %>% 
                 gather(name,Value,-Date)

# After this, I have created a dataframe for the Nam Slate Account using the spreadsheet. I have coded it in a way that it always extracts the last 13 rows in the spreadsheet (which are slate account balances for the past year)

slate <- read_excel(path='Data/Namibia Dataset.xlsx', sheet = 'Slate Account', col_names = T) %>%
         mutate(Date=as.Date(Date)) %>% 
         filter(Date >= head(tail(Date, 13), 1)) %>% 
         mutate_if(is.numeric, round, 2) %>% 
         gather(Key, Value, -Date)

# Over here, I just create a dataframe for components based forecasts. It automatically will always take the 1st column and the last 3 columns in the spreadsheet

components <- read_excel(path = 'Data/Namibia Dataset.xlsx', 
                         sheet = 'Inflation Components Forecast', 
                         col_names = T) %>% select(1,tail(names(.), 3)) %>% 
              gather(Date, Value, -Component, convert=TRUE) %>% 
              mutate(Date = excel_numeric_to_date(Date)) %>% 
              mutate_if(is.numeric, round,2)


# Finally, I've loaded the spreadsheets for Short term and medium term inflation models here
x <- c('Adjusted Forecasts','ARIMA Model','STIF Model','Consensus Forecast')  


short_forecasts <- read_excel(path = 'Data/Namibia Dataset.xlsx',
                       sheet = 'Short Term Models',
                       col_names = T) %>% 
                   select(1,tail(names(.), 3)) %>% 
                   gather(Date,Value,-Model, convert=TRUE) %>% 
                   mutate(Date = excel_numeric_to_date(Date)) %>% 
                   spread(Date,Value) %>% 
                   mutate_if(is.numeric, round,1) %>% 
                   mutate(Model = factor(Model, levels = x)) %>% 
                   arrange(Model)



medium_forecasts <- read_excel(path = 'Data/Namibia Dataset.xlsx',
                               sheet = 'Medium Term Models',
                               col_names = T) %>% 
                    select(1,tail(names(.), 12)) %>% 
                    gather(Date, Value, -Scenario, convert=TRUE) %>% 
                    mutate(Date = excel_numeric_to_date(Date)) %>% 
                    mutate_if(is.numeric, round,2) %>% 
                    spread(Scenario, Value) %>% 
                    select(Date,Baseline, `Alternative (Downside)`) 


bop <-  read_excel(path = 'Data/Namibia Dataset.xlsx', 
                     col_names = T, sheet = 'Trade Bal - Current Account') %>% 
        slice(tail(row_number(), 5))



neer_reer <- read_excel(path = 'Data/Namibia Dataset.xlsx', 
                   col_names = T, 
                   sheet = 'NEER-REER') %>% 
        mutate(Date = as.Date(Date)) %>% 
        slice(tail(row_number(), 49)) %>%
        gather(Key, Value, -Date) %>% 
        mutate_if(is.numeric, round, 2)


npl <- read_excel(path = 'Data/Namibia Dataset.xlsx',
                  col_names = T, 
                  sheet = 'FS & NPL') %>% 
       select(-`Financial Stability Index`) %>% 
       mutate(Date=as.Date(Date)) %>% 
       rename(Value = `NPL Ratio`) %>% 
       mutate_if(is.numeric, round, 2) %>% 
       slice(tail(row_number(), 17))


vix <- c("CHRIS/CBOE_VX1") %>%
       tq_get(get  = "quandl",
              from = ymd(todays_date - years(1)),
         to = todays_date) %>% 
       select(trade.date,close) %>% 
       rename(Date = trade.date, Value = close) %>% 
       mutate_if(is.numeric, round, 2) %>% 
       drop_na()


fs <- read_excel(path = 'Data/Namibia Dataset.xlsx',
                 col_names = T, 
                 sheet = 'FS & NPL') %>% 
  select(-`NPL Ratio`) %>% 
  mutate(Date=as.Date(Date)) %>% 
  drop_na() %>% 
  mutate_if(is.numeric, round, 3) %>% 
  rename(Value = `Financial Stability Index`) %>% 
  slice(tail(row_number(), 17))


hp_index <- read_excel(path = 'Data/Namibia Dataset.xlsx', 
                       col_names = T, sheet = 'FNB House Price Index') %>% 
            mutate(Date=as.Date(Date)) %>% 
            mutate(Index= (Index/lag(Index, 12) - 1)*100) %>% 
            mutate(`12 Month MA` = zoo::rollapplyr(`Index`, 12, 
                                         mean, partial = TRUE)) %>% 
            slice(tail(row_number(), 25)) %>% 
            gather(Key,Value,-Date) %>% 
            mutate_if(is.numeric, round, 2)


quarterly_gdp <- read_excel(path = 'Data/Namibia Dataset.xlsx', 
                            col_names = T, sheet = 'Quarterly Real GDP') %>% 
                 mutate(Date=as.Date(Date)) %>% 
                 rename(Value = `Real GDP Growth`) %>% 
                 slice(tail(row_number(), 17)) %>% 
                 mutate_if(is.numeric, round, 2)

annual_gdp <-read_excel(path = 'Data/Namibia Dataset.xlsx', col_names = T, 
                        sheet = 'Annual Growth Forecasts') %>% 
             slice(tail(row_number(), 3)) %>% 
             gather(Key, Value, -Year) %>% 
             mutate_if(is.numeric, round, 2)


output_gap <- read_excel(path = 'Data/Namibia Dataset.xlsx',
                         col_names=T, sheet = 'Output Gap') %>% 
              mutate_if(is.numeric, round, 2) %>% 
              rename(Value = `Output Gap (%)`) %>%
              slice(tail(row_number(), 7))



fiscal_stance <- read_excel(path = 'Data/Namibia Dataset.xlsx', 
                            col_names = T, sheet = 'Fiscal Stance') %>% 
                slice(tail(row_number(), 7)) %>% 
                gather(Key,Value,-Year) %>% 
                mutate_if(is.numeric, round, 2)



dsa <- read_excel(path = 'Data/Namibia Dataset.xlsx', col_names = T,
                  sheet = 'DSA Projections') %>% 
       slice(tail(row_number(), 6)) %>% 
       gather(Key,Value,-Year) %>% 
       mutate_if(is.numeric, round, 2)
