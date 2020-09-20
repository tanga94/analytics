
# These lines calculate all of the year-on-year/monthly changes that you see in the red boxes on the dashboard. I have done this in a way that you won't ever have to make changes to get the right figures


return_maize <- prices %>% 
                select(Date,Maize) %>%          
                filter(month(Date) == month(todays_date), year(Date) == year(todays_date)) %>%
                arrange(Date) %>% 
                slice(1L, n()) %>% 
                mutate(Return=(Maize/lag(Maize) - 1)*100) %>% 
                mutate_if(is.numeric, round, 2)

return_wheat <- prices %>% 
                select(Date,Wheat) %>%          
                filter(month(Date) == month(todays_date), year(Date) == year(todays_date)) %>%
                arrange(Date) %>% 
                slice(1L, n()) %>% 
                mutate(Return=(Wheat/lag(Wheat) - 1)*100) %>% 
                mutate_if(is.numeric, round, 2)

return_crude <- prices %>% 
                select(Date,Crude) %>%          
                filter(month(Date) == month(todays_date), year(Date) == year(todays_date)) %>%
                arrange(Date) %>% 
                slice(1L, n()) %>% 
                mutate(Return=(Crude/lag(Crude) - 1)*100) %>% 
                mutate_if(is.numeric, round, 2)

return_gold <- prices %>% 
               select(Date,Gold) %>%          
               filter(month(Date) == month(todays_date), year(Date) == year(todays_date)) %>%
               arrange(Date) %>% 
               slice(1L, n()) %>% 
               mutate(Return=(Gold/lag(Gold) - 1)*100) %>% 
  mutate_if(is.numeric, round, 2)

return_euro <- currencies %>% 
               spread(Ticker, Value) %>% 
               select(Date,`EUR-ZAR`) %>% 
               filter(month(Date) == month(todays_date), year(Date) == year(todays_date)) %>%
               arrange(Date) %>% 
               slice(1L, n()) %>% 
               mutate(Return=((`EUR-ZAR`/lag(`EUR-ZAR`) - 1)*100)*-1) %>% 
               mutate_if(is.numeric, round, 2)

return_usd <- currencies %>% 
              spread(Ticker, Value) %>% 
              select(Date,`USD-ZAR`) %>% 
              filter(month(Date) == month(todays_date), year(Date) == year(todays_date)) %>%
              arrange(Date) %>% 
              slice(1L, n()) %>% 
              mutate(Return=((`USD-ZAR`/lag(`USD-ZAR`) - 1)*100)*-1) %>% 
              mutate_if(is.numeric, round, 2)

return_gbp <- currencies %>% 
              spread(Ticker, Value) %>% 
              select(Date,`GBP-ZAR`) %>% 
              filter(month(Date) == month(todays_date), year(Date) == year(todays_date)) %>%
              arrange(Date) %>% 
              slice(1L, n()) %>% 
              mutate(Return=((`GBP-ZAR`/lag(`GBP-ZAR`) - 1)*100)*-1) %>% 
              mutate_if(is.numeric, round, 2)

annual_inflation_sa <- sa_inflation %>% 
                       spread(Key,Rate) %>% 
                       slice(n()) %>% 
                       mutate_if(is.numeric, round, 2)

annual_ip_sa <- sa_industrial %>% 
                slice(n()) %>% 
                mutate_if(is.numeric, round, 2)


psce_annual_growth <- psce_data %>% 
                      filter(Date==head(tail(Date, 19), 1)| Date==head(tail(Date, 7), 1)) %>% 
                      mutate(Growth=(`Total PSCE`/lag(`Total PSCE`) - 1)*100) %>% 
                      select(Date, Growth) %>% 
                      slice(n()) %>% 
                      mutate_if(is.numeric, round, 2)

average_hh <- psce_data %>% 
              select(Date,`Household PSCE`) %>% 
              mutate(Growth = (`Household PSCE`/lag(`Household PSCE`, 12) - 1)*100) %>% 
              filter(Date >= floor_date(Sys.Date(), 'year')) %>% 
              select(-`Household PSCE`) %>% 
              mutate(Average=mean(Growth)) %>% 
              slice(n()) %>% 
              mutate_if(is.numeric, round, 2)

average_business <- psce_data %>% 
                    select(Date,`Business PSCE`) %>% 
                    mutate(Growth = (`Business PSCE`/lag(`Business PSCE`, 12) - 1)*100) %>% 
                    filter(Date >= floor_date(Sys.Date(), 'year')) %>% 
                    select(-`Business PSCE`) %>% 
                    mutate(Average=mean(Growth)) %>% 
                    slice(n()) %>% 
                    mutate_if(is.numeric, round, 2)

average_psce <- psce_data %>% 
                select(Date,`Total PSCE`) %>% 
                mutate(Growth = (`Total PSCE`/lag(`Total PSCE`, 12) - 1)*100) %>% 
                filter(Date >= floor_date(Sys.Date(), 'year')) %>% 
                select(-`Total PSCE`) %>% 
                mutate(Average=mean(Growth)) %>% 
                slice(n()) %>% 
                mutate_if(is.numeric, round, 2)

latest_inflation <- nam_inflation %>% 
                    filter(name == 'Headline') %>% 
                    slice(n()) %>% 
                    mutate_if(is.numeric, round, 2)

domestic_inflation_ann_average <- nam_inflation %>% 
                                  filter(Date >= floor_date(Sys.Date(), 'year')) %>% 
                                  filter(name == 'Headline') %>% 
                                  mutate(Average = mean(Value)) %>% 
                                  slice(n()) %>% 
                                  mutate_if(is.numeric, round, 2)

monthly_inflation <- read_excel(path = 'Data/Namibia Dataset.xlsx', col_names = T, sheet = 'Namibia CPI') %>% 
                     mutate(Date=as.Date(Date)) %>% select(-Core) %>% 
                     mutate(Inflation = (Headline/lag(Headline) - 1)*100) %>% 
                     mutate_if(is.numeric, round, 2) %>% 
                     slice(n())

pump_price <- read_excel(path = 'Data/Namibia Dataset.xlsx', 
                         col_names = T, sheet = 'Pump Price') %>% 
              mutate(Change = (Petrol - lag(Petrol))*100) %>% 
              slice(n())


neer_reer_change <- read_excel(path = 'Data/Namibia Dataset.xlsx', 
                               col_names = T, 
                               sheet = 'NEER-REER') %>% 
                    mutate(Date = as.Date(Date)) %>% 
                    slice(tail(row_number(), 49)) %>% 
                    mutate_at(vars(`NEER`,`REER`), ~(./lag(.) - 1)*100) %>% 
                    slice(n()) %>% 
                    mutate_if(is.numeric, round, 2) 


bop_change <- read_excel(path = 'Data/Namibia Dataset.xlsx', 
                         col_names = T, sheet = 'Trade Bal - Current Account') %>% 
              slice(tail(row_number(), 2)) 


return_vix <- vix %>% 
              filter(month(Date) == month(todays_date), year(Date) == year(todays_date)) %>% 
              arrange(Date) %>% 
              slice(1L, n()) %>% 
              mutate(Return=((Value/lag(Value) - 1)*100)*-1) %>% 
              mutate_if(is.numeric, round, 2)


return_hp <- hp_index %>% 
             filter(Key == 'Index') %>% 
             slice(n())


return_npl <- npl %>% 
              slice(n())

return_fs <- fs %>% 
             slice(n()) %>% 
             mutate_if(is.numeric, round, 2)

current_output_gap <- output_gap %>% 
                      filter(Year == current_year) 

annual_growth_forecasts <- read_excel(path = 'Data/Namibia Dataset.xlsx', col_names = T, 
                                      sheet = 'Annual Growth Forecasts') %>% 
                           slice(tail(row_number(), 3)) %>% 
                           mutate_if(is.numeric, round, 2)

annual_growth_forecasts <- read_excel(path = 'Data/Namibia Dataset.xlsx', col_names = T, 
                                      sheet = 'Annual Growth Forecasts') %>% 
                           slice(tail(row_number(), 3)) %>% 
                           mutate_if(is.numeric, round, 2) %>% 
                           filter(Year == next_year)


current_annual_forecast <- read_excel(path = 'Data/Namibia Dataset.xlsx', col_names = T, 
                                      sheet = 'Annual Growth Forecasts') %>% 
                           slice(tail(row_number(), 3)) %>% 
                           mutate_if(is.numeric, round, 2) %>% 
                           filter(Year == current_year)


latest_quarterly_gdp <- quarterly_gdp %>% 
                        slice(n())


current_dsa <- dsa %>% 
               filter(Year==current_year) %>% 
               mutate_if(is.numeric, round, 1)

current_fiscalstance <- fiscal_stance %>% 
                        filter(Year == current_year) %>% 
                        mutate_if(is.numeric, round, 1)

