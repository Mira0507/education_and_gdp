library(ggplot2)
library(tidyverse)



g <- glimpse
h <- head  
s <- summary




# dt: an original data set
dt <- read_csv("Gender_StatsData.csv")
names(dt) <- c("Country_Name", 
               "Country_Code", 
               "Indicator_Name",
               "Indicator_Code", 
               as.character(1960:2019), 
               "X65")

# key_indicator: indicator of interest
key_indicators <- c("Government expenditure on education, total (% of GDP)",
                    "GDP per capita (Current US$)",
                    "GDP (current US$)")





dt1 <- dt %>%
        select(Country_Name:"2019") %>%
        gather(Year, 
               Value, 
               -c(Country_Name, 
                  Country_Code, 
                  Indicator_Name, 
                  Indicator_Code)) %>%
        filter(Indicator_Name %in% key_indicators,
               !is.na(Value),
               Year >= 2015)

# countries: valid countries
countries <- unique(dt1$Country_Name)
countries1 <- countries[23:length(countries)]

dt2 <- dt1 %>%
        filter(Country_Name %in% countries1) %>%
        mutate(Indicator = case_when(Indicator_Name == 
                                             "GDP (current US$)" ~ 
                                             "Total_GDP",
                                     Indicator_Name == 
                                             "GDP per capita (Current US$)" ~ 
                                             "GDP_per_Capita",
                                     Indicator_Name == 
                                             "Government expenditure on education, total (% of GDP)" ~ 
                                             "Percent_Education_Expenditure")) 

dt3 <- dt2 %>% 
        select(-Indicator_Name, -Indicator_Code) %>% 
        spread(Indicator, Value)

dt4 <- dt3[complete.cases(dt3), ] 

