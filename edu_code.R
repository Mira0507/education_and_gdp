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
indicators <- unique(dt$Indicator_Name)

interests <- c("Female professional and technical workers (% of total)",
               "Age at first marriage, female",
               "Educational attainment, Doctoral or equivalent, population 25+, female (%) (cumulative)",
               "Government expenditure on education, total (% of GDP)")

dt1 <- dt %>%
        filter(Indicator_Name %in% interests) %>%
        select(-X65) %>%
        gather(Year, Value, -c("Country_Name",
                               "Country_Code",
                               "Indicator_Name",
                               "Indicator_Code")) 

dt2 <- dt1[complete.cases(dt1), ]