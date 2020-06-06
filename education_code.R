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

dt4 <- dt3[complete.cases(dt3), ] %>%
        filter(Year == "2016")


# PCA analysis
pca_mtx <- as.matrix(dt4[, 4:6])
rownames(pca_mtx) <- dt4$Country_Name

pca_dt <- prcomp(x = pca_mtx, 
                 scale = TRUE, center = TRUE)



# train/test split 

set.seed(123)
dt4_train <- sample_n(dt4, 
                      size = nrow(dt4) * 0.75)

dt4_test <- dt4 %>%
        anti_join(dt4_train, by = "Country_Code")

dt4_glimpse <- gather(dt4, 
                      GDP_Category,
                      GDP_Value, 
                      -!c("GDP_per_Capita",
                          "Total_GDP"))



# modeling 
fm_single <- as.formula(Percent_Education_Expenditure ~ GDP_per_Capita)
fm_double <- as.formula(Percent_Education_Expenditure ~ GDP_per_Capita + GDP_per_Capita:Total_GDP)

model_single <- lm(fm_single, 
                   data = dt4_train)
model_double <- lm(fm_double,
                   data = dt4_train)





################################ Plotting ####################################


# PCA biplot
biplot_dt <- biplot(pca_dt, main = "PCA Analysis")


glimpse_plot <- 
        ggplot(dt4_glimpse, aes(x = GDP_Value,
                                y = Percent_Education_Expenditure,
                                color = GDP_Category)) + 
        geom_point() + 
        geom_smooth(method = "lm") + 
        scale_x_log10() + 
        theme_bw() + 
        ylab("Government Dxpenditure on Education, Total (% of GDP)") + 
        xlab("GDP (US dollars)") +
        ggtitle("Relationship between Education Expenditure and GDP")




