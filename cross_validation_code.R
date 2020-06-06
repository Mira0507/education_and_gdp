library(ggplot2)
library(tidyverse)



g <- glimpse
h <- head  
s <- summary


################################# Data Cleaning #################################

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


dt5 <- dt4


#################################### Modeling ##################################


# fm: formula
fm_single <- as.formula(Percent_Education_Expenditure ~ 
                                log(GDP_per_Capita))
fm_double <- as.formula(Percent_Education_Expenditure ~ 
                                log(GDP_per_Capita) + 
                                log(GDP_per_Capita):log(Total_GDP))

# Cross-validation 
library(vtreat)

set.seed(70)
splitPlan <- kWayCrossValidation(nrow(dt5), 3, NULL, NULL)

dt5$pred_cv_single <- 0 
dt5$pred_cv_double <- 0


for(i in 1:3) {
        split <- splitPlan[[i]]
        model <- lm(fm_single, 
                    data = dt5[split$train,])
        dt5$pred_cv_single[split$app] <- predict(model, 
                                                 newdata = dt5[split$app,])
}

for(i in 1:3) {
        split <- splitPlan[[i]]
        model <- lm(fm_double, 
                    data = dt5[split$train,])
        dt5$pred_cv_double[split$app] <- predict(model, 
                                                 newdata = dt5[split$app,])
}



# Calculating residual 
dt5 <- dt5 %>% 
        mutate(resid_cv_single = pred_cv_single - Percent_Education_Expenditure,
               resid_cv_double = pred_cv_double - Percent_Education_Expenditure)
                               



# evaluation 

RMSE_cv_single <- sqrt(mean(dt5$resid_cv_single^2))
RMSE_cv_double <- sqrt(mean(dt5$resid_cv_double^2))
sd_cv <- sd(dt5$Percent_Education_Expenditure)
r_squared_cv_single <- (cor(dt5$Percent_Education_Expenditure, 
                            dt5$pred_cv_single))^2
r_squared_cv_double <- (cor(dt5$Percent_Education_Expenditure, 
                            dt5$pred_cv_double))^2

# Combining evaluation results 
eval <- data.frame(Model = c("Additive",
                               "Interactive",
                               "Additive",
                               "Interactive"),
                   Training_And_Testing = c("75-25% Spliting",
                                            "75-25% Spliting",
                                            "Cross-Validation",
                                            "Cross-Validation"),
                   RMSE = c(RMSE_single, 
                            RMSE_double,
                            RMSE_cv_single,
                            RMSE_cv_double),
                   R_squared = c(r_squared_single,
                                 r_squared_double,
                                 r_squared_cv_single,
                                 r_squared_cv_double),
                   Standard_Deviation = c(sd, 
                                          sd,
                                          sd_cv,
                                          sd_cv))
                   

#################################### Plotting #################################

outcome_vs_pred_cv_single_plot <- 
        ggplot(dt5, 
               aes(x = pred_cv_single, 
                   y = Percent_Education_Expenditure)) + 
        geom_point(alpha = 0.5,
                   size = 2,
                   color = "#CC0099") + 
        geom_smooth(method = "lm", se = FALSE) + 
        theme_bw() + 
        xlab("Percent Education Expenditure (Predicted)") +
        ylab("Percent Education Expenditure (Actual)") + 
        ggtitle("Outcome vs Prediction: Additive Model")

outcome_vs_pred_cv_double_plot <- 
        ggplot(dt5, 
               aes(x = pred_cv_double, 
                   y = Percent_Education_Expenditure)) + 
        geom_point(alpha = 0.5,
                   size = 2,
                   color = "#660066") + 
        geom_smooth(method = "lm", se = FALSE) + 
        theme_bw() + 
        xlab("Percent Education Expenditure (Predicted)") +
        ylab("Percent Education Expenditure (Actual)") + 
        ggtitle("Outcome vs Prediction: Interactive Model")




# residual plots

residual_cv_single_plot<-
        ggplot(dt5,
               aes(x = pred_cv_single,
                   y = resid_cv_single)) +
        geom_point(alpha = 0.5, 
                   size = 2,
                   color = "#CC0099") + 
        geom_smooth(method = "lm", 
                    se = FALSE) + 
        theme_bw() + 
        xlab("Percent Education Expenditure (Predicted)") + 
        ylab("Residual") + 
        ggtitle("Residual over Predicted Percent Education Expenditure: Additive Model")

residual_cv_double_plot<-
        ggplot(dt5,
               aes(x = pred_cv_double,
                   y = resid_cv_double)) +
        geom_point(alpha = 0.5, 
                   size = 2,
                   color = "#660066") + 
        geom_smooth(method = "lm", 
                    se = FALSE) + 
        theme_bw() + 
        xlab("Percent Education Expenditure (Predicted)") + 
        ylab("Residual") + 
        ggtitle("Residual over Predicted Percent Education Expenditure: Interactive Model")

# gain curves 

library(WVPlots)

gain_curve_cv_single <- GainCurvePlot(dt5,
                                   "pred_cv_single",
                                   "Percent_Education_Expenditure", 
                                   "Gain Curve: Additive Model") + 
        theme_bw() + 
        xlab("Fraction Itens in Sort Order") + 
        ylab("Fraction Total Sum: Percent Education Expenditure")

gain_curve_cv_double <- GainCurvePlot(dt5,
                                   "pred_cv_double",
                                   "Percent_Education_Expenditure", 
                                   "Gain Curve: Interactive Model") + 
        theme_bw() + 
        xlab("Fraction Itens in Sort Order") + 
        ylab("Fraction Total Sum: Percent Education Expenditure")