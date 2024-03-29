#set the working directory
getwd()
setwd("G:/Fall 2020/George Mason University/STAT -515")
getwd()

library(tidyverse)

#Read in the CSV file 

NEpop <- read.csv('all-countries-data-ined-institut-national-d-etudes-demographiques.csv', header = TRUE, na.strings = c("", "NA"))
View(NEpop)
#Delete the NA rows from the data frame
library(dplyr)
NEpop <- na.omit(NEpop)
#Change the columns names
colnames(NEpop) <- c("Countries","Total_population_thousands","Birth_rate","Mortality_rate",
                     "Life_expectancy","Infant_mortality_rate","children_per_woman",
                     "Growth_rate","Population_aged_65_and_more_thousands")
colnames(NEpop)
#Drop the 2nd row
NEpop <- NEpop[-1,]
# Drop the last row as it gives the statistics for the entire region
NEpop <- NEpop[-12,]
View(NEpop)
str(NEpop)
NEpop$Total_population_thousands = as.numeric(NEpop$Total_population_thousands)
NEpop$Birth_rate = as.numeric(NEpop$Birth_rate)
NEpop$Mortality_rate = as.numeric(NEpop$Mortality_rate)
NEpop$Life_expectancy = as.numeric(NEpop$Life_expectancy)
NEpop$Infant_mortality_rate = as.numeric(NEpop$Infant_mortality_rate)
NEpop$children_per_woman = as.numeric(NEpop$children_per_woman)
NEpop$Growth_rate = as.numeric(NEpop$Growth_rate)
NEpop$Population_aged_65_and_more_thousands= as.numeric(NEpop$Population_aged_65_and_more_thousands)

str(NEpop)
View(NEpop)
#Plot the map using plotly
g <- list(
  scope = 'europe')


NE_pop <- NEpop
LAND_ISO <- c('','DNK','EST','FIN','ISL','IRL','LVA','LTU','NOR','SWE','GBR')
NE_pop$LAND_ISO <- LAND_ISO
View(NE_pop)

#Total population in 2021
#install.packages("plotly")
library(plotly)
plot_geo(NE_pop) %>%
  add_trace(
    z = ~Total_population_thousands,locations = ~LAND_ISO,
    color = ~Total_population_thousands, colors = 'Purples'
  ) %>%
  colorbar(title = "Population in thousands") %>%
  layout(geo = g, title = "Total Population(in thousands) in Northern Europe,2021"
  )
#Birth Rate per 1000 pop.
plot_geo(NE_pop) %>%
  add_trace(
    z = ~Birth_rate,locations = ~LAND_ISO,
    color = ~Birth_rate, colors = 'Purples'
  ) %>%
  colorbar(title = "Birth Rate per 1000 pop.") %>%
  layout(geo = g, title = "Birth Rate in Northern Europe,2021"
  )
#Mortality Rate
plot_geo(NE_pop) %>%
  add_trace(
    z = ~Mortality_rate,locations = ~LAND_ISO,
    color = ~Mortality_rate, colors = 'Purples'
  ) %>%
  colorbar(title = "Mortality rate per 1000 pop.") %>%
  layout(geo = g, title = "Mortality Rate in Northern Europe,2021"
  )
#Life expectancy
plot_geo(NE_pop) %>%
  add_trace(
    z = ~Life_expectancy,locations = ~LAND_ISO,
    color = ~Life_expectancy, colors = 'Purples'
  ) %>%
  colorbar(title = "Life_expectancy in years.") %>%
  layout(geo = g, title = "Life Expectancy in Northern Europe,2021"
  )
#Infant Mortality Rate
plot_geo(NE_pop) %>%
  add_trace(
    z = ~Infant_mortality_rate,locations = ~LAND_ISO,
    color = ~Infant_mortality_rate, colors = 'Purples'
  ) %>%
  colorbar(title = "Infant Mortality per 1000 births.") %>%
  layout(geo = g, title = "Infant Mortality in Northern Europe,2021"
  )
#Children Per Woman
plot_geo(NE_pop) %>%
  add_trace(
    z = ~children_per_woman,locations = ~LAND_ISO,
    color = ~children_per_woman, colors = 'Purples'
  ) %>%
  colorbar(title = "Children per woman") %>%
  layout(geo = g, title = "Children per Woman in Northern Europe,2021"
  )
#Growth Rate
plot_ly(
  x = NEpop$Countries,
  y = NEpop$Growth_rate,
  name = "Growth Rate",
  type = "bar"
)
#Population Aged 65 or more
plot_geo(NE_pop) %>%
  add_trace(
    z = ~Population_aged_65_and_more_thousands, locations = ~LAND_ISO,
    color = ~Population_aged_65_and_more_thousands, colors = 'Purples'
  ) %>%
  colorbar(title = "Population(in thousands) aged 65 or more") %>%
  layout(geo = g, title = "Population of People Aged 65 or more in Northern Europe,2021"
  )

#Uni variate Analysis
ggplot(NE_pop,aes(x = Birth_rate ,y = Total_population_thousands)) +
  geom_point(shape = 21,fill = "red",
             color = "black",size = 2) +
  stat_smooth(method = lm,
              color = "blue",fill = "cyan") +
  labs(
    x = "Birth Rate",
    y = "Total Population Estimates,2021",
    title = "Population Estimates based on Birth Rate")


ggplot(NE_pop,aes(x = Mortality_rate,y = Total_population_thousands)) +
  geom_point(shape = 21,fill = "red",
             color = "black",size = 2) +
  stat_smooth(method = lm,
              color = "blue",fill = "cyan") +
  labs(
    x = "Mortality Rate",
    y = "Total Population Estimates,2021",
    title = "Population Estimates based on Mortality Rate")

ggplot(NE_pop,aes(x = Life_expectancy,y = Total_population_thousands)) +
  geom_point(shape = 21,fill = "red",
             color = "black",size = 2) +
  stat_smooth(method = lm,
              color = "blue",fill = "cyan") +
  labs(
    x = "Life Expectancy",
    y = "Total Population Estimates,2021",
    title = "Population Estimates based on Life Expectancy")

ggplot(NE_pop,aes(x = Infant_mortality_rate,y = Total_population_thousands)) +
  geom_point(shape = 21,fill = "red",
             color = "black",size = 2) +
  stat_smooth(method = lm,
              color = "blue",fill = "cyan") +
  labs(
    x = "Infant Mortality Rate",
    y = "Total Population Estimates,2021",
    title = "Population Estimates based on Infant Mortality Rate")

ggplot(NE_pop,aes(x = children_per_woman,y = Total_population_thousands)) +
  geom_point(shape = 21,fill = "red",
             color = "black",size = 2) +
  stat_smooth(method = lm,
              color = "blue",fill = "cyan") +
  labs(
    x = "Children per woman",
    y = "Total Population Estimates,2021",
    title = "Population Estimates based on Children per Woman")

ggplot(NE_pop,aes(x = Growth_rate,y = Total_population_thousands)) +
  geom_point(shape = 21,fill = "red",
             color = "black",size = 2) +
  stat_smooth(method = lm,
              color = "blue",fill = "cyan") +
  labs(
    x = "Growth Rate",
    y = "Total Population Estimates,2021",
    title = "Population Estimates based on Growth Rate")

ggplot(NE_pop,aes(x = Population_aged_65_and_more_thousands,y = Total_population_thousands)) +
  geom_point(shape = 21,fill = "red",
             color = "black",size = 2) +
  stat_smooth(method = lm,
              color = "blue",fill = "cyan") +
  labs(
    x = "Number of People over the age of 65",
    y = "Total Population Estimates,2021",
    title = "Population Estimates based on Number of people over the age of 65")


#Multiple Linear Regression
#To find correlation
NE_pop_LM <- select(NE_pop, c("Total_population_thousands","Birth_rate","Mortality_rate",
                              "Life_expectancy","Infant_mortality_rate","children_per_woman",
                              "Growth_rate","Population_aged_65_and_more_thousands"))
View(NE_pop_LM)
str(NE_pop_LM)

pairs(NE_pop_LM)
#Linear regression model of all variables on the response variable of total population
tot_pop.lm <- lm(Total_population_thousands ~ ., data = NE_pop_LM)
summary(tot_pop.lm)
summary(tot_pop.lm)$coefficients
plot(tot_pop.lm,las = 1)


model1 <- lm(Total_population_thousands ~ Birth_rate + Infant_mortality_rate + Growth_rate + Population_aged_65_and_more_thousands, 
             data = NE_pop_LM)
summary(model1)
summary(model1)$coefficients
plot(model1,las = 1)

model2 <- lm(Total_population_thousands ~ Mortality_rate + Life_expectancy + Population_aged_65_and_more_thousands,
             data = NE_pop_LM)
summary(model2)
summary(model2)$coefficients
plot(model2,las = 1)

model3 <- lm(Total_population_thousands ~ log(Birth_rate) + log(Infant_mortality_rate) + Growth_rate + Population_aged_65_and_more_thousands, data = NE_pop_LM)
summary(model3)
#Confidence interval
confint(model1)
confint(model2)
#Model Accuracy
#Calculate error rate by dividing RSE by mean of the outcome variable
sigma(model1)/mean(NE_pop_LM$Total_population_thousands)
sigma(tot_pop.lm)/mean(NE_pop_LM$Total_population_thousands)
sigma(model2)/mean(NE_pop_LM$Total_population_thousands)

plot(model3,las = 1)

model4 <- lm(Total_population_thousands ~ poly(Birth_rate,2)+poly(Mortality_rate,2)+
               log(Infant_mortality_rate)+poly(Growth_rate,3)+Population_aged_65_and_more_thousands,
             data = NE_pop_LM)
summary(model4)
plot(model4,las = 1)

model5 <- lm(Total_population_thousands ~ poly(Life_expectancy,2)+poly(Mortality_rate,2)+
               log(Infant_mortality_rate)+poly(Growth_rate,3)+Population_aged_65_and_more_thousands,
             data = NE_pop_LM)
summary(model5)
plot(model5,las = 1)

model6 <- lm(Total_population_thousands ~ Life_expectancy, data = NE_pop_LM)
summary(model6)
plot(model6,las = 1)


