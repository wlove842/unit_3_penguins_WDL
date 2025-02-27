library(palmerpenguins)
library(rstatix) # for levene_test() and t_test()
library(knitr)  # prints pretty tables with RMarkdown
library(tidyverse)

#Exploratory Data Analysis
head(penguins)

gentoo = penguins %>%
  filter(species=="Gentoo")
ggplot()+
  geom_histogram(aes(x=body_mass_g), data=gentoo)

#ONE SAMPLE T TEST

#ASSUMPTIONS FOR T TEST
#No significant outliers in the data
#Data should be approx normally dist 

##Assess normallity with Q Q plot
ggplot(gentoo) +
  stat_qq(aes(sample=body_mass_g))
#Want points along the 1:1 line if you normalize the data.
#Rlly just want a straight line
#if not, use wilcoxon signed-rank test

#Literature derived body mass is 5500 g
t.test(gentoo$body_mass_g, mu=5500)


t.test_results = t.test(gentoo$body_mass_g, mu=5500) 
t.test_results$p.value

t_test_results = gentoo %>% t_test(body_mass_g ~ 1, mu = 5500) # pipe-friendly version from rstatix
kable(t_test_results)
t_test_results$p




##INDEPENDENT SAMPLE T TEST
#Assumptions:
  #Independence of observations
  #No significant outliers
  #The two groups should be normally distributed
  #If using students t-test, variances of 2 groups should no be significantly different
    #Use Welch's test to ignore this parameter

data_for_t_test = penguins %>%
  dplyr::filter(species %in% c("Gentoo", "Adelie"),
                !is.na(body_mass_g)) %>%
  select(species, body_mass_g) %>%
  droplevels() # removes "Chinstrap" level from the species factor / categorical variable
summary(data_for_t_test)

data_for_t_test %>%
  group_by(species) %>%
  summarize(mean=mean(body_mass_g), sd=sd(body_mass_g))

ggplot(aes(x=body_mass_g), data=data_for_t_test) +
  geom_histogram() +
  facet_wrap(~species)

ggplot(data_for_t_test) +
  stat_qq(aes(sample=body_mass_g)) +
  facet_wrap(~species, scales="free")

data_for_t_test %>% levene_test(body_mass_g ~ species)
# if p<0.05, variances are NOT equal

##Independent sample t test actually running it
# Base R version:
t.test(data_for_t_test$body_mass_g ~ data_for_t_test$species)
# dplyr-friendly version:
data_for_t_test %>% 
  t_test(body_mass_g ~ species) 
