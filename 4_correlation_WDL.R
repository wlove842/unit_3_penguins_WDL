library(tidyverse)
library(palmerpenguins)
library(rstatix)

gentoo = penguins %>% 
  filter(species=="Gentoo")

# Exploratory data analysis:
glimpse(gentoo)
ggplot(gentoo) +
  stat_qq(aes(sample=bill_depth_mm))
ggplot() +
  geom_point(aes(x=bill_length_mm, y=bill_depth_mm), data=gentoo)
#Want data to be either big or linear QQ
#If nt you can use Kentall tau or Spearman rho 

# cor() returns just the correlation coefficient r
cor(x=gentoo$bill_length_mm, y=gentoo$bill_depth_mm, use="complete.obs")
# cor.test() returns the t-statistic, df, p-value, etc.
cor.test(x=gentoo$bill_length_mm, y=gentoo$bill_depth_mm, use="complete.obs")
# cor_test is the pipe-friendly version from rstatix
gentoo %>%
  cor_test(bill_length_mm, bill_depth_mm)

##Correlation matrix
head(gentoo)
cor(gentoo[,3:6], use="complete.obs")
library(GGally)
gentoo %>%
  select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g) %>%
  ggpairs()
penguins %>%
  select(species, body_mass_g, ends_with("_mm")) %>% # clever way to select variables with names that end in "_mm"
  GGally::ggpairs(aes(color = species))
