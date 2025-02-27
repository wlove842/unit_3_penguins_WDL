library(palmerpenguins)
library(tidyverse)
ggplot(data=penguins)+ 
  geom_point(aes(x=flipper_length_mm, y=body_mass_g))

summary(penguins)

penguins %>%
  filter(is.na(body_mass_g)) %>% # Keep only values that are NA
  summarize(n_na = n())  # Count the NAs


ggplot(data=penguins) + 
  geom_point(aes(x=flipper_length_mm, y=body_mass_g, color=species, shape=sex))


ggplot(data=penguins) + 
  geom_point(aes(x=flipper_length_mm, y=body_mass_g, color=species)) +
  geom_smooth(aes(x=flipper_length_mm, y=body_mass_g))


ggplot(data=penguins) + 
  geom_point(aes(x=flipper_length_mm, y=body_mass_g, color=species)) + 
  xlab("Flipper length (mm)") +
  ylab("Body mass (g)") +
  ggtitle("Penguins are cute")


##Exercise 2.1
ggplot(data=penguins)+
  geom_point(aes(x=bill_depth_mm, y=bill_length_mm, color=island))+
  xlab("Bill Depth (mm)")+
  ylab("Bill Length (mm)")+
  ggtitle("Bill Length vs Depth by Island")


##LINE GRAPHS
# Count number of observations per species per year
penguin_ts = penguins %>%
  group_by(species, year) %>%
  summarize(n=n())
## `summarise()` has grouped output by 'species'. You can override using the
## `.groups` argument.

ggplot(data=penguin_ts) +
  geom_line(aes(x=year, y=n, color=species))


##HISTOGRAMS
ggplot(penguins) + 
  geom_histogram(aes(x=flipper_length_mm))
#Adding a filter for just one species and changing the bin number
ggplot(penguins %>% filter(species=="Gentoo")) + 
  geom_histogram(aes(x=flipper_length_mm), binwidth=2)
##Differentiate between species by color
ggplot(data = penguins) +
  geom_histogram(aes(x = flipper_length_mm, fill = species), alpha = 0.5, binwidth=5, position="identity") +
  scale_fill_manual(values = c("darkorange","darkorchid","cyan4"))

##BOXPLOTS AND JITTERED POINTS
ggplot(penguins) + 
  geom_boxplot(aes(y = flipper_length_mm, x = species)) +
  geom_jitter(aes(y = flipper_length_mm, x = species, color = species), width = 0.2) 


##BAR CHARTS
ggplot(data = penguins) +
  geom_bar(aes(x = sex, fill = species)) 
#Separating data into 3 plots by species
ggplot(data = penguins) +
  geom_bar(aes(x = sex, fill = species)) +
  facet_wrap(~species, nrow = 1)  
# Create a new plot for each species, line them all up into 1 row

#Can also switch coordinates
ggplot(penguins) +
  geom_bar(aes(x = island, fill = species)) +
  facet_wrap(~species, ncol = 1) +
  coord_flip()


##SAVING GGPLOTS
ggsave(filename = "figures/penguin_species_per_island.png",  
       device = "png", width = 5, height = 4, units = "in", dpi = 300)


##FINDING COLORS
colors()


##THEMES
ggplot(penguins) + 
  geom_point(aes(x=flipper_length_mm, y=body_mass_g, color=species)) +
  theme_bw()


##EXERCISE 2.2
ggplot(penguins)+
  geom_point(aes(x=bill_depth_mm, y=bill_length_mm, color=sex))+
  facet_wrap(~species, ncol=1, scales="free")+
  theme_minimal()+
  xlab("Bill Depth (mm)")+
  ylab("Bill Length (mm)")
ggsave(filename = "figures/penguin_bill_by_species_sex.png",  
       device = "png", width = 5, height = 4, units = "in", dpi = 300)
