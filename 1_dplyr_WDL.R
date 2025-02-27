library(tidyverse)
tidyverse_packages()
##When in doubt, my_data = as.data.frame(my_data)
library(palmerpenguins)
head(penguins)
summary(penguins)
dim(penguins)
glimpse(penguins)

##Can use filter function to filter by a variable
gentoo = filter(penguins, species=="Gentoo")
gentoo_ladies = filter(gentoo, sex=="female")
summary(gentoo_ladies)

#Can also do it in one line
gentoo_ladies = filter(penguins, species=="Gentoo", sex=="female")


##THE PIPE
#CAN SAY
h(g(f(x)))
##OR COULD SAY
x %>% 
  f() %>% 
  g() %>% 
  h()
#Which is easier to read and digest

##THESE TWO LINES ARE THE SAME
gentoo_ladies = filter(penguins, species=="Gentoo", sex=="female")
gentoo_ladies = penguins %>% 
  filter(species=="Gentoo", sex=="female")

#Can use to do one function after another 
female_mean_mass = penguins %>% 
  filter(sex == "female") %>% 
  summarize(mean_mass_g = mean(body_mass_g))
female_mean_mass

##In base R, it would look like this
female_penguins = penguins[which(penguins$sex == "female"), ] 
female_mean_mass = mean(female_penguins$body_mass_g)

#EXERCISE 1.1
#create dataset of only chinstrap penguins
summary(penguins)
chinstrap<-filter(penguins, species=="Chinstrap")
chinstrap_longflipper<-filter(penguins, species=="Chinstrap", flipper_length_mm > 200)
chinstrap
summary(chinstrap)
#34-34 M to F
summary(chinstrap_longflipper)
#17 - 1 M to F

# Calculate mass of each species
species_mean_mass = penguins %>% 
  group_by(species) %>%
  summarize(mean_mass_g = mean(body_mass_g, na.rm=TRUE)) 
species_mean_mass
# Calculate mass of each species by sex 
##Can use group_by for several things at once
species_sex_mean_mass = penguins %>% 
  filter(!is.na(sex)) %>%   # Removes rows where sex is NA. Read the ! as the word "not" here - i.e. it flips the logical value
  group_by(species, sex) %>%
  summarize(mean_mass_g = mean(body_mass_g)) %>%
  print()
## `summarise()` has grouped output by 'species'. You can override using the
## `.groups` argument.


# Save the table
write_csv(x=species_sex_mean_mass, file="data/processed/peguin_mean_body_mass_g.csv")


##EXERCISE 2
##There are lots of different functions you can use in the summarize function
chinstrap<- penguins %>%
  filter(!is.na(sex)) %>%
  group_by(sex,species, flipper_length_mm>200) %>%
  summarize(n())
chinstrap

# Which species has the most observations?
n_by_species = penguins %>%
  group_by(species) %>%
  summarize(n = n()) 

# Use mutate() to convert body mass units:
penguins_for_america = penguins %>%
  mutate(body_mass_lb = body_mass_g * 0.0022) # 0.0022 lb/g
penguins_for_america
# Quickly display the names of all of the islands surveyed:
penguins %>%
  distinct(island)


# Grab just the species and sex columns:
penguins_brief = penguins %>% 
  select(species, sex)
penguins_brief
# Remove bill data:
penguins_no_bill = penguins %>%
  select(-bill_length_mm, -bill_depth_mm)

# Sort data by body mass, then species:
penguins_sorted = penguins %>%
  arrange(body_mass_g, species)

# Sort data by body mass (highest to lowest), then species:
penguins_sorted = penguins %>%
  arrange(desc(body_mass_g), species)

#  summarize(mean_mass_g = mean(body_mass_g, na.rm=TRUE)) 
##EXERCISE 3
summary(penguins)
dream_BL_adelie<- penguins %>%
  filter(island %in% c("Dream", "Biscoe"), species=="Adelie") %>%
  filter(!is.na(bill_length_mm))%>%
  mutate(bill_length_in=(bill_length_mm*0.0393701))%>%
  summarize(mean_bill_length_in=mean(bill_length_in, na.rm=TRUE), 
            sd_bill_length_in=sd(bill_length_in, na.rm=TRUE))
dream_BL_adelie

