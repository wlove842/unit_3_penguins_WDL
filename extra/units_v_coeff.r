library(palmerpenguins)
library(tidyverse)

### multiple regression with 2 or 3 continuous variables
gentoo = penguins %>%
  filter(species == "Gentoo") %>%
  mutate(bill_depth_um = bill_depth_mm * 1000,
         bill_length_um = bill_length_mm * 1000)
summary(gentoo)

# bill depth and bill length both in mm
lm_gentoo_mm = lm(bill_depth_mm ~ bill_length_mm, data=gentoo)
summary(lm_gentoo_mm)

# bill depth in mm and bill length in um
lm_gentoo_um = lm(bill_depth_mm ~ bill_length_um, data=gentoo)
summary(lm_gentoo_um)

# bill depth in um and bill length in mm
lm_gentoo_um2 = lm(bill_depth_um ~ bill_length_mm, data=gentoo)
summary(lm_gentoo_um2)

# bill depth and bill length both in um
lm_gentoo_um3 = lm(bill_depth_um ~ bill_length_um, data=gentoo)
summary(lm_gentoo_um3)

# The coefficient in front of bill length is the same (0.204) as long as 
# both bill depth and bill length are in the same units (both mm or both um)
# but the coefficient increases to 204 if just bill depth (y) is in um
# and the coefficient decreases to 0.000204 if just bill length (x) in in um

# Within a single model, if (and only if) all x's are standardized, the coeff 
# can be used to compare the relative effect between the x variables
# A disadvantage of standardizing your variables is reducing interpretability
# Another option: calculate and report change in predicted y across one standard 
# deviation in your x

# Beware of over-interpreting effect size. For example:
# Perhaps we have another variable "toxicity" and if a penguin is exposed to toxins their
# beak grows weak and skinny The effect size for toxicity is high bc of its strong impact on bill depth. 
# But if exposure to toxins is rare, the toxcity variable may not actually be contributing much to your 
# prediction of bill depth
# Also beware of interpreting effect size when there is any collinearity in your x's! 

# Either way, whether or not you standardize your variables, you'll need to make space for some 
# thoughtful conversation on the meaning of your model results rather than rely on a statistical
# test or metric
