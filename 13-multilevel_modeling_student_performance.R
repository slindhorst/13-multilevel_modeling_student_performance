# Install new packages, fastDummies installed because dummies is obsolete
install.packages(c("lme4", "performance", "fastDummies"))

# Add libraries
library(tidyverse)
library(fastDummies)
library(sjPlot)
library(lme4)
library(performance)
library(dataedu)

str(iris)
glimpse(iris)
head(iris)
levels(iris$Species)

# Example test before looking at real data
# New command in fastDummies dummy_cols() automatically detects and binds new columns to df
d_iris <- 
  dummy_cols(iris)

d_iris %>% 
  select(starts_with("Species")) %>% 
  head()

species_dummy_coded <- 
  d_iris %>% 
  select(starts_with("Species"))

# Just one step! dummy_cols adds the new columns to the existing iris data
iris_with_dummy_codes <- dummy_cols(iris, select_columns = "Species")

# Now you can count immediately
iris_with_dummy_codes %>% 
  count(Species, Species_setosa, Species_versicolor, Species_virginica)

# Now let's import the real data!
dat <- dataedu::sci_mo_processed

dat %>% 
  count(course_id)

# Rename final grade variable to make it easier to type
dat <- 
  dat %>% 
  rename(final_grade = FinalGradeCEMS)

# it automatically selects the refences as the first alphabetical course on in the column
m_linear_dc <- 
  lm(final_grade ~ TimeSpent_std + course_id, data = dat)

tab_model(m_linear_dc,
          title = "Table 13.1")

# Specify the exact course I want to use as my linear model reference by turning the variable course_ID into a factor
dat <- 
  dat %>% 
  mutate(course_id = fct_relevel(course_id, "PhysA-S116-01"))
str(dat)

m_linear_dc_1 <- 
  lm(final_grade ~ TimeSpent_std + course_id, data = dat)

tab_model(m_linear_dc_1,
          title = "Table 13.2")

# specifying the same linear model as the previous example, but using a "-1" to indicate that there should not be a reference group
m_linear_dc_2 <- 
  lm(final_grade ~ -1 + TimeSpent_std + course_id, data = dat)

tab_model(m_linear_dc_2,
          title = "Table 13.3")

# Multi-level model based on course_id group and (1) varying intercepts for each group
m_course <- 
  lmer(final_grade ~ TimeSpent_std + (1|course_id), data = dat)

tab_model(m_course,
          title = "Table 13.4")

install.packages("performance")
library(performance)
icc(m_course)

# this model would specify a group effect for both the course and school
# we don't actually have school_id variable, so shouldn't run this code
m_course_school <- 
  lmer(final_grade ~ TimeSpent + (1|course_id) + (1|school_id), data = dat)
