# PACKAGES ----
library(tidyverse)
library(here)
library(kableExtra)

# DATA ----

darwin <- read_csv(here("data", "darwin.csv"))

# CHECK ----
## check structure
glimpse(darwin)

## check data -tibble 6x3
head(darwin)

## check variable names
colnames(darwin)

## clean up column names
darwin <- janitor::clean_names(darwin)

## check for duplication
darwin %>% 
  duplicated() %>% 
  sum()

## missing values
darwin %>% 
  is.na() %>% 
  sum()

## quick summary
summary(darwin) # pair = min:1, max:15, mean:8
                # type
                # height = min:12, max:18.88, mean:23.5

## check for typos = <0
darwin %>% 
  summarise(min=min(height, na.rm=TRUE), 
            max=max(height, na.rm=TRUE))

## check for typos by looking at distinct characters/values
darwin %>% 
  distinct(pair)

darwin %>% 
  distinct(type) # cross fertilisation, self fertilisation

## quick summary
summary(darwin)

# Visualisation ----
darwin %>%
  ggplot(aes(x=type,
             y=height))+
  geom_boxplot() # look at coursebook for other examples of plots = point, violin

# Mean and Standard Deviations ----
darwin %>%
  group_by(type) %>%
  summarise(mean=mean(height),
            sd=sd(height))  # cross = mean 20.2, sd 3.62
                            # self= mean 17.6, sd 2.05

# make a new object
darwin_summary <-darwin %>%
  group_by(type) %>%
  summarise(mean=mean(height),
            sd=sd(height))

# make a summary plot
darwin_summary %>%
  ggplot(aes(x=type,
             y=mean))+
  geom_pointrange(aes(ymin=mean-sd, ymax=mean+sd))+
  theme_bw()

# use kable extra functions to make a nice table (could be replaced with kable() if needed)
darwin_summary %>%
  kbl(caption="Summary statistics of crossed and selfed maize plants") %>%
  kable_styling(bootstrap_options = "striped", full_width = T, position = "left")

# new column difference = height of the Selfed plant - crossed plant pair ----
## pivot data to wide format then subtract Selfed plant heights from Crossed plant heights
darwin_wide <- darwin %>% 
  pivot_wider(names_from = type, values_from = height) %>% 
  mutate(difference = Cross - Self)

# MEAN, SD, n, SE ----
difference_summary <- darwin_wide %>%
  summarise(mean=mean(difference),
            sd=sd(difference),
            n=n())

difference_summary %>% 
  mutate(se= sd/sqrt(n))
