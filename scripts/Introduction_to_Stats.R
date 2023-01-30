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

#Create a sequence of 100 equally spaced numbers between -4 and 4
x <-seq(-4, 4, length=100)

#create a vector of values that shows the height of the probability distribution
#for each value in x
y <-dnorm(x)

#plot x and y as a scatterplot with connected lines (type = "l") and add
#an x-axis with custom labels
plot(x,y, type = "l", lwd = 2, axes = FALSE, xlab = "", ylab = "")
axis(1, at = -3:3, labels =c("-3s", "-2s", "-1s", "mean", "1s", "2s", "3s"))

# work out the 95% confidence interval range of our estimated mean
lowerCI <- 2.62-(2*1.22)
upperCI <- 2.62+(2*1.22)

lowerCI
upperCI

