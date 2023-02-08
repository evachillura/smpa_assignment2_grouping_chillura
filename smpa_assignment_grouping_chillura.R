#first we'll load our libraries
library(tidyverse)
library(lubridate)
library(dplyr)

# run this to load the data for this assignment
# it will create a dataframe called "impeach," with all House Democrats and election results/demographics
# it indicates whether or not the member publicly called for the impeachment 
# note: THIS IS FOR THE FIRST TRUMP IMPEACHMENT IN 2020, NOT THE SECOND IN 2021

impeach <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRh8d5JaDqBtByzNw2JZF3idaACobDhhk-p7chJoktA0HawELHFOvjQOqCpzGA4MGONvPlR7GASqW-K/pub?gid=1765341510&single=true&output=csv")

# FOR EACH OF THE QUESTIONS BELOW IN EACH CODE CHUNK, WRITE YOUR WORKING R CODE 
# TO RETURN THE REQUESTED RESULTS

view(impeach)

# 1) Answer this question using grouping.
# How many members in the dataset who are holdouts on impeachment (not favoring it)
# come from districts with a GDP that is above the national figure (column gdp_above_national)? How many below?

impeach %>%
  filter(for_impeachment == "NO", gdp_above_national == "ABOVE") %>% 
  group_by(for_impeachment, gdp_above_national) %>% 
  summarise(n())

# 7 members of the dataset who do not favor impeachment come from districts with
# a GDP that is above the national figure.

# 2) Answer this question using grouping.
# Similar to #1 above, now break down the members of Congress by TWO measures:
# - those who are for or against impeachment
# - districts above/below national GDP

impeach %>% 
  group_by(for_impeachment, gdp_above_national) %>% 
  summarise(n())


# 3) Now do the same as #2, but this time instead of GDP look at above/below 
# the national avg percent of college graduates (pct_bachelors_compared_to_national)

impeach %>% 
  group_by(for_impeachment, pct_bachelors_compared_to_national) %>% 
  summarise(n())


# 4) Instead of showing the above/below national average counts for those for or 
# against impeachment, this time show the combined MEAN percentage of college
# grads (pct_bachelors) for districts that are Yes for impeachment vs. No

impeach %>% 
  group_by(for_impeachment) %>% 
  summarise(grads_for_impeachment = mean(pct_bachelors))


# 5) Do the same as #4, but this time show the combined MEAN percentage of 
# the vote that Donald Trump received for districts that are Yes for 
# impeachment vs. No

impeach %>% 
  group_by(for_impeachment) %>% 
  summarise(trump_voters_for_impeachment = mean(trump_percent))


# 6) Filter out only the members who are a yes for impeachment, 
# then of those how many won their 2018 election by less than
# 5 percentage points (margin_flag_2018) vs. more?

impeach %>% 
  filter(for_impeachment == "YES") %>% 
  group_by(margin_flag_2018) %>% 
  summarise(n())


### For these next questions, we'll use a built-in dataset from ggplot2
# that contains different cars and their fuel efficiency (miles-per-gallon)

#LOAD THE SECOND DATASET WE'LL USE
mpgdata <- ggplot2::mpg

#see what it looks like
mpgdata
view(mpgdata)

# 7) First, let's see how many different car manufacturers we have
#in the mpgdata table. Counting like this can be a helpful way, when you
#have a new dataset, to explore it a bit, find out what's in it.
#So for this question, include your code to count the manufacturers.

mpgdata %>% 
  count(manufacturer) 

# There are 15 different car manufacturers in this dataframe.

# 8) Similarly, let's see how many years this data covers?
# This time, do a count on the "year" column. The results should give you a big
# clue as to what this data can be used to explore.

mpgdata %>% 
  count(year)

# The data only covers two years -- 1999 and 2008 -- so this data can be used to 
# compare and contrast between the years which are 9 years apart by grouping.

# 9) Using group_by() and summarise(), create a code chunk that shows the mean 
# highway mileage for each year (the "hwy" column). 

mpgdata %>% 
  group_by(year) %>% 
  summarise(avg_hwy = mean(hwy))


# 10) Now this time, build on #9 and include the means for both highway
# and city (the "cty" column). Give each column its own name of your choosing 
# as well.

mpgdata %>% 
  group_by(year) %>% 
  summarise(avg_hwy = mean(hwy), avg_cty = mean(cty))


# 11) Determine the mean highway mileage for each manufacturer in the dataset,
# and then sort the results from highest to lowest, so we can see which company
# has the most fuel efficient cars.

mpgdata %>% 
  group_by(manufacturer) %>% 
  summarise(avg_hwy = mean(hwy)) %>% 
  arrange(desc(avg_hwy))

# 12) Do the same as above in #11, but this time filter to limit the year to just 2008.

mpgdata %>% 
  filter(year == 2008) %>% 
  group_by(manufacturer) %>% 
  summarise(avg_hwy = mean(hwy)) %>% 
  arrange(desc(avg_hwy))

# 13) Let's see which manufacturers have the largest engines in 2008. The engine
# size is known as displacement (in the "displ" column). Calculate the mean
# displacement for every manufacturer in 2008, and sort by highest to lowest.

mpgdata %>% 
  filter(year == 2008) %>% 
  group_by(manufacturer) %>% 
  summarise(avg_displ = mean(displ)) %>% 
  arrange(desc(avg_displ))
