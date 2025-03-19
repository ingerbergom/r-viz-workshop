########################################
#  S-057 Data Visualization Workshop
#  Spring 2024
#  Author: Liz Salinas
#  Based on the S-057 Excel visualization file
########################################

####################
# SETUP
####################
rm(list=ls()) # clears the workspace

library (dplyr) # library the packages we need
library (ggplot2)

setwd("/Documents/S057") # set the working directory 
dat <- read.csv("s057workshopdata.csv") # load the data

####################
# GENERAL STRUCTURE
####################
# In general, making graphs/figures in R is a matter of becoming familiar with the typical figure syntax as well as how it changes for certain types of figures. There are many useful tools for R data visualization online. 

# When we use ggplot, we can essentially split our figure into the following basic parts: 

###### PLOT = DATA + AESTHETICS + GEOMETRY ######

# DATA refers to the data frame (our data set).

# AESTHETICS indicates the x and y variables. It also tells R how to display data in the plot (e.g., what colors to use, the size and shape of points, etc.).

#GEOMETRY refers to the type of graphs (e.g., bar chart, histogram, box plot, line plot, etc.) to use.

####################
# LINE GRAPHS
####################
# A line graph is really useful when you have data with a time component, or in other words, you are measuring the same things across different times. You can also use this to compare differences across time between different units. If there is a time component (years, months, etc.) try a line graph.

# Example Research Question: How has high school enrollment changed over time at 3 different sites (Cambridge Rindge and Latin, Boston Latin Academy, Brighton High School)?

dat_line <- dat %>% filter(school == 'CAMBRIDGE RINDGE AND LATIN' | school == 'BOSTON LATIN ACADEMY' | school == 'BRIGHTON HIGH') # create a subset of our dataset that includes only the 3 schools from our research question. 

###### PLOT = DATA + AESTHETICS + GEOMETRY ######
ggplot(dat_line, aes(x = year, y = enrollment, color = school)) +
  geom_line() 

# There are a lot of things you can do to change the style of the graph and how these lines look, but note that lines should cover about 2/3 of the chart height. We want to clearly see where each of these lines lie in context of the y-axis. This is something that can be changed. Let's try this with our line chart. Let's change the maximum bound of the y-axis to 3000, and let's see if we prefer it.

ggplot(dat_line, aes(x = year, y = enrollment, color = school)) +
  geom_line() +
  ylim(0, 3000) # set the y-range to cover 0 to 3000

# We can also add a title to our graph, update our legend, and update our axis labels
ggplot(dat_line, aes(x = year, y = enrollment, color = school)) +
  geom_line() +
  ylim(0, 3000) +
  scale_color_discrete(labels = c("Boston Latin", "Brighton", "Cambridge")) + # update legend
  ggtitle("This is our useful takeaway title.") +  # add a title
  labs(color = "School",   # update our legend label
       x = "Year",         # update our x-axis label
       y = "Enrollment")   # update our y-axis label

# NOTE: When you use scale_color_discrete(labels = c()), R will not know how to match your labels to the data; essentially we are just "overriding" the default labels R will use. You will need to verify manually that your labels match the actual data.

####################
# BAR GRAPHS
####################
# A bar graph is really useful when you have data that you want to rank. Or when you have data within groups that you want to show the differences between, such as something like differences in salary between men and women. Finally, you can use a bar graph to show a distribution of a variable - that is, the range and frequency of all possible values. This is called a histogram, and we'll talk about that separately. Basically, if there are categories and you are interested in highlighting the differences between them, try a bar graph.

# Example Research Question: How does racial composition of schools differ between Cambridge and Boston public schools, on average? 

dat_bar <- dat %>%            # create a new dataset called dat_bar
  group_by(district) %>%      # we are going to aggregate to the district level
  summarize(avg_pct_black = mean(black / enrollment, na.rm = TRUE), # get avg_pct by race
            avg_pct_hispanic = mean(hispanic / enrollment, na.rm = TRUE),
            avg_pct_asian = mean(asian / enrollment, na.rm = TRUE),
            avg_pct_white = mean(white / enrollment, na.rm = TRUE))

# We get an error that we have a non-numeric argument. This is likely because R thinks our race counts by school are not numbers. Let's tell R to change the "class" (i.e., type of variable) to be numeric.

dat$hispanic <- as.numeric(dat$hispanic) # make sure the student counts for race are numbers
dat$black <- as.numeric(dat$black)
dat$asian <- as.numeric(dat$asian)
dat$white <- as.numeric(dat$white)
dat$enrollment <- as.numeric(dat$enrollment)

# Now let's try making our data set again. 
dat_bar <- dat %>%            
  group_by(district) %>%     
  summarize(avg_pct_black = mean(black / enrollment, na.rm = TRUE),
            avg_pct_hispanic = mean(hispanic / enrollment, na.rm = TRUE),
            avg_pct_asian = mean(asian / enrollment, na.rm = TRUE),
            avg_pct_white = mean(white / enrollment, na.rm = TRUE))

# To help with our chart, let's swap our rows and coluumns 
library(tidyr)
dat_bar <- dat_bar %>%
  pivot_longer(cols = starts_with("avg"),      # Select columns starting with "avg"
               names_to = "race",              # New column name for racial categories
               values_to = "average_enrollment")  # New column name for average enrollment

###### PLOT = DATA + AESTHETICS + GEOMETRY ######
ggplot(dat_bar, aes(x = race, y = average_enrollment, fill = district)) +
  geom_bar(stat = "identity", position = "dodge")

# Note the stat = "identity" code tells R that the height of each bar is determined by the y-values in the data. The position = "dodge" code tells R that the bars within each group should be placed next to each other instead of stacking them.

# Like before, we can make our plot fancier.
ggplot(dat_bar, aes(x = race, y = average_enrollment, fill = district)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_discrete(labels = c("Boston", "Cambridge")) + #update legend labels
  ggtitle("This is our useful takeaway title.") + # add a title
  labs(fill = "District", #update our legend and axis labels
       x = "",
       y = "Average Percentage")  +
    scale_x_discrete(labels = c("avg_pct_black" = "Black",     #change x-labels
                                "avg_pct_hispanic" = "Hispanic",
                                "avg_pct_asian" = "Asian",
                                "avg_pct_white" = "White")) 

# An important stylistic point with bar graphs is that you always want to start with a baseline (the beginning of your y-axis) at zero. Doing otherwise can lead to people misinterpreting your data. 

ggplot(dat_bar, aes(x = race, y = average_enrollment, fill = district)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_discrete(labels = c("Boston", "Cambridge")) + 
  ggtitle("This is our useful takeaway title.") + 
  labs(fill = "District", 
       x = "",
       y = "Average Percentage")  +
  scale_x_discrete(labels = c("avg_pct_black" = "Black",     
                              "avg_pct_hispanic" = "Hispanic",
                              "avg_pct_asian" = "Asian",
                              "avg_pct_white" = "White")) +
  ylim(0, 0.5) # set range for y
  
####################
# HISTOGRAMS
####################
# Histograms are a special type of bar chart that allows you to visually observe the distribution of a single variable. This is especially useful for a continuous variable that has lots of observations. Say you want to understand the distribution of test scores for students in a school. In that case, a histogram would work really well.

# Example Research Question: How does school size vary in BPS? 

dat_hist <- dat %>% filter(district == 'boston') # 

###### PLOT = DATA + AESTHETICS + GEOMETRY ######
ggplot(dat_hist, aes(x = enrollment)) +
  geom_histogram(binwidth = 100) 

# Note for histograms we don't indicate a y-variable because by definition the y-axis for a histogram will be a tally or a count. 

#Like a bar graph, you want to make sure you start your y-axis at zero. However, a choice you are going to make is how many bins (categories) you want your histogram to have. Make sure you have enough bins to be able to see the distribution, but not too many bins that your graph gets messy. The binwidth tells stata how big to set the "buckets."

#Let's add a title and update the labels.
ggplot(dat_hist, aes(x = enrollment)) +
  geom_histogram(binwidth = 100) +
  ggtitle("This is our useful takeaway title.") + # add a title
  labs(x = "Enrollment", # add axis labels
     y = "Count") 

# One final thing to note with histograms is that when you are comparing the distribution of 2 variables by comparing 2 histograms, make sure they have the same number of bins. Having different bins can lead people to conclude things about the distribution of the variables that may not be present

####################
# SCATTERPLOTS
####################
# Scatterplots are similar to histograms in that they show you the distribution of a variable. However, they can be much more detailed in that they can show you each individual data point on a chart. More importantly, they allow you to add a second variable to the chart, allowing you to see its distribution, which is really useful if you are interested in knowing if 2 variables are related or correlated with each other in some way. A scatterplot also allows you to easily identify outliers in your data.

# Example Research Question: Do white students tend to go to schools where fewer students receive free lunch? 

# Let's practice making a scatterplot by looking at the relationship between white student proportion and the proportion of students who receive free lunch in Boston and Cambridge Public Schools. 

dat_scat <- dat #duplicate our dataset

dat_scat$pct_white <- dat_scat$white / dat_scat$enrollment # make a new variable
dat_scat$pct_freelunch <- dat_scat$freelunch / dat_scat$enrollment

###### PLOT = DATA + AESTHETICS + GEOMETRY ######
ggplot(dat_scat, aes(x = pct_white, y = pct_freelunch)) +
  geom_point()

#Let's add a title and make a few other updates
ggplot(dat_scat, aes(x = pct_white, y = pct_freelunch)) +
geom_point(color = "blue", size = 1) + # make our dots blue and small
  ggtitle("This is our useful takeaway title.") +  #add a title
  labs(x = "X-Axis Label",    #update the axes
       y = "Y-Axis Label")

####################
# EXTRA PRACTICE ON YOUR OWN
####################

# Line Graphs Practice: Create a line graph that shows how black student enrollment as a proportion of total school enrollment has changed between 2013-2017.

# Bar Graph Practice: Create a bar graph that shows the enrollment proportion by race for Cambridge High School and Boston Latin Academy.

# Histogram Practice: Create a histogram that shows the distribution of the proportion of students that are Black in Boston public schools in 2017. 

# Create a scatterplot that shows the relationship between school size and % of students on free lunch. 