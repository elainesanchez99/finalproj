library(tidyverse)
library(skimr)
library(visdat)


#' # Step 1:  Formulate your research question 

#' Are some industries more likely to lack race and/or ethnic representation than others? 



#' # Step 2: Get the Data/read in your data


#'  Read in with tidytuesdayR package 
#'  Install from CRAN via: install.packages("tidytuesdayR")
#' This loads the readme and all the datasets for the week of interest
 
tuesdata <- tidytuesdayR::tt_load('2021-02-23')
tuesdata <- tidytuesdayR::tt_load(2021, week = 9)


employed <- tuesdata$employed


# Or read in the data manually

employed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-23/employed.csv')

earn <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-23/earn.csv') 



#' # Step 3: checking package 


skim(employed)
# rows: 8184
#columns: 7
#character: 4
#numeric: 3

##industry and occupation 
#there are major and minor occupations within each industry 

count(employed, industry) ## 25 unique values
count(employed, major_occupation) ## 5 unique values
count(employed, minor_occupation) ## 12 unique values  

summary(employed$industry)

## race_gender
count(employed, race_gender)

## This data contains 25 industries, but we will filter the industries that have a label of a race or gender
#' for our motivating question 
#industry .960 complete_rate 
#race and gender 1 complete_rate

#' Missing values 
vis_miss(employed)
#97.1% present 
 

## cluster = TRUE uses hierarchical clustering to order the rows 
# will continue to to work with 'employed' data from here 

vis_miss(employed, cluster = TRUE) +
  coord_flip()


#' # Step 4: Look at the top and the bottom of your data 
#employed |> head() |> View()

employed |> tail() |> View()

data ranges from 2015-2020


#' # Step 5: check your Ns


nrow(employed)

min(employed$year, na.rm = TRUE)
max(employed$year, na.rm = TRUE)


#' # Step 6: Make a plot 


employed |>
  mutate(year) |>
  ggplot(aes(year))+
  geom_bar()

##seems count is the same every year 


## we will filter out 'women' 'na' 'white" 'Asian", "Black or African American"
#from industry colmn NOT from race_gender

#gender
#| fig.width: 12
#| fig.height: 8
employed |>
  filter(race_gender %in% c("Women", "Men")) |>
  filter(!(industry %in% c("Women", "NA", "White"))) |>
  ggplot(aes(x = year, y = employ_n, fill = race_gender)) +
  geom_bar(stat = "identity") +
  facet_wrap(vars(industry), scales = 'free_y')

#race
employed |>
  filter(race_gender %in% c("Asian", "Black or African American", 'White')) |>
  filter(!(industry %in% c("Women", "NA", "White", 'Asian', "Black or African American"))) |>
  ggplot(aes(x = year, y = employ_n, fill = race_gender)) +
  geom_bar(stat = "identity") +
  facet_wrap(vars(industry), scales = 'free_y')

#' # Step 7:switching to plots and lines 

#gender
employed %>%
  filter(race_gender %in% c('Women', 'Men')) %>%
  filter(!(industry %in% c("Women", "NA", "White"))) |>
  ggplot(aes(x = year, y = employ_n, color = race_gender, group = interaction(industry, race_gender))) +
  geom_line() +
  geom_point(position = position_dodge(width = 0.2), alpha = 0.5) +
  facet_wrap(~industry, scales = 'free_y') +
  labs(title = "Female and Male Employment in Different Industries Over Time",
       x = "Year",
       y = "Employment",
       color = "Gender") +
  theme_minimal()


#race
employed %>%
  filter(race_gender %in% c('Asian', 'Black or African American', 'White')) %>%
  filter(!(industry %in% c('Women', 'NA', 'White', 'Asian', 'Black or African American'))) |>
  ggplot(aes(x = year, y = employ_n, color = race_gender, group = interaction(industry, race_gender))) +
  geom_line() +
  geom_point(position = position_dodge(width = 0.2), alpha = 0.5) +
  facet_wrap(~industry, scales = 'free_y') +
  labs(title = "Female and Male Employment in Different Industries Over Time",
       x = "Year",
       y = "Employment",
       color = "Gender") +
  theme_minimal()

#' #Step 8: Fitted Regression Curve 

#Gender excluding "Education and health services"
ggplot(employed %>% 
         filter(race_gender %in% c('Men', 'Women') & !(industry %in% c('White', 'Women','NA', 'Education and health services'))), 
       aes(x = year, y = employ_n, color = race_gender)) +
  geom_point() +
  geom_smooth(aes(group = industry), method = "lm", se = FALSE, linetype = "dashed") +
  labs(title = "Fitted Regression Curve by Race Gender and Industry (Excluding 'White' and 'Women')", x = "Year", y = "Employment") +
  theme_minimal() +
  facet_wrap(~industry)

#Gender only in "Education and health services" 
ggplot(employed %>% 
         filter(race_gender %in% c('Men', 'Women') & industry == 'Education and health services'), 
       aes(x = year, y = employ_n, color = race_gender)) +
  geom_point() +
  geom_smooth(aes(group = industry), method = "lm", se = FALSE, linetype = "dashed") +
  labs(title = "Fitted Regression Curve by Race Gender and Industry (Including Only 'Education and health services')", x = "Year", y = "Employment") +
  theme_minimal() +
  facet_wrap(~industry)

#Race
ggplot(employed %>% 
         filter(race_gender %in% c('Asian', 'Black or African American','White') &
                  !(industry %in% c('White', 'Women', 'NA', 'Asian', 'Black or African American'))), 
       aes(x = year, y = employ_n, color = race_gender)) +
  geom_point() +
  geom_smooth(aes(group = industry), method = "lm", se = FALSE, linetype = "dashed") +
  labs(title = "Fitted Regression Curve by Race Gender and Industry (Excluding 'White' and 'Women')", x = "Year", y = "Employment") +
  theme_minimal() +
  facet_wrap(~industry)

#' #Step 9: Scatterplot

#Gender 
ggplot(employed, aes(x = year, y = as.factor(industry), color = race_gender)) +
  geom_point(data = subset(employed, race_gender %in% c('Men', 'Women') & !industry %in% c('Asian', 'White', 'Women')),
             position = position_jitter(width = 0.3), size = 3, alpha = 0.7) +
  labs(title = "Scatter Plot of Count by Year, Industry, and Race/Gender",
       x = "Year",
       y = "Industry Count",
       color = "Race/Gender") +
  theme_minimal() 

#Race
ggplot(employed, aes(x = year, y = as.factor(industry), color = race_gender)) +
  geom_point(data = subset(employed, race_gender %in% c('White', 'Asian', 'Black or African American')
                           & !industry %in% c('Asian', 'White', 'Women')),
             position = position_jitter(width = 0.3), size = 3, alpha = 0.7) +
  labs(title = "Scatter Plot of Count by Year, Industry, and Race/Gender",
       x = "Year",
       y = "Industry Count",
       color = "Race/Gender") +
  theme_minimal() 

#' #Step 10: Try the easy solution first

#Pr(working in education and health services/women) vs Pr(education and health services/men)
#Pr(working in education and health services/ Asian) vs
#Pr(education and health services/Black or African American)

employed %>%
  filter(industry == 'Education and health services') |> 
  count(race_gender) |>  
  mutate(share = n / sum(n)) |>  
  arrange(desc(share)) |> 
  mutate(share = scales::percent(share, accuracy = 1)) 

