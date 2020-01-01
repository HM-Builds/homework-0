set.seed(1989, sample.kind="Rounding") 
options(digits = 3)


#install.packages("HistData") 

library(tidyverse)
library(HistData)
library(broom)


data("GaltonFamilies")

# Parent to child relationships rather than mother/daughter and father/son
set.seed(1, sample.kind = "Rounding") # if you are using R 3.6 or later
galton <- GaltonFamilies %>%
  group_by(family, gender) %>%
  sample_n(1) %>%
  ungroup() %>% 
  gather(parent, parentHeight, father:mother) %>%
  mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
  unite(pair, c("parent", "child"))

galton

# sum(galton$pair == 'father_daughter')
# sum(galton$pair == 'mother_son')

# the following 3 lines can replace the lines between '<START>' and '<FINISH>'
galton %>% 
  group_by(pair) %>%
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE)) %>%
  filter(term == 'parentHeight') %>%
  ggplot((aes(pair, y = estimate, ymin = conf.low, ymax = conf.high))) + 
  geom_errorbar() + 
  geom_point()

galton %>% 
  group_by(pair) %>%
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE)) %>%
  filter(term == 'parentHeight') %>%
  conf.low - conf.high


# # <START> 
# father_daughter_height <- galton %>% 
#   filter(pair %in% 'father_daughter') %>%
#   select(parentHeight, childHeight)
# 
# father_son_height <- galton %>%
#   filter(pair %in% 'father_son') %>%
#   select(parentHeight, childHeight)
# 
# mother_daughter_height <- galton %>%
#   filter(pair %in% 'mother_daughter') %>%
#   select(parentHeight, childHeight)
# 
# mother_son_height <- galton %>%
#   filter(pair %in% 'mother_son') %>%
#   select(parentHeight, childHeight)
# 
# cor(father_daughter_height$parentHeight, father_daughter_height$childHeight)
# cor(father_son_height$parentHeight, father_son_height$childHeight)
# cor(mother_daughter_height$parentHeight, mother_daughter_height$childHeight)
# cor(mother_son_height$parentHeight, mother_son_height$childHeight)
# # <FINISH> 

  
# Male heights with regression residuals plotted 
# galton_heights <- GaltonFamilies %>%
#   filter(gender == "male") %>%
#   group_by(family) %>%
#   sample_n(1) %>%
#   ungroup() %>%
#   select(father, childHeight) %>%
#   rename(son = childHeight)
# 
# rss <- function(beta0, beta1, data){
#   resid <- galton_heights$son - (beta0+beta1*galton_heights$father)
#   return(sum(resid^2))
# }
# 
# 
# beta1 = seq(0, 1, len=nrow(galton_heights))
# results <- data.frame(beta1 = beta1,
#                       rss = sapply(beta1, rss, beta0 = 36))
# results %>% ggplot(aes(beta1, rss)) + geom_line() + 
#   geom_line(aes(beta1, rss))

# Female heights
# # Remember to run "set.seed" everytime to get the same answer
# set.seed(1989, sample.kind="Rounding") 
# female_heights <- GaltonFamilies%>%
#   filter(gender == "female") %>%
#   group_by(family) %>%
#   sample_n(1) %>%
#   ungroup() %>%
#   select(mother, childHeight) %>%
#   rename(daughter = childHeight)
# 
# linear_regression_model <- lm(mother ~ daughter, data = female_heights)
# first_mother_height <- linear_regression_model$fitted.values[1]


# Conditional average of female heights
# conditional_avg <- female_heights %>%
#   filter(round(mother) == 60) %>%
#   summarize(avg = mean(daughter)) %>%
#   pull(avg)
# 
# conditional_avg


