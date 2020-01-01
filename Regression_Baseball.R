
library(tidyverse)
library(tidyverse)
library(tidyverse)
library(HistData)
#install.packages("Lahman")
library(Lahman)

set.seed(1989, sample.kind="Rounding") 
options(digits = 3)

Teams %>%
  filter(yearID %in% 1971:2018) %>%
  #mutate(BB = BB,
  #       singles = (H - X2B - X3B - HR),
  #       doubles = X2B,
  #       triples = X3B,
  #       HR = HR )  %>%
   group_by(yearID)  %>%
   do(tidy(lm(R ~ BB + HR, data = .), conf.int = TRUE)) %>%
   ungroup() %>%
   filter(term == 'BB') %>%
   ggplot(aes(yearID, estimate )) + 
   geom_point() +
   geom_smooth(method = 'lm')








# regression with BB, singles, doubles, triples, HR
# fit <- Teams %>% 
#   filter(yearID %in% 1961:2001) %>% 
#   mutate(BB = BB / G, 
#          singles = (H - X2B - X3B - HR) / G, 
#          doubles = X2B / G, 
#          triples = X3B / G, 
#          HR = HR / G,
#          R = R / G) %>%  
#   lm(R ~ BB + singles + doubles + triples + HR, data = .)
# coefs <- tidy(fit, conf.int = TRUE)
# coefs


# Linear regression and stability of coefficients over 3 years
bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)

bat_99_01 <- Batting %>% filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  group_by(playerID) %>%
  summarise(mean_singles = mean(singles), mean_bb = mean(bb)) %>%
  select(playerID, mean_singles, mean_bb)


bat_99_02 <- inner_join(bat_02, bat_99_01)

lm(singles ~ mean_singles, data = bat_99_02)
lm(bb ~ mean_bb , data = bat_99_02)


# bat_99_02 %>% ggplot(aes(singles, mean_singles)) +
#   geom_point()
# 
# bat_99_02 %>% ggplot(aes(bb, mean_bb)) +
#   geom_point()
#
# cor(bat_99_02$singles, bat_99_02$mean_singles)
# cor(bat_99_02$bb, bat_99_02$mean_bb)


# Multivariate linear regression predicting runs per game
# Team_mutated <- Teams %>% filter(yearID %in% 1961:2001) %>%
#   mutate(HR_per_game = HR / G, R_per_game = R / G, BB_per_game = BB / G )
# 
# Team_mutated
# 
# lm(R_per_game ~ BB_per_game + HR_per_game  , data = Team_mutated)
  
