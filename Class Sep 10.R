library(tidyverse)
library(fivethirtyeight)
data("congress_age")

congress_age %>%
  arrange(desc(age)) %>%
  select(lastname, age) #the select command shows specific columns

congress_age %>%
  arrange(age) %>%
  select(lastname, age) 

congress_age %>%
  summarize(mean(age)) #average age

congress_age %>%
  count(party)

congress_age %>%
  filter(party == c("R","D")) %>%
  group_by(congress, party) %>%
  summarize(mean_age = mean(age)) %>% #average age in every congress
  ggplot(aes(congress, mean_age, color = party)) + geom_line()

# ALTERNATIVE, WITHOUT PIPE COMMAND

c <- congress_age %>%
  filter(party == c("R","D")) %>%
  group_by(congress, party) %>%
  summarize(mean_age = mean(age))

ggplot(c, aes(congress, mean_age, color = party)) + geom_line()

# ---------

congress_age %>%
  filter(party %in% c("R","D")) %>% #use %in% instead of == to make the warning message go away
  group_by(congress, party) %>%
  summarize(mean_age = mean(age)) %>% 
  ggplot(aes(congress, mean_age, color = party)) + geom_line() +
  ggtitle("Average Age of Members of Congress", subtitle = "At start of term, 1947 -- 2013")

congress_age %>%
  filter(party %in% c("R","D")) %>% 
  group_by(termstart, party) %>% #using termstart to get year names on x axis
  summarize(mean_age = mean(age)) %>% 
  ggplot(aes(termstart, mean_age, color = party)) + geom_line() +
  ggtitle("Average Age of Members of Congress", subtitle = "At start of term, 1947 -- 2013") + 


  
#PIAZZA CODE 
  congress_age %>%
  mutate(startYear = congress * 2 +1787) %>% 
  filter(party %in% c("D", "R")) %>% 
  group_by(startYear, party) %>% 
  summarize(avgAge = mean(age)) %>% 
  ggplot(aes(x = startYear, y = avgAge, color = party)) + geom_line(size = 2) +
  ggtitle("Average Age of Members of Congress", subtitle = "At start of term, 1947-2013") +
  scale_color_manual(values = c("#4286f4", "#f44141")) +
  scale_x_continuous(breaks = round(seq(min(1950), max(2020), by = 10), 1)) +
  scale_y_continuous(breaks = round(seq(min(40), max(65), by = 5),1))

congress_age %>%
  mutate(startYear = congress * 2 +1787) %>% 
  filter(party %in% c("D", "R")) %>% 
  group_by(startYear, party) %>% 
  summarize(maxAge = max(age)) %>% 
  ggplot(aes(x = startYear, y = maxAge, color = party)) + geom_line(size = 2) +
  ggtitle("Average Age of Members of Congress", subtitle = "At start of term, 1947-2013") +
  scale_color_manual(values = c("#4286f4", "#f44141")) +
  scale_x_continuous(breaks = round(seq(min(1950), max(2020), by = 10), 1)) +
  scale_y_continuous(breaks = round(seq(min(20), max(100), by = 5),1))
