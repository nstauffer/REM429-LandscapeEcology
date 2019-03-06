library(tidyverse)

data.working <- dplyr::mutate(data.wide, dbh.delta = DBH13 - DBH99)
write.csv(data.working,"C:/Users/Nelson/Documents/Projects/GitHub/REM429-LandscapeEcology/labs/11_patterns/data/worthwhile.csv")

ggplot() +
  geom_histogram(data = dplyr::filter(data.working, source == "Caffey Hill"),
                 aes(y=..count../sum(..count..),
                     x = dbh.delta,
                     fill = source),
                 alpha = 0.5) +
  geom_histogram(data = dplyr::filter(data.working, source == "Red-Tail Ridge"),
                 aes(y=..count../sum(..count..),
                     x = dbh.delta,
                     fill = source),
                 alpha = 0.5) +
  scale_fill_discrete() +
  labs(y = "Proportion of individuals within sampling area",
       x = "Change in DBH (cm)",
       fill = "Sampling Area") +
  theme(panel.background = element_blank())


ggplot() +
  geom_histogram(data = dplyr::mutate(dplyr::filter(data.tall, Year == 1999), yearstring = as.character(Year)),
                 aes(y=..count../sum(..count..),
                     x = DBH,
                     fill = yearstring),
                 alpha = 0.5) +
  geom_histogram(data = dplyr::mutate(dplyr::filter(data.tall, Year == 2013), yearstring = as.character(Year)),
                 aes(y=..count../sum(..count..),
                     x = DBH,
                     fill = yearstring),
                 alpha = 0.5) +
  scale_fill_discrete() +
  labs(y = "Proportion of individuals within sampling area",
       x = "DBH (cm)",
       fill = "Sampling Year") +
  theme(panel.background = element_blank()) +
  facet_grid(~ source)
