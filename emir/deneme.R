library(ggplot2)
library(readxl)
library(dplyr)
library(tidyverse)
data <- read_excel("C:/Users/emirm/Desktop/R/stats.xls")
head(data)
data |> ggplot() + geom_point(aes(x = Year, y = `General Population`/10^6), size=1) +
  geom_text(aes(Year,`General Population`/10^6, label = `Province code`),  nudge_x = 0.3)

data <- read_excel("C:/Users/emirm/Desktop/R/stats.xls")
head(data)
data_2008 <- data |> filter(Year == 2008)
data_2008 |> ggplot() + geom_histogram(aes(x = `Male Universities and Other Higher Educational Institutions Graduates`/10^6)) 

data <- read_excel("C:/Users/emirm/Desktop/R/stats.xls")
head(data)
data |> filter(!is.na(`Male Illiterate Population`) & !is.na(Year)) |> ggplot(aes(x = Year, y = `Male Illiterate Population`/10^6), fill = `Province code`) + geom_boxplot()
