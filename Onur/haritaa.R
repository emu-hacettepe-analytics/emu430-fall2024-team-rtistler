library(sf)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readxl)
library(gganimate)
library(gapminder)
library(magick)
library(animation)
library(gifski)


yol <- "C:/Users/oonur/OneDrive/Belgeler/stats.xls"

aRtist <- read_excel(yol)

turkey_map <- st_read("C:/Users/oonur/Downloads/gadm41_TUR_1.json/gadm41_TUR_1.json")

sadece2008 <- aRtist %>% filter(Year == 2008,!is.na(`Province code`))

# Burada ??ehirlerin Yaz??m Problemini ????zd??m.
head(sadece2008[4])

colnames(sadece2008) <- trimws(gsub("[\n\r\t]", "", colnames(sadece2008)))

sadece2008$`Province name`[sadece2008$`Province name` == " Adana"] <- "\u00a0 Adana"
sadece2008$`Province name`[sadece2008$`Province name` == " Kahramanmara??"] <- "\u00a0 Kahramanmara??"
sadece2008$`Province name`[sadece2008$`Province name` == " Mersin"] <- "\u00a0 Mersin"

turkey_map[3,11] <- "TR-03"
turkey_map[13,11] <- "TR-74"
turkey_map[17,11] <- "TR-12"
turkey_map[22,11] <- "TR-17"
turkey_map[23,11] <- "TR-18"
turkey_map[24,11] <- "TR-19"
turkey_map[27,11] <- "TR-81"
turkey_map[29,11] <- "TR-23"
turkey_map[35,11] <- "TR-29"
turkey_map[38,11] <- "TR-76"
turkey_map[42,11] <- "TR-46"
turkey_map[43,11] <- "TR-78"
turkey_map[49,11] <- "TR-71"
turkey_map[54,11] <- "TR-43"
turkey_map[81,11] <- "TR-67"




sadece2008$`Province code` <- sprintf("TR-%02d",sadece2008$`Province code`)

colnames(sadece2008)[colnames(sadece2008) == "Province code"] <- "ISO_1"

stat2008 <- left_join(turkey_map,sadece2008, by = "ISO_1")

ggplot(data = stat2008) +
  geom_sf(aes(fill = `General Population` / 10^6)) +  # Genel N??fus (Milyon)
  labs(title = "T??rkiye Haritas?? (B??lgelere G??re)", x = "Boylam", y = "Enlem") +
  theme_minimal() +
  scale_fill_gradient(
    name = "General Population",
    low = "#fdb912", 
    high = "#a90432", 
  ) +
  theme(legend.position = "bottom")


ggplot(data = stat2008) +
  geom_sf(aes(fill = `General Illiterate Population` / `General Population` * 100)) +  # Genel N??fus (Milyon)
  labs(title = "T??rkiye Haritas?? (B??lgelere G??re)", x = "Boylam", y = "Enlem") +
  theme_minimal() +
  scale_fill_gradient(
    name = "General Population",
    low = "green", 
    high = "red", 
  ) +
  theme(legend.position = "bottom")









