library(readxl)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(gganimate)
library(gapminder)
library(magick)
library(animation)
library(gifski)

yol <- "C:/Users/oonur/OneDrive/Belgeler/stats.xls"

aRtist <- read_excel(yol)

aRtist |> 
  filter(!is.na(`Province code`)) |> 
  ggplot(aes(x = `General Population`/10000,
             y = `General Illiterate Population`/1000)) +
  scale_x_log10() +  
  scale_y_log10() +
  geom_point() +
  geom_text(aes(label = `Province code`), vjust = -0.5, hjust = 0.5, size = 3) +
  labs(
    title = "General Population vs. Illiterate Population in {frame_time}",
    x = "General Population in 10K's",
    y = "Illiterate Population in 1K's") +
  transition_time(Year) +
  ease_aes('linear')



combined_all <- aRtist |> 
  filter(!is.na(`Province code`)) |> 
  select(Year, `Province code`, `General Female Population`, `Female Illiterate Population`,
         `General Male Population`, `Male Illiterate Population`) |> 
  mutate(
    Gender = "Female",
    General_Population = `General Female Population` / 10000,
    Illiterate_Population = `Female Illiterate Population` / 1000
  ) |> 
  select(Year, `Province code`, Gender, General_Population, Illiterate_Population) |> 
  bind_rows(
    aRtist |> 
      filter(!is.na(`Province code`)) |> 
      select(Year, `Province code`, `General Male Population`, `Male Illiterate Population`) |> 
      mutate(
        Gender = "Male",
        General_Population = `General Male Population` / 10000,
        Illiterate_Population = `Male Illiterate Population` / 1000
      ) |> 
      select(Year, `Province code`, Gender, General_Population, Illiterate_Population)
  )

ggplot(combined_all, aes(x = General_Population, y = Illiterate_Population, color = Gender)) +
  scale_x_log10() +
  scale_y_log10() +
  geom_point(size = 3) +
  geom_text(aes(label = `Province code`), vjust = -0.5, hjust = 0.5, size = 3, show.legend = FALSE) +
  scale_color_manual(values = c("Female" = "tomato", "Male" = "blue")) +
  labs(
    title = "General Population vs. Illiterate Population in {frame_time}",
    x = "General Population (10K's)",
    y = "Illiterate Population (1K's)",
    color = "Gender"
  ) +
  transition_time(Year) +  # Year ??zerinden animasyon
  ease_aes('linear') +     # Ak???? d??zeni lineer
  theme_minimal()

anim_save("abo",path = "C:/Users/oonur/OneDrive/Belgeler/GitHub/emu430-fall2024-team-rtistler/Onur")

#------------------------------------------------------2023
{
  #---General
  
  aRtist |> 
    filter(Year == 2023, !is.na(`Province code`)) |> 
    ggplot(aes(x = `General Population`/10000, y = `General Illiterate Population`/1000)) +
    scale_x_log10() +  
    scale_y_log10() +
    geom_point() +
    geom_text(aes(label = `Province code`), vjust = -0.5, hjust = 0.5, size = 3) +
    labs(
      title = "General Population vs. Illiterate Population in 2023",
      x = "General Population in 10K's",
      y = "Illiterate Population in 1K's")
  #---Female
  aRtist |> 
    filter(Year == 2023, !is.na(`Province code`)) |> 
    ggplot(aes(x = `General Female Population`/10000, y = `Female Illiterate Population`/1000)) +
    scale_x_log10() +  
    scale_y_log10() +
    geom_point() +
    geom_text(aes(label = `Province code`), vjust = -0.5, hjust = 0.5, size = 3) +
    labs(
      title = "General Population vs. Illiterate Population in 2023 - FEMALE",
      x = "General Population in 10K's",
      y = "Illiterate Population in 1K's")
  #---Male
  aRtist |> 
    filter(Year == 2023,!is.na(`Province code`)) |> 
    ggplot(aes(x = `General Male Population`/10000, y = `Male Illiterate Population`/1000)) +
    scale_x_log10() +  
    scale_y_log10() +
    geom_point() +
    geom_text(aes(label = `Province code`), vjust = -0.5, hjust = 0.5, size = 3) +
    labs(
      title = "General Population vs. Illiterate Population in 2023 - MALE",
      x = "General Population in 10K's",
      y = "Illiterate Population in 1K's")
  #---Combined
  combined_2023 <- aRtist |> 
    filter(Year == 2023, !is.na(`Province code`)) |> 
    select(`Province code`, `General Female Population`, `Female Illiterate Population`,
           `General Male Population`, `Male Illiterate Population`) |> 
    mutate(
      Gender = "Female",
      General_Population = `General Female Population` / 10000,
      Illiterate_Population = `Female Illiterate Population` / 1000
    ) |> 
    select(`Province code`, Gender, General_Population, Illiterate_Population) |> 
    bind_rows(
      aRtist |> 
        filter(Year == 2023, !is.na(`Province code`)) |> 
        select(`Province code`, `General Male Population`, `Male Illiterate Population`) |> 
        mutate(
          Gender = "Male",
          General_Population = `General Male Population` / 10000,
          Illiterate_Population = `Male Illiterate Population` / 1000
        ) |> 
        select(`Province code`, Gender, General_Population, Illiterate_Population)
    )
  
  ggplot(combined_2023, aes(x = General_Population, y = Illiterate_Population, color = Gender)) +
    scale_x_log10() +
    scale_y_log10() +
    geom_point() +
    geom_text(aes(label = `Province code`), vjust = -0.5, hjust = 0.5, size = 3, show.legend = FALSE) +
    scale_color_manual(values = c("Female" = "tomato", "Male" = "blue")) +
    labs(
      title = "General Population vs. Illiterate Population in 2023",
      x = "General Population (10K's)",
      y = "Illiterate Population (1K's)",
      color = "Gender"
    ) +
    theme_minimal()
}


                  

