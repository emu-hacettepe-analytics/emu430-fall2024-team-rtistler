library(readxl)
library(dplyr)
library(ggplot2)



yol <- "C:/Users/oonur/OneDrive/Belgeler/stats.xls"

aRtist <- read_excel(yol)

#-----------------------------2023

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
  scale_color_manual(values = c("Female" = "pink", "Male" = "blue")) +
  labs(
    title = "General Population vs. Illiterate Population in 2023",
    x = "General Population (10K's)",
    y = "Illiterate Population (1K's)",
    color = "Gender"
  ) +
  theme_minimal()
