library(readxl)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(gganimate)
library(gapminder)
library(magick)
library(animation)
library(gifski)
library(sf)
library(ggfortify)
library(hrbrthemes)


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
  ease_aes('linear')+ 
  facet_wrap(~Region)



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

ggplot(aRtist |> filter(!is.na(`Province code`)), aes(x = factor(Year), y = `General Illiterate Population` / `General Population` * 100)) + 
  geom_boxplot() +
  labs(
    x = "Year",
    y = "Illiteracy Rate (%)",
    title = "Illiteracy Rate by Year"
  ) +
  geom_boxplot(data = aRtist |> filter(is.na(`Province code`)),col = "red") +
  theme_minimal()

ggplot(aRtist |> filter(is.na(`Province code`)), aes(x = factor(Year), y = `General Illiterate Population` / `General Population` * 100)) + 
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +  # ??ubuk grafik
  labs(
    x = "Year",
    y = "Illiteracy Rate (%)",
    title = "Illiteracy Rate by Year"
  ) +
  theme_minimal()


illiterates_combined <- aRtist |> 
  filter(!is.na(`Province code`)) |> 
  select(Year, `Province code`, Region, `General Female Population`, `Female Illiterate Population`,
         `General Male Population`, `Male Illiterate Population`) |> 
  mutate(
    Gender = "Female",
    General_Population = `General Female Population`,
    Illiterate_Population = `Female Illiterate Population`
  ) |> 
  select(Year, `Province code`, Region, Gender, General_Population, Illiterate_Population) |> 
  bind_rows(
    aRtist |> 
      filter(!is.na(`Province code`)) |> 
      select(Year, `Province code`, Region, `General Male Population`, `Male Illiterate Population`) |> 
      mutate(
        Gender = "Male",
        General_Population = `General Male Population`,
        Illiterate_Population = `Male Illiterate Population`
      ) |> 
      select(Year, `Province code`, Region, Gender, General_Population, Illiterate_Population)
  )


ggplot(
  illiterates_combined |> filter(!is.na(`Province code`)),
  aes(
    x = factor(Year),
    y = Illiterate_Population / General_Population * 100,
    fill = Gender
  )
) + 
  geom_boxplot() +
  labs(
    x = "Year",
    y = "Illiteracy Rate (%)",
    title = "Illiteracy Rate by Year and Gender",
    fill = "Gender"
  ) +
  scale_fill_manual(values = c("Female" = "pink", "Male" = "lightblue")) + 
  theme_minimal() 

ggplot(
  illiterates_combined |> filter(!is.na(`Province code`)),
  aes(
    x = factor(Year),
    y = Illiterate_Population / General_Population * 100,
    fill = Gender
  )
) + 
  geom_boxplot() +
  labs(
    x = "Year",
    y = "Illiteracy Rate (%)",
    title = "Illiteracy Rate by Year, Gender, and Region",
    fill = "Gender"
  ) +
  scale_fill_manual(values = c("Female" = "pink", "Male" = "lightblue")) +
  theme_minimal() +
  facet_wrap(~ Region)

ggplot(illiterates_combined |> filter(!is.na(`Province code`)), aes(x = factor(Year), y = Illiterate_Population / General_Population * 100)) + 
  geom_bar(aes(fill = Gender), stat = "identity", position = "dodge") +  # ??ubuk grafik
  labs(
    x = "Year",
    y = "Illiteracy Rate (%)",
    title = "Illiteracy Rate by Year"
  ) +
  scale_fill_manual(values = c("Male" = "#fdb912", "Female" = "#a90432")) +  # Renkleri manuel ayarlay??n
  theme_minimal()


ggplot(aRtist |> filter(is.na(`Province code`)), aes(x = factor(Year), y = `General Illiterate Population` / `General Population` * 100)) + 
  geom_point() +  # Noktalar?? ??iz
  geom_line(aes(group = 1)) +  # Noktalar?? birbirine ba??la
  labs(
    x = "Year",
    y = "Illiteracy Rate (%)",
    title = "Illiteracy Rate by Year"
  ) +
  theme_minimal()

educ_level <- aRtist %>%
  group_by(Year) %>%
  summarize(
    illiteracy_s = sum(`General Illiterate Population`, na.rm = TRUE) / sum(`General Population`, na.rm = TRUE) * 100,
    primary_s = sum(`General Primary School Graduates`, na.rm = TRUE) / sum(`General Population`, na.rm = TRUE) * 100,
    universitie_s = sum(`General Universities and Other Higher Educational Institutions Graduates`, na.rm = TRUE) / sum(`General Population`, na.rm = TRUE) * 100,
    masteranddoctorate_s = sum(`General Doctorate Graduates` + `General Master Graduates`, na.rm = TRUE) / sum(`General Population`, na.rm = TRUE) * 100,
    unknown_s = sum(`General Unknown`, na.rm = TRUE) / sum(`General Population`, na.rm = TRUE) * 100
  )
educ_level_long <- educ_level %>%
  pivot_longer(cols = c("illiteracy_s", "primary_s", "universitie_s", "masteranddoctorate_s", "unknown_s"),
               names_to = "Variable",
               values_to = "Value")
ggplot(educ_level_long, aes(x = factor(Year), y = Value, color = Variable, group = Variable)) + 
  geom_line() +  # Zaman i??indeki de??i??imi ??izgilerle g??ster
  geom_point() +  # Noktalarla y??ll??k veriyi g??ster
  labs(
    x = "Year",
    y = "Percentage (%)",
    title = "Educational Levels Over Time"
  ) +
  scale_color_manual(values = c("illiteracy_s" = "red", 
                                "primary_s" = "blue", 
                                "universitie_s" = "green", 
                                "masteranddoctorate_s" = "purple", 
                                "unknown_s" = "orange")) +  # Farkl?? renkler
  theme_minimal() + 
  theme(legend.position = "bottom")

