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
library("viridis")

#calling the data
data <- readxl::read_excel("EMU430-DATA.xls")

#Graph 1 : Education Level Change Over the Years
{
  #adjusting data
  education_level_percentages <- data |> 
    group_by(Year) |>
    summarize(
      illiteracy_percentage = sum(`General Illiterate Population`,na.rm = TRUE) / sum(`General Population`,na.rm = TRUE),
      literates_without_diploma_percentage = sum(`General Literates Without Diploma`,na.rm = TRUE) / sum(`General Population`,na.rm = TRUE),
      primary_education_and_school_percentage = (sum(`General Primary School Graduates`,na.rm = TRUE) + sum(`General Primary Education Graduates`,na.rm = TRUE)) / sum(`General Population`,na.rm = TRUE),
      lower_secondary_school_percentage = sum(`General Lower Secondary School Graduates`,na.rm = TRUE) / sum(`General Population`,na.rm = TRUE),
      upper_secondary_school_percentage = sum(`General Upper Secondary School Graduates`,na.rm = TRUE) / sum(`General Population`,na.rm = TRUE),
      universities_percentage = sum(`General Universities and Other Higher Educational Institutions Graduates`,na.rm = TRUE) /sum(`General Population`,na.rm = TRUE),
      master_doctorate_percentage = (sum(`General Master Graduates`,na.rm = TRUE) + sum(`General Doctorate Graduates`,na.rm = TRUE)) / sum(`General Population`,na.rm = TRUE),
      unknowns_percentage = sum(`General Unknown`,na.rm = TRUE) / sum(`General Population`,na.rm = TRUE)
    )
  
  education_level_percentages_long <- education_level_percentages |> 
    pivot_longer(
      cols = c(
        illiteracy_percentage,
        literates_without_diploma_percentage,
        primary_education_and_school_percentage,
        lower_secondary_school_percentage,
        upper_secondary_school_percentage,
        universities_percentage,
        master_doctorate_percentage,
        unknowns_percentage
      ),
      names_to = "Variable",
      values_to = "Value"
    )
  
  #plotting data
  change_in_education_levels_over_years_plot <- ggplot(education_level_percentages_long, aes(x = Year, y = Value * 100, color = Variable)) +
    geom_line(size = 1.2) + 
    scale_color_manual(
      values = c(
        "illiteracy_percentage" = "green",
        "literates_without_diploma_percentage" = "black",
        "primary_education_and_school_percentage" = "blue",
        "lower_secondary_school_percentage" = "purple",
        "upper_secondary_school_percentage" = "yellow",
        "universities_percentage" = "cyan",
        "master_doctorate_percentage" = "orange",
        "unknowns_percentage" = "red"
      )) + 
    labs(
      title = "Change in Education Levels Over the Years",
      x = "Year",
      y = "Percentage - %"
    ) +
    theme_minimal()
}
