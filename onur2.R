library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(sf)
library(gganimate)
library(writexl)


#calling the data
data <- readxl::read_excel("EMU430-DATA.xls")

#Graph 1: Education Level Change Over the Years
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
  ggplot(education_level_percentages_long, aes(x = Year, y = Value * 100, color = Variable)) +
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
      ),
      labels = c(
        "illiteracy_percentage" = "Illiteracy",
        "literates_without_diploma_percentage" = "Literates Without Diploma",
        "primary_education_and_school_percentage" = "Primary Education",
        "lower_secondary_school_percentage" = "Lower Secondary School",
        "upper_secondary_school_percentage" = "Upper Secondary School",
        "universities_percentage" = "Universities",
        "master_doctorate_percentage" = "Master/Doctorate",
        "unknowns_percentage" = "Unknowns"
      )
    ) + 
    labs(
      title = "Change in Education Levels Over the Years",
      x = "Year",
      y = "Percentage - %"
    ) +
    theme_minimal()
  
  
  
}

#Graph 1.1.1 : Illiterates Change Over the Years by Provinces -MAP
{
  #Calling the map data
  turkey_map <- st_read("gadm41_TUR_1.json")
  #Adjusting the Map data
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
  
  
  turkey_map$ISO_1 <-  as.numeric(gsub("TR-", "", turkey_map$ISO_1))
  names(turkey_map)[11] <- "Province Code"
  names(data)[2] <- "Province Code"
  
  data_with_map <- left_join(turkey_map,data,by = "Province Code")
  
  #Plotting the Data
illiterateanim <-  ggplot(data = data_with_map) +
    geom_sf(aes(fill = `General Illiterate Population`/`General Population`)) +
    labs(title = "Percentage of Illiteracy Relative to the General Population in Year :{frame_time}", x = "Longitude", y = "Latitude") +
    theme_minimal() +
    scale_fill_gradientn(
      name = "Percentage of Illiteracy",
      colors = c("white", "red"),
    ) +
    theme(legend.position = "bottom") +
    transition_time(as.integer(data_with_map$Year))
  
  anim_save("illiterateanim.gif", animation = illiterateanim)
  
  #Plotting the Data
    LiteratesWODiplomaAnim <-  ggplot(data = data_with_map) +
    geom_sf(aes(fill =`General Literates Without Diploma`/`General Population`)) +
    labs(title = "Percentage of Literates Without Diploma Relative to the General Population in Year :{frame_time}", x = "Longitude", y = "Latitude") +
    theme_minimal() +
    scale_fill_gradientn(
      name = "Percentage of Literates Without Diploma",
      colors = c("white", "red"),
    ) +
    theme(legend.position = "bottom") +
    transition_time(as.integer(data_with_map$Year))
  
  anim_save("LiteratesWODiplomaanim.gif", animation = LiteratesWODiplomaAnim)
  
  #Plotting the Data
  LSSAnim <- ggplot(data = data_with_map) +
    geom_sf(aes(fill =`General Lower Secondary School Graduates`/`General Population`)) +
    labs(title = "Percentage of Lower Secondary School Graduates Relative to the General Population in Year :{frame_time}", x = "Longitude", y = "Latitude") +
    theme_minimal() +
    scale_fill_gradientn(
      name = "Percentage of Lower Secondary School Graduates",
      colors = c("white", "red"),
    ) +
    theme(legend.position = "bottom") +
    transition_time(as.integer(data_with_map$Year))
  
  anim_save("LSSAnim.gif", animation = LSSAnim)
  
  #Plotting the Data
  PE_GraduatesAnim <- ggplot(data = data_with_map) +
    geom_sf(aes(fill =(`General Primary School Graduates` + `General Primary Education Graduates`)/`General Population`)) +
    labs(title = "Percentage of Primary Education Graduates Relative to the General Population in Year :{frame_time}", x = "Longitude", y = "Latitude") +
    theme_minimal() +
    scale_fill_gradientn(
      name = "Percentage of Primary Education Graduates",
      colors = c("white", "yellow"),
    ) +
    theme(legend.position = "bottom") +
    transition_time(as.integer(data_with_map$Year))
  
  anim_save("PE_.gif", animation = PE_GraduatesAnim)
  
  #Plotting the Data
  USS_GraduatesAnim <- ggplot(data = data_with_map) +
    geom_sf(aes(fill =(data_with_map$`General Upper Secondary School Graduates`/`General Population`))) +
    labs(title = "Percentage of Upper Secondary School Relative to the General Population in Year :{frame_time}", x = "Longitude", y = "Latitude") +
    theme_minimal() +
    scale_fill_gradientn(
      name = "Percentage of Upper Secondary School Graduates",
      colors = c("white", "yellow"),
    ) +
    theme(legend.position = "bottom") +
    transition_time(as.integer(data_with_map$Year))
  
  anim_save("USS.gif", animation = USS_GraduatesAnim)
  
  #Plotting the Data
  U_GraduatesAnim <- ggplot(data = data_with_map) +
    geom_sf(aes(fill =(data_with_map$`General Universities and Other Higher Educational Institutions Graduates`/`General Population`))) +
    labs(title = "Percentage of University Graduates Relative to the General Population in Year :{frame_time}", x = "Longitude", y = "Latitude") +
    theme_minimal() +
    scale_fill_gradientn(
      name = "Percentage of University Graduates",
      colors = c("white", "green"),
    ) +
    theme(legend.position = "bottom") +
    transition_time(as.integer(data_with_map$Year))
  
  anim_save("U.gif", animation = U_GraduatesAnim)
  
  #Plotting the Data
  MD_GraduatesAnim <- ggplot(data = data_with_map) +
    geom_sf(aes(fill =((data_with_map$`General Master Graduates` + data_with_map$`General Doctorate Graduates`)/`General Population`))) +
    labs(title = "Percentage of Master or Doctorate Graduates Relative to the General Population in Year :{frame_time}", x = "Longitude", y = "Latitude") +
    theme_minimal() +
    scale_fill_gradientn(
      name = "Percentage of Master or Doctorate Graduates",
      colors = c("white", "green"),
    ) +
    theme(legend.position = "bottom") +
    transition_time(as.integer(data_with_map$Year))
  
  anim_save("MD.gif", animation = MD_GraduatesAnim)
  
  #Plotting the Data
  UKGraduatesAnim <- ggplot(data = data_with_map) +
    geom_sf(aes(fill =(data_with_map$`General Unknown`/`General Population`))) +
    labs(title = "Percentage of Unknowns Relative to the General Population in Year :{frame_time}", x = "Longitude", y = "Latitude") +
    theme_minimal() +
    scale_fill_gradientn(
      name = "Percentage of Unknowns ",
      colors = c("white", "green"),
    ) +
    theme(legend.position = "bottom") +
    transition_time(as.integer(data_with_map$Year))
  
  anim_save("UK.gif", animation = UKGraduatesAnim)
  
  
}
{
  #Data 1.1.1 Illiterates Change Over the Years
illiterates_change <- education_level_percentages |>
  select(Year,illiteracy_percentage)
illiterates_change[2] <- illiterates_change[2] * 100

write_xlsx(illiterates_change,path = "illiterates_Change.xlsx")

#Data 1.1.2 Literates WO Diploma Change Over the Years
LiteratesWODiploma <- education_level_percentages |>
  select(Year,literates_without_diploma_percentage)
LiteratesWODiploma[2] <- LiteratesWODiploma[2] * 100

write_xlsx(LiteratesWODiploma,path = "LiteratesWODiploma_Change.xlsx")

#Data 1.1.3  LSS Change Over the Years
LSS_Change <- education_level_percentages |>
  select(Year,lower_secondary_school_percentage)
LSS_Change[2] <- LSS_Change[2] * 100

write_xlsx(LSS_Change,path = "LSSChange.xlsx")

#Data 1.1.4 Primary Education Graduates Change Over the Years
PE_Graduates <- education_level_percentages |>
  select(Year,primary_education_and_school_percentage)
PE_Graduates[2] <- PE_Graduates[2] * 100

write_xlsx(PE_Graduates,path = "PE_Change.xlsx")

#Data 1.1.5  USS Graduates Change Over the Years
USS_Graduates <- education_level_percentages |>
  select(Year,upper_secondary_school_percentage)
USS_Graduates[2] <- USS_Graduates[2] * 100

write_xlsx(USS_Graduates,path = "USS_Change.xlsx")

#Data 1.1.6  University Graduates Change Over the Years
U_Graduates <- education_level_percentages |>
  select(Year,universities_percentage)
U_Graduates[2] <- U_Graduates[2] * 100

write_xlsx(U_Graduates,path = "U_Change.xlsx")

#Data 1.1.7  Master / Doctorate Graduates Change Over the Years
MD_Graduates <- education_level_percentages |>
  select(Year,master_doctorate_percentage)
MD_Graduates[2] <- MD_Graduates[2] * 100

write_xlsx(MD_Graduates,path = "MD_Change.xlsx")

#Data 1.1.8  Unknowns Change Over the Years
UnknownsChange <- education_level_percentages |>
  select(Year,unknowns_percentage)
UnknownsChange[2] <- UnknownsChange[2] * 100

write_xlsx(UnknownsChange,path = "Unknown_Change.xlsx")
}
