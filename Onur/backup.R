## 1- Education Level Change Over the Years {toc-text="Graph 1"}

```{r message=FALSE, warning=FALSE}
#| code-fold: true
#| code-summary: "Show the code"

library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)

#calling the data
data <- readxl::read_excel("EMU430-DATA.xls")

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
```

![](Change%20in%20Education%20Levels%20Over%20the%20Years%20Plotted.png)

At first glance, the graph highlights that the majority of Turkey's population consists of primary education graduates. Over the years, the data shows a decline in primary education, literates without a diploma, illiteracy, and unknown categories, while upper secondary school, university, lower secondary school, and master/doctorate categories have increased. This indicates that Turkey's overall education level has significantly improved compared to 2008.

### **1.1 Illiterates** {toc-text="Illiterates"}

We can clearly see that the rate of illitrate has almost doubled between 2008-2023. This means that the value and awareness given to reading is increasing due to various reasons. The main reasons can be:
  
  1\. Improvements in Education Policies

2\. Social and Economic Developments

3.Technological Advancements and Access to Information

4.Socio-Cultural Changes

```{r}
#| echo: false

library(readxl)
library(knitr)
illiterate_dataset <- readxl::read_excel("illiterates_Change.xlsx")
kable(
  head(illiterate_dataset, 15),
  format = "html",
  caption = "Illiterates Percentage Change",
  align = "c"
)
```

![](illiterateoveryears.gif)


```{r message=FALSE, warning=FALSE}
#| code-fold: true
#| code-summary: "Show the code"

library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(sf)
library(gganimate)
library(writexl)

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
}
```

![](illiterateanim.gif)

![](Illiteracy_Change_Rates_by_Region_08-23.png)

```{r message=FALSE, warning=FALSE}
#| code-fold: true
#| code-summary: "Show the code"

data |>
  filter(Year %in% c(2008, 2023), !is.na(Region), !is.na(`Province code`)) |>
  group_by(`Province code`, Region) |>
  summarize(
    Illiterate2023 = sum(`General Illiterate Population`[Year == 2023], na.rm = TRUE),
    Illiterate2008 = sum(`General Illiterate Population`[Year == 2008], na.rm = TRUE)
  ) |>
  mutate(
    ChangeRate = (Illiterate2023 - Illiterate2008) / Illiterate2008
  ) |>
  ggplot(aes(x = Region, y = ChangeRate, fill = Region)) +
  geom_boxplot() +
  labs(
    x = "Region",
    y = "Change Rate of Illiterate Population (2008-2023)",
    title = "Illiteracy Change Rates by Region (2008-2023)"
  ) +
  theme_minimal()
```

### Literates Without Diploma {toc-text="LiteratesWODiploma"}

LiteratesWODiploma Doldurcaz

```{r}
#| echo: false


library(readxl)
library(knitr)
literates_wo_diploma_dataset <- readxl::read_excel("LiteratesWODiploma_Change.xlsx")
kable(
  head(literates_wo_diploma_dataset, 15),
  format = "html",
  caption = "Literates Without Diploma Percentage Change",
  align = "c"
)
```

![](LiteratesWODiplomaanim.gif)

### **Primary Education Graduates** {toc-text="Primary Education Graduates"}

```{r}
#| echo: false


library(readxl)
library(knitr)
PE_Dataset <- readxl::read_excel("PE_Change.xlsx")
kable(
  head(PE_Dataset, 15),
  format = "html",
  caption = "Primary Education Graduates Percentage Change",
  align = "c"
)
```

![](PE_.gif)

### Lower Secondary School Graduates {toc-text="LOwer Secondary School Graduates"}

```{r}
#| echo: false


library(readxl)
library(knitr)
LSS_Dataset <- readxl::read_excel("LSSChange.xlsx")
kable(
  head(LSS_Dataset, 15),
  format = "html",
  caption = "Lower Secondary School Graduates Percentage Change",
  align = "c"
)
```

![](LSSAnim.gif)

### **Upper Secondary School Graduates** {toc-text="Upper Secondary School Graduates"}

```{r}
#| echo: false


library(readxl)
library(knitr)
uss_dataset <- readxl::read_excel("USS_Change.xlsx")
kable(
  head(uss_dataset, 15),
  format = "html",
  caption = "Upper Secondary School Graduates Percentage Change",
  align = "c"
)
```

![](USS.gif)

### University Graduates {toc-text="University Graduates"}

```{r}
#| echo: false


library(readxl)
library(knitr)
U_Dataset <- readxl::read_excel("U_Change.xlsx")
kable(
  head(U_Dataset, 15),
  format = "html",
  caption = "University Graduates Percentage Change",
  align = "c"
)
```

![](U.gif)

### **Master and Doctorate Graduates** {toc-text="Master and Doctorate Graduates"}

```{r}
#| echo: false


library(readxl)
library(knitr)
MD_Dataset <- readxl::read_excel("MD_Change.xlsx")
kable(
  head(MD_Dataset, 15),
  format = "html",
  caption = "Master or Doctorate Graduates Percentage Change",
  align = "c"
)
```

![](MD.gif)

### Unknowns {toc-text="Unknowns"}

```{r}
#| echo: false


library(readxl)
library(knitr)
UK_Dataset <- readxl::read_excel("Unknown_Change.xlsx")
kable(
  head(MD_Dataset, 15),
  format = "html",
  caption = "Unknowns Percentage Change",
  align = "c"
)
```

![](UK.gif)
