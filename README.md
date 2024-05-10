---
title: "Forest_Fires_In_Portugal"
author: "Kelly Katsouli"
date: "2024-05-03"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. 


[**Forest Fires in Portugal**]{.underline}

This analysis aims to deeply understand forest fires in Portugal. By using predictive modeling, we try to predict when they might happen next and find connections in the data. Our goal is to come up with smart plans to stop fires from starting and reduce their impact."

 Each column represents distinct pieces of information:

* **X**: X-axis spatial coordinate within the Montesinho park map: 1 to 9 
* **Y**: Y-axis spatial coordinate within the Montesinho park map: 2 to 9 
* **month**: Month of the year: 'jan' to 'dec' 
* **day**: Day of the week: 'mon' to 'sun' 
* **FFMC**: Fine Fuel Moisture Code index from the FWI system: 18.7 to 96.20 
* **DMC**: Duff Moisture Code index from the FWI system: 1.1 to 291.3 
* **DC**: Drought Code index from the FWI system: 7.9 to 860.6 
* **ISI**: Initial Spread Index from the FWI system: 0.0 to 56.10 
* **temp**: Temperature in Celsius degrees: 2.2 to 33.30 
* **RH**: Relative humidity in percentage: 15.0 to 100 
* **wind**: Wind speed in km/h: 0.40 to 9.40 
* **rain**: Outside rain in mm/m2 : 0.0 to 6.4 
* **area**: The burned area of the forest (in ha): 0.00 to 1090.84 

I'm going to load everything I will need for this analysis.

```{r}
install.packages("tidyverse")
```

```{r}
library(tidyverse)
```

```{r}
install.packages("ggplot2")
```

```{r}
library(ggplot2)
```

```{r}
forest_fires<-read.csv("forestfires.csv")
```

These lines of code find all the different months and days listed in the forest_fires dataset.

```{r}
forest_fires %>% pull(month) %>% unique
```

```{r}
forest_fires %>% pull(day) %>% unique
```

These variables naturally follow a specific order. Transforming them into factors guarantees that their arrangement in visual representations aligns with their chronological progression.

```{r}
month_order <- c("jan", "feb", "mar",
                 "apr", "may", "jun",
                 "jul", "aug", "sep",
                 "oct", "nov", "dec")

day_order <- c("sun", "mon", "tue", "wed", "thu", "fri", "sat")

forest_fires <- forest_fires %>% 
  mutate(
    month = factor(month, levels = month_order),
    day = factor(day, levels = day_order)
  )
```

# When Do Most Forest Fires Occur?

We'll start by making a summary table that shows how many fires happened in each month. Then, we'll use this table to create a visual representation.

```{r}
fires_by_month <- forest_fires %>%
  group_by(month) %>%
  summarize(total_fires = n())

fires_by_month %>% 
  ggplot(aes(x = month, y = total_fires)) +
  geom_col() +
  labs(
    title = "Number of forest fires in data by month",
    y = "Fire count",
    x = "Month"
  )
```

This table is similar to the previous one, illustrating the frequency of fires occurring on each day.


**Visualizing Relationships between Time and Other Variables by Month**

```{r}
forest_fires_long <- forest_fires %>% 
  pivot_longer(
    cols = c("FFMC", "DMC", "DC", 
             "ISI", "temp", "RH", 
             "wind", "rain"),
    names_to = "data_col",
    values_to = "value"
  )

forest_fires_long %>% 
  ggplot(aes(x = month, y = value)) +
  geom_point() +
  facet_wrap(vars(data_col), scale = "free_y") +
  labs(
    title = "Variable changes over month",
    x = "Month",
    y = "Variable value",
  )
```

**Visualizing Relationships between Time and Other Variables by Day**

```{r}
forest_fires_long_2 <- forest_fires %>% 
  pivot_longer(
    cols = c("FFMC", "DMC", "DC", 
             "ISI", "temp", "RH", 
             "wind", "rain"),
    names_to = "data_col",
    values_to = "value"
  )

forest_fires_long_2 %>% 
  ggplot(aes(x = day, y = value)) +
  geom_point() +
  facet_wrap(vars(data_col), scale = "free_y") +
  labs(
    title = "Variable changes in a day",
    x = "Day",
    y = "Variable value",
  )
```

**Exploring the Intensity of Forest**

```{r}
forest_fires_long %>% 
  ggplot(aes(x = value, y = area)) +
  geom_point() +
  facet_wrap(vars(data_col), scales = "free_x") +
  labs(
    title = "Relationships between other variables and area burned",
    x = "Value of column",
    y = "Area burned (hectare)"
  )
```



```{r}
forest_fires_long %>% 
  filter(area < 300) %>% 
  ggplot(aes(x = value, y = area)) +
  geom_point() +
  facet_wrap(vars(data_col), scales = "free_x") +
  labs(
    title = "Relationships between other variables and area burned (area < 300)",
    x = "Value of column",
    y = "Area burned (hectare)"
  )
```

[**Addressing Outliers:**]{.underline}

Some outliers in the data, particularly in the area burned variable, significantly impact the scale of visualizations. By excluding these extreme observations, we can obtain clearer insights into how other variables relate to the area burned.

***Based on the visualizations provided:***

**1.Monthly Fire Occurrences:**

There is a notable increase in forest fires during August and September, with a smaller spike observed in March. Additionally, fires seem to be more frequent during weekends.

**2.Variable Changes Over Time:**

When examining various environmental variables over the months, we observe some changes. For instance, factors like Fine Fuel Moisture Code (FFMC), Duff Moisture Code (DMC), and Drought Code (DC) show varying patterns across different months.

**3.Relationships between Variables and Fire Severity**:

When investigating the relationship between different environmental variables and the area burned, we notice some interesting patterns. For example, temperature (temp) and relative humidity (RH) appear to have a noticeable impact on the extent of forest fires. Higher temperatures and lower humidity levels seem to correlate with larger areas burned.

**Based on the observations from the data:**

**1.Stay Alert during Risky Months:** Pay extra attention in August and September. Increase patrols and remind people about fire safety.

**2.Be Prepared for Weekends:** Ensure enough firefighters and resources are available as fires are more common on weekends.

**3.Monitor Weather Conditions:** Keep an eye on temperature and wind speed. Act fast if conditions are dangerous.

**4.Teach Fire Safety:** Educate people on preventing fires and acting responsibly in forests.

**5.Involve the Community:** Engage everyone in keeping forests safe through clean-ups and reporting suspicious activity.
