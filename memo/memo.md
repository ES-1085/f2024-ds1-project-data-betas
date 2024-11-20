Project memo
================
Team name

This document should contain a detailed account of the data clean up for
your data and the design choices you are making for your plots. For
instance you will want to document choices you’ve made that were
intentional for your graphic, e.g. color you’ve chosen for the plot.
Think of this document as a code script someone can follow to reproduce
the data cleaning steps and graphics in your handout.

``` r
library(tidyverse)
library(broom)
library(readr)
library(lubridate)
library(stringr)
library(dplyr)
library(readxl)
library(ggstream)
library(showtext)
library(ggtext)
library(gghighlight)
options(scipen = 999)
```

## Data Clean Up Steps for Overall Data

### Step 1: Load datasets

#### Step 1a: Load fossil fuel data and change column names

``` r
fuels <- read_csv("../data/Dead-River.csv",
                  col_names = c("X1", "X2", 
                                "Delivery_date", "Fuel_type",
                                "Tank_number", "Building",
                                "Gallons", "Unit_cost",
                                "Cost"))
```

#### Step 1b: Load building area data

``` r
area <- read_csv("../data/BuildingArea.csv",
                 col_names = c("Building", "Square_feet"))
```

    ## Rows: 23 Columns: 2
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (2): Building, Square_feet
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

#### Step 1c: Join area data to fuels data

``` r
fuels <- fuels |>
left_join(area)
```

    ## Joining with `by = join_by(Building)`

### Step 2: Change Delivery Date & Remove top extra row

``` r
fuels <- fuels |>
select(Delivery_date, Fuel_type, Tank_number, Building, Gallons, Unit_cost, Cost, Square_feet) |>
  filter(Delivery_date != "Delivery Date") |> 
  mutate(Delivery_date = mdy(Delivery_date))
```

### Step 3: Change tank number to integer values

``` r
fuels <- fuels |>  
  mutate(Tank_number = case_when(
   Tank_number == "Tank 8" ~ "8",
   Tank_number == "Tank 3" ~ "3",
   Tank_number == "Tank 24" ~ "24",
   Tank_number == "Tank 17" ~ "17",
   TRUE ~ Tank_number
  ),
  Tank_number = as.integer(Tank_number))
```

### Step 4: Change cost to numeric values

``` r
fuels <- fuels |>
  mutate(Cost = case_when(
    Cost == "610.12 cr" ~ "-610.12", # the cr denotes a credit/refund, so here it is changed to a negative numeric value
    TRUE ~ Cost
  ),
  Cost = as.numeric(Cost))
```

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `Cost = as.numeric(Cost)`.
    ## Caused by warning:
    ## ! NAs introduced by coercion

### Step 5: Standardize fuel type names

``` r
fuels <- fuels |>  
  mutate(Fuel_type = case_when(
    Fuel_type == "#2 HEATING OIL" ~ "Heating Oil",
    Fuel_type == "#2 Heating Oil" ~ "Heating Oil",
    Fuel_type == "SLPP #2 FUEL OIL" ~ "Heating Oil",
    Fuel_type == "SLP LIQ PROPANE" ~ "Propane",
    Fuel_type == "LIQUID PROPANE" ~ "Propane",
    Fuel_type == "DYED KEROSENE" ~ "Dyed Kerosene",
  TRUE ~ Fuel_type
  ))
```

### Step 6: Change gallons and unit cost to numeric values

``` r
fuels <- fuels |>
  mutate(Gallons = as.numeric(Gallons)) |>
  mutate(Unit_cost = as.numeric(Unit_cost))
```

### Step 7: Add year and month variables

``` r
fuels <- fuels |>
  mutate(Year = substr(Delivery_date, 1, 4)) |>
  mutate(Month = substr(Delivery_date, 6, 7)) |>
  mutate(Year = as.numeric(Year)) |>
  mutate(Month = as.numeric(Month))
```

### Step 8: Change square feet to numeric

``` r
fuels <- fuels |>
  mutate(Square_feet = case_when(
    Building == "Turrets" ~ "8400",
    Building == "Turrets Annex" ~ "4200",
    TRUE ~ Square_feet
  )) |>
  mutate(Square_feet = as.numeric(Square_feet))
```

## Actual Plots

### Plot 1: Total fuel usage by month

``` r
fuels |> 
  filter(Year < 2024, Fuel_type %in% c("Heating Oil", "Propane")) |>
  group_by(Month, Fuel_type) |>
  summarize(month_sum = sum(Gallons), .groups = "drop") |>
  ggplot(aes(x = Month, y = month_sum, fill = Fuel_type)) +
  geom_col() +
  geom_col(data = . %>% filter(Month == 12), color = "red", size = 1, show.legend = FALSE) +
  scale_y_continuous(limits = c(0, 65000), labels = scales::comma) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
 facet_wrap(~ Fuel_type, ncol = 1, scales = "free_x") +    
  geom_text(aes(x = Month, y = month_sum, label = scales::comma(month_sum)), 
            vjust = -0.5, 
            size = 2.2) +
  labs(title = "Total Fuel Usage per Month",
       subtitle = "in all campus-owned buildings, summed from 2014 - 2023",
       x = "",
       y = "Total Gallons",
       fill = "Fuel Type",
       caption = "Coastal biodiesel and kerosene are excluded from this graph as they are no longer used.") +
  
  theme(strip.text = element_blank(),
        panel.background = element_rect(fill = "transparent"), 
        panel.grid = element_blank(),
        legend.background = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.margin = margin(20,20,20,20)
        ) +
  
  scale_fill_manual(values = c("#003f5c", "#d45087"))
```

    ## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `linewidth` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

<img src="memo_files/figure-gfm/fuel_by_month-1.png" alt="A bar graph showin the total fuel usage per month for all campus owned buildings from 2014 to 2023. Months are shown on the x-axis and gallons on the y-axis. The data is split into two graphs showing the usage of different fuel types, with heating oil on top in navy blue and propane on bottom in pink. January has the highest fuel usage for both fuel types and July and August have the least. The bar for December is outlined in red on both graphs. A not is made at the bottom reading: 'Coastal biodiesel and kerosene are excluded from this graph as they are no longer used.'"  />

``` r
ggsave("months.png", width = 8, height = 6)
```

### Plot 2: Fuel consumption progression

``` r
# Colour Palette
pal=c("#003f5c",
      "#2f4b7c",
      "#665191",
      "#a05195",
      "#d45087",
      "#f95d6a",
      "#ff7c43",
      #"#ffa600",
      "#6E6675",
      "#6E6675",
      "#6E6675",
      "#6E6675",
      "#6E6675",
      "#6E6675",
      "#6E6675",
      "#6E6675",
      "#6E6675",
      "#6E6675",
      "#6E6675",
      "#6E6675",
      "#6E6675",
      "#6E6675",
      "#6E6675",
      "#6E6675",
      "#6E6675",
      "#6E6675",
      "#6E6675",
      "#6E6675",
      "#6E6675",
      "#6E6675")

# Order of buildings for the fill
order <- c( "Davis Center", "Dorr NHM", "Seafox", "Davis Village", "Blair Tyson", "Kaelber", "Arts & Sci + Gates", "Studio 5+6", "Pottery Studio", "Peach House", "Hatchery", "Greenhouse", "Carriage", "BHF New Greenhouse", "BHF Main Bldg/2 Greenhouses", "BHF Farm House", "18B Norris Ave", "171 Beech Hill Road", "14 Norris Ave", "Turrets", "Turrets Annex", "B&G", "Witchcliff Apartments", "Witchcliff", "PRF", "Cottage", "Peggy Barn", "Davis Carriage", "CHE Generator")


fuels |>
  filter(Year != 2024) |>
  group_by(Year, Building) |>
  summarise(Total_Gallons_per_years = sum(Gallons)) |>
  mutate(Building = factor(Building, levels=order)) |>
  filter(!is.na(Building)) |> #why do we need this?
  ggplot(aes(Year, Total_Gallons_per_years, fill = Building)) +
  geom_area() +
  
  scale_fill_manual(values = pal) + #get rid of lines
  scale_color_manual(values = pal) +
  scale_x_continuous(breaks=c(2014,2017,2020,2023),labels = c("2014","2017","2020","2023")) + 
  scale_y_continuous(expand = c(0,0)) +
  
  coord_cartesian(clip = "off", xlim = c(2014,2023)) +
  
  theme(
    axis.line.x = element_line(linewidth = .75),
    panel.grid = element_blank(),
    #axis.text.y = element_blank(),
    #axis.ticks.y = element_blank(),
    axis.text.x = element_text(color="black", size=10, margin = margin(5,0,0,0)),
    plot.margin = margin(20,90,20,20),
    legend.position = "none",
    panel.background = element_rect(fill = "transparent"), 
    plot.background = element_rect(fill = "transparent", 
                                       color = NA) 
  ) +
  labs(title = "Progression of Fuel Consumption",
       subtitle = "Across all Buildings, from 2014 - 2023",
       y = "Gallons of Fuel Consumed per Year",
       x = "") +
  
# Labels
  annotate("text", x = 2023.1, y = 45000,
           label = "Davis Center",
           hjust=0,
           size=3,
           lineheight=.8,
           fontface="bold",
           color=pal[1]) +
  
    annotate("text", x = 2023.1, y = 42000,
           label = "Dorr NHM",
           hjust=0,
           size=3,
           lineheight=.8,
           fontface="bold",
           color=pal[2]) +
  
    annotate("text", x = 2023.1, y = 39000,
           label = "Seafox",
           hjust=0,
           size=3,
           lineheight=.8,
           fontface="bold",
           color=pal[3]) +
  
    annotate("text", x = 2023.1, y = 34500,
           label = "Davis Village",
           hjust=0,
           size=3,
           lineheight=.8,
           fontface="bold",
           color=pal[4]) +
  
  annotate("text", x = 2023.1, y = 30000,
           label = "Blair Tyson",
           hjust=0,
           size=3,
           lineheight=.8,
           fontface="bold",
           color=pal[5]) +
  
  annotate("text", x = 2023.1, y = 23200,
           label = "Kaelber",
           hjust=0,
           size=3,
           lineheight=.8,
           fontface="bold",
           color=pal[6]) +
  
  annotate("text", x = 2023.1, y = 13000,
           label = "Arts & Sci + Gates",
           hjust=0,
           size=3,
           lineheight=.8,
           fontface="bold",
           color=pal[7]) +
  
  annotate("text", x = 2023.1, y = 3000,
           label = "Other",
           hjust=0,
           size=3,
           lineheight=.8,
           fontface="bold",
           color=pal[8]) +
  
  annotate("text", x = 2025, y = 27500,
           label = "",
           hjust=0,
           size=3,
           lineheight=.8,
           fontface="bold",
           color=pal[2]) +
  
  # Fuel Lines
  geom_segment(aes(x = 2014, y = 0, xend = 2014, yend = 63400+8000),color="black") +
  geom_point(aes(x = 2014, y = 63400+8000),color="black") +
  annotate("text", x = 2014, y = 63400+12000,
           label = "63,443",
           hjust=0.5,
           size=3,
           lineheight=.8,
           fontface="bold",
           color="black") +
  
  geom_segment(aes(x = 2019, y = 0, xend = 2019, yend = 70300+8000),color="black") +
  geom_point(aes(x = 2019, y = 70300+8000),color="black") +
  annotate("text", x = 2019, y = 70300+12000,
           label = "70,333",
           hjust=0.5,
           size=3,
           lineheight=.8,
           fontface="bold",
           color="black") +
  
  geom_segment(aes(x = 2023, y = 0, xend = 2023, yend = 46000+8000),color="black") +
  geom_point(aes(x = 2023, y = 46000+8000),color="black") +
  annotate("text", x = 2023, y = 46000+12000,
           label = "46,050",
           hjust=0.5,
           size=3,
           lineheight=.8,
           fontface="bold",
           color="black")
```

    ## `summarise()` has grouped output by 'Year'. You can override using the
    ## `.groups` argument.

<img src="memo_files/figure-gfm/progression-1.png" alt="Area graph showing the progression of total fuel consumption on campus between 2014 and 2023. The fill color of the graph indicates the building, highlighting the top 7 consumers: Arts &amp; Science + Gates, Kaelber, Blair Tyson, Davis Village, Seafox, Dorr Natural History Museum and Davis Center. Fuel consumption started at just over 60,000 in 2014, and is down to around 50,000 in 2023, with a peak in 2019 at around 70,000."  />

``` r
ggsave("progression.png", width = 10, height = 6)
```

### Plot 3: Total fuel all buildings

``` r
# library(gghighlight)  # Make sure this is loaded

library(dplyr)
library(ggplot2)
library(forcats)
library(scales)
```

    ## 
    ## Attaching package: 'scales'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     discard

    ## The following object is masked from 'package:readr':
    ## 
    ##     col_factor

``` r
fuels |>
  filter(Year == 2023 & Building != "BHF Main Bldg/2 Greenhouses") |>
  group_by(Building) |>
  summarize(Total_Gallons_per_Buildings = sum(Gallons)) |>
  filter(!is.na(Building)) |>
  arrange(desc(Total_Gallons_per_Buildings)) |>
  mutate(Building = factor(Building, levels = order),
         label_color = ifelse(row_number() <= 9, "white", "black"),
         hjust_pos = ifelse(row_number() <= 9, 1.1, -0.1)) |>
  ggplot(aes(x = Total_Gallons_per_Buildings, 
             y = fct_reorder(Building, Total_Gallons_per_Buildings, .fun = sum), 
             fill = Building)) +
  geom_col() +
  scale_x_continuous(labels = scales::comma) +
  geom_text(aes(x = Total_Gallons_per_Buildings, 
                y = Building, 
                label = scales::comma(round(Total_Gallons_per_Buildings)),
                color = label_color,
                hjust = hjust_pos),
            size = 2.7) +
  scale_fill_manual(values = pal) +
  scale_color_identity() +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "transparent"), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "transparent", 
                                       color = NA) ,
    plot.margin = margin(20,0,20,0),
    axis.ticks.y = element_blank(),
    ) +
  
  labs(title = "Use of Fuel Across COA Buildings",
       subtitle = "In 2023",
       y = "",
       x = "Total Amount of Fuel Consumed in Gallons")
```

<img src="memo_files/figure-gfm/use-of-fuel-across-all-COA-buildings-1.png" alt="A horizontal bar graph showing the building on the y-axis and the total amount of gallons of fuel used by those buildings in 2023. The fill color of the graph indicates the top 7 buildings with the most fuel consumption in order: Arts &amp; Science + Gates, Kaelber, Blair Tyson, Davis Village, Seafox, Dorr Natural History Museum and Davis Center. The bars for the other buildings are uncolored. Arts &amp; Science + Gates is the greatest consumer by far at around 15,000 gallons. Kaelber is in second place at around 7000. Davis Center, the smallest consumer out of the 7 used around 3000 gallons."  />

``` r
ggsave("buildings-total.png", width = 6, height = 6)
```

### Plot 4: Fuel per square foot all buildings

``` r
fuels |>
  filter(Year == 2023) |>
  group_by(Building) |>
  mutate(Total_Gallons_per_Buildings = sum(Gallons)) |>
  mutate(total_per_sf = (Total_Gallons_per_Buildings / Square_feet)) |>
  summarize(total_per_sf = unique(total_per_sf)) |>
  filter(!is.na(total_per_sf)) |>
  mutate(Building = factor(Building, levels=order)) |>
  ggplot(aes(y = fct_reorder(Building, total_per_sf), x = total_per_sf, fill = Building)) +
  geom_col() +
  scale_x_continuous(limits = c(0, 1.05)) +
  geom_text(aes(x = total_per_sf, 
                y = Building, 
                label = scales::comma(round(total_per_sf, 2))),
                color = "black",
                hjust = -0.2,
            size = 2.7) +
  labs(title = "Use of Fuel per Square Foot Across COA Buildings",
       subtitle = "In 2023",
       x = "Total Gallons Consumed per Square Foot",
       y = "") +
  scale_fill_manual(values = pal) +

  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "transparent"), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "transparent", 
                                       color = NA),
    plot.margin = margin(20,0,20,0),
    axis.ticks.y = element_blank()
    )
```

<img src="memo_files/figure-gfm/total_gallons_per_sf_updated-1.png" alt="Horizontal bar graph showing the building on the y-axis and the total gallons of fuel per square foot used by those buildings in 2023. The top 7 buildings in order are B&amp;G, Davis Center, Dorr Natural Hidtory Museum, Seafox, Turrets Annex, Kaelber, and Arts &amp; Sci + Gates. The fill color of the graph indicates the top 7 buildings with the most total fuel consumption (disregarding square foot) in order: Arts &amp; Science + Gates, Kaelber, Blair Tyson, Davis Village, Seafox, Dorr Natural History Museum and Davis Center. B&amp;G consumed almost 1 gallon per square foot, and Davis Center is second at around 0.8. The lowest of the 7 is Arts &amp; Science + Gates at around 0.3, and the lowest on campus is Carriage at near 0."  />

``` r
ggsave("buildings-per-sf.png", width = 6, height = 6)
```

### Plot 5: Top 3 consumers

``` r
fuels |>
  filter(Building %in% c("Blair Tyson", "Arts & Sci + Gates", "Kaelber"), Year < 2024) |>
  group_by(Building, Year) |>
  summarize(year_sum = sum(Gallons), .groups = "drop") |>
  ggplot(aes(x = Year, y = year_sum, color = Building, group = Building)) +
  #geom_point() +
  geom_line(linewidth = 2, lineend = "round") +
  
  labs(title = "Total Gallons of Fuel Consumed per Year",
       subtitle = "by the highest three buildings, from 2014 - 2023",
       x = "",
       y = "Total Gallons",
       caption = "HP = heat pumps (library stacks and archives), HPWH = heat pump water heating system") +
  scale_x_continuous(breaks = seq(from = 2014, to = 2024, by = 1)) +
  scale_color_manual(values = c("#ff7c43", "#d45087", "#f95d6a")) +
  ylim(0, NA) +
  scale_y_continuous(labels = scales::comma) +

  theme(legend.position = "none",
        plot.margin = margin(20,20,20,20),
        panel.background = element_rect(fill = "transparent"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.line.x = element_line(linewidth = .25),
        ) +
  
  coord_cartesian(clip = "off") +
  
  geom_vline(xintercept = 2020, linetype = "dotted", linewidth = 1, colour = "black") +
  geom_text(aes(x = 2020.5, y = 16000), label = "COVID", 
            colour = "black") +
  geom_text(aes(x = 2021.3, y = 8300), label = "BT HPWH installed", 
            colour = "black", size = 3) +
  geom_point(aes(x = 2022, y = 7500), color = "black", size = 3, shape = 16) +
  geom_text(aes(x = 2017, y = 8800), color = "black", label = "Kaelber HPs installed", 
            size = 3) +
  geom_point(aes(x = 2017, y = 9550), color = "black", size = 3, shape = 16) +
  geom_line(aes(y = 900), color = "#2f4b7c", linewidth = 2, lineend = "round") +
  geom_text(aes(x = 2021.75, y = 1500), label = "Well insulated family house", 
            color = "#2f4b7c", size = 3) +
  geom_text(aes(x = 2015, y = 15000), label = "Arts & Sci + Gates", 
            colour = "#ff7c20", size = 3, fontface = "bold") +
  geom_text(aes(x = 2015, y = 11000), label = "Kaelber", 
            colour = "#f95d6a", size = 3, fontface = "bold") +
  geom_text(aes(x = 2015, y = 7000), label = "Blair Tyson", 
            colour = "#d45087", size = 3, fontface = "bold")
```

    ## Scale for y is already present.
    ## Adding another scale for y, which will replace the existing scale.

<img src="memo_files/figure-gfm/top-3-over-time-1.png" alt="A line graph showing the total gallons of fuel used by three biggest fuel consuming buildings, Arts &amp; Sciences + Gates, Kaebler, and Blair Tyson, from the years 2014 to 2023. There is also a line to show the average fuel consumption of a well insulated family home, which consumes much less fuel than either of the three buildings depicted. A dotted vertical line on 2020 marks where COVID affected fuel useage. A dot on Kaebler's line in 2017 shows when the heating pump was installed and a similar dot on Blair Tyston's line in 2022 shows when its heating pump was installed. Arts &amp; Sciences + Gates used the most fuel on average, while Blair Tyson used the least."  />

``` r
ggsave("top-3.png", width = 7, height = 5)
```
