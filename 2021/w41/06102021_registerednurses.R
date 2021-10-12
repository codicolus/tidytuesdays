# Tidytuesday script - week 41
# Registered Nurses
# Date: 06-10-2021
# Author: Christoph von Matt (@chvonmatt)
# ---------------------------------------------------------------------------------------------------------

# libraries
library(tidyverse)
library(geofacet)
library(ggtext)
library(glue)

# load data
nurses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-05/nurses.csv')

# clean names
nurses <- nurses %>% 
  janitor::clean_names()

# make categorical variable in advance to match box x-axes!
ggplot(nurses) +
  facet_geo(~state, scales = "fixed", grid = "us_state_grid2") +
  geom_line(aes(x = year, y = annual_salary_avg), color = "red") +
  geom_line(data = nurses %>% select(year, annual_salary_avg) %>% 
              filter(!is.na(annual_salary_avg)) %>% 
              group_by(year) %>% 
              summarise(avg_slry = sum(annual_salary_avg) / n()) %>% 
              ungroup(),
    aes(x = year, y = avg_slry), color = "black") +
  theme_light() +
  labs(title = "Annual average salaries of nurses in the United States",
       subtitle = "Per state (<span style='color:red'>red</span>), compared with the average over all states (black).",
       caption = "Plot by @chvonmatt | #TidyTuesday\nData from Data.World") +
  xlab("Year") +
  ylab("Annual average salary") +
  scale_y_continuous(limits = c(0, max(nurses$annual_salary_avg)), breaks = c(0, 50000, 100000), labels = scales::dollar_format()) +
  scale_x_continuous(breaks = seq(2000, 2020, 5), labels = c("2000", "", "2010", "", "2020")) +
  coord_fixed(ratio = 0.00014) +
  theme(
  axis.text.x = element_text(size = 10),
  axis.text.y = element_text(size = 10),
  #   axis.title = element_text(size = 14),
  axis.title.x = element_text(vjust = 5, size = 20),
  axis.title.y = element_text(vjust = 5, size = 20),
  plot.title = element_text(vjust = 2, size = 30, face = "bold"),
  plot.subtitle = element_markdown(size = 25, margin = margin(b=20), vjust = 40), #vjust = 40, 
  plot.margin = margin(t=30, r=0, b=5, l=20),
  strip.text.x = element_text(size=11),
  panel.spacing = unit(0.3, "cm", data=NULL),
  plot.caption = element_text(size = 14, color = "grey", vjust = 310)#, vjust = 300)
  )

# save
ggsave("annual_avg_salariesnurses.png", scale = 1.5, width = 14, height = 9, dpi = 300)
