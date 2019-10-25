library(tidyverse)
library(cowplot)
library(data.table)
library(plotly)
library(shinyWidgets)
library(shinydashboard)
library(shinyBS)
library(shinyhelper)


meteo <- read_csv("www/meteo.csv") %>% 
  mutate(svpmax = 6.1078*exp(17.269*tmax/(237.3+tmax))*0.1) %>% 
  mutate(svpmin = 6.1078*exp(17.269*tmin/(237.3+tmin))*0.1)

plant <- read_csv("www/plants.csv")

soils <- read_csv("www/soils.csv")

# Colorblind palette 
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

