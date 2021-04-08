### Housekeeping ###############################################################
rm(list = ls(all = TRUE))                        # Remove all previous objects
gc()                                             # Garbage collection
assign("last.warning", NULL, envir = baseenv())  # Reset list of past warnings
################################################################################

# Survival analysis

library(tidyverse)
library(readxl)

crises_raw <- read_excel("C:/Users/andre/Desktop/R/surv_crises/20160923_global_crisis_data.xlsx")

# Extracting our years
crises_time <- crises_raw %>% filter(Country == "United Kingdom") %>% mutate(time = as.numeric(Year)) %>% select(time)
# Checking our country names
unique(crises_raw$Country)

# Survival Analysis
library(survival)
library(survminer)
library(ggthemes)

# Filtered data for Belgium
crises_belg <- crises_raw %>%
  filter(Country == "Belgium") %>%
  mutate(event = as.numeric(`Systemic Crisis`)) %>%
  select(event)
# Filtered data for France
crises_fr <- crises_raw %>% 
  filter(Country == "France") %>%
  mutate(event = as.numeric(`Systemic Crisis`)) %>%
  select(event)
# Filtered data for United Kingdom
crises_uk <- crises_raw %>% 
  filter(Country == "United Kingdom") %>%
  mutate(event = as.numeric(`Systemic Crisis`)) %>%
  select(event)
# Filtered data for Germany
crises_ger <- crises_raw %>% 
  filter(Country == "Germany") %>%
  mutate(event = as.numeric(`Systemic Crisis`)) %>%
  select(event)

# Combining data
event_df <- data.frame(
  belg_event = crises_belg$event,
  fr_event = crises_fr$event,
  uk_event = crises_uk$event,
  ger_event = crises_ger$event,
  time = crises_time$time
  )

# Fitting and combining Kaplan-Meier survival curves for each and combining them in a single list object
belg_km <- survfit(Surv(time, belg_event) ~ 1, data = event_df, start.time = 1800)
fr_km <- survfit(Surv(time, fr_event) ~ 1, data = event_df, start.time = 1800)
uk_km <- survfit(Surv(time, uk_event) ~ 1, data = event_df, start.time = 1800)
ger_km <- survfit(Surv(time, ger_event) ~ 1, data = event_df, start.time = 1800)
list_km <- list(Belgium = belg_km, France = fr_km, `United Kingdom` = uk_km, Germany = ger_km)

# Setting a theme for our plots
library(ggthemes)
theme <- theme(axis.line = element_line(colour = "black"),
               panel.grid.major = element_line(colour = "grey90"),
               panel.grid.minor = element_line(colour = "grey90"),
               panel.border = element_blank(),
               panel.background = element_blank()) 

# FIGURE 1:
# Only Belgium since 1800
ggsurvplot(belg_km, conf.int = FALSE, censor = TRUE, 
           ylab="Systemic Crises, %", xlab="Year", break.time.by = 25,
           cumevents = TRUE, ggtheme = theme, tables.theme = theme, xlim = c(1800,2025),
           title = "Belgium's Systemic Crises, 1800-2016",
           subtitle = "Kaplan-Meier survival curve",
           caption = "Source: Harvard Business School, 'Global Crises Data by Country' \nhttps://www.hbs.edu/behavioral-finance-and-financial-stability/data/Pages/global.aspx")
# FIGURE 2:
# Only France
ggsurvplot(fr_km, conf.int = FALSE, censor = TRUE, 
           ylab="Systemic Crises, %", xlab="Year", break.time.by = 25,
           cumevents = TRUE, ggtheme = theme, tables.theme = theme, xlim = c(1800,2025),
           title = "France's Systemic Crises, 1800-2016",
           subtitle = "Kaplan-Meier survival curve",
           caption = "Source: Harvard Business School, 'Global Crises Data by Country' \nhttps://www.hbs.edu/behavioral-finance-and-financial-stability/data/Pages/global.aspx")
# FIGURE 3:
# All four countries, 1800-2016
ggsurvplot(list_km, conf.int = FALSE, censor = FALSE, combine = TRUE,
           ylab="Systemic Crises, %", xlab="Year", break.time.by = 25,
           cumevents = TRUE, ggtheme = theme, tables.theme = theme, xlim = c(1800,2025),
           title = "Systemic Crises, 1800-2016",
           subtitle = "Kaplan-Meier survival curve",
           caption = "Source: Harvard Business School, 'Global Crises Data by Country' \nhttps://www.hbs.edu/behavioral-finance-and-financial-stability/data/Pages/global.aspx")
# FIGURE 4:
# All four countries, subset to 2004-2016 for easier visualization. Our data still runs from the year 1800,
# so our 'Cumulative number of events' table includes the system crises
# prior to the year 2004 since 1800.
ggsurvplot(list_km, conf.int = FALSE, censor = FALSE, combine = TRUE,
           ylab="Systemic Crises, %", xlab="Year", break.time.by = 4,
           cumevents = TRUE, ggtheme = theme, tables.theme = theme, xlim = c(2004,2016),
           title = "Systemic Crises, 2004-2016",
           subtitle = "Kaplan-Meier survival curve",
           caption = "Source: Harvard Business School, 'Global Crises Data by Country' \nhttps://www.hbs.edu/behavioral-finance-and-financial-stability/data/Pages/global.aspx")
