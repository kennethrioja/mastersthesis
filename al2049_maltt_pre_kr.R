## /\ ----
## __________________________
## Title: Clean TBALL dataset (Master's thesis, 2024)
## Author: Kenneth Rioja, University of Geneva
## Email: kenneth.rioja@unige.ch
## Date Created: 2024-05-30
## __________________________
## Notes:
##  
## __________________________

## Top ----
## clear workspace
rm(list=ls())

## Seed for random number generation
set.seed(42)

## Load packages
# install.packages("pacman")
library(pacman)
pacman::p_load(
  "rstudioapi",
  "dplyr"
  # insert packages' name here
)

## Set working directory to current folder
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

## For TOC : https://community.rstudio.com/t/starting-toc-on-a-new-page-after-title-page-in-rmd-to-word-doc-output/36081/2
devtools::source_gist("c83e078bf8c81b035e32c3fc0cf04ee8",
                      filename = 'render_toc.R')

## Load environment or read .csv files
df_raw <- readxl::read_xlsx("assets/priv/ng/TBALL_EventVariable_start_until2023_07_18_TRIEES.xlsx")

## Functions ----

tableall <- function(df, list) {
  ## Prints all the tables of a given list
  ## @params1 df : The name of the dataframe
  ## @params2 list : The list of the column names you want to display, e.g., c("col1", "col2")
  n = 0
  for (n in 1:length(list)) {
    cat(">>>", list[n], "------------------------------------", n)
    print(table(df[list[n]]))
    cat("NA =", sum(is.na(df[list[n]])), "\n\n")
  }
}

convertToNum <- function(col) {
  ## Convert column to numeric
  as.numeric(as.character(col))
}

find_tlist <- function(num) {
  ## Find plot in tlist
  ## @params1 num : The number of the plot, not the ID.
  page = 2
  a = num/2
  if (a %% 1 == 0.5) {
    a = a + 0.5
    page = 1
  }
  return(tlist[[a]][[page]])
}

## Global Variables ----

## Colors
c_fossil = "coral2"
c_green = "darkolivegreen3"
c_human = "dodgerblue"

## Mapping of numeric values to character values for the installation
card_code <- c(
  "1" = "chicken",
  "2" = "cow",
  "3" = "fish",
  "4" = "insect",
  "5" = "crops",
  "6" = "leguminous",
  "7" = "veggies",
  "8" = "meat_transfo",
  "9" = "vegetarian_transfo",
  "10" = "resto",
  "11" = "market",
  "12" = "lab",
  "13" = "hunting"
)

## Variable list
# table(dfs$variable, useNA="always")
variable_list <- c( 
  "FoodProduction",
  "FoodLoss",
  "Pollution",
  "Co2Emission",
  "Pleasure",
  "WorkAmount",
  "FoodDiversity",
  "Health",
  "WellBeing",
  "EnergyHuman",
  "EnergyGreen",
  "EnergyFossil"
)

## Tidy ----

### Check n Start per ID ----

## 79e90533-9391-403a-b575-e4ca0657749d has two games under same ID, split it in two
# unique(table(raw$session_ID[raw$verb == "Start" & raw$session_ID == "79e90533-9391-403a-b575-e4ca0657749d"]) > 1)
# length(raw$session_ID[raw$session_ID == "79e90533-9391-403a-b575-e4ca0657749d"]) == 3186
dfg <- df_raw %>%
  dplyr::mutate(session_ID = ifelse((grepl("^20230428.*$", timeStamp, ignore.case = TRUE)
                                     & session_ID == "79e90533-9391-403a-b575-e4ca0657749d"),
                                    "79e90533-9391-403a-b575-e4ca0657749e",
                                    session_ID))

### Translate Game States ----

dfs <- dfg %>% 
  ## Split GS by # creating p for phase, s for space, n for news
  dplyr::mutate(
    phase = dplyr::if_else(variable == 'GameState', stringr::str_split(value, '#', simplify = TRUE)[, 1], NA_character_),
    s = dplyr::if_else(variable == 'GameState', stringr::str_split(value, '#', simplify = TRUE)[, 2], NA_character_),
    n = dplyr::if_else(variable == 'GameState', stringr::str_split(value, '#', simplify = TRUE)[, 3], NA_character_)
  ) %>%
  
  ## Replace phase values to I and II and populate every rows
  dplyr::group_by(session_ID) %>% # group by ID
  tidyr::fill(phase) %>% # fill phase down
  tidyr::fill(phase, .direction = "up") %>% # and up for each first 13 lines
  dplyr::ungroup(session_ID) %>% # un group
  dplyr::mutate(
    phase = dplyr::recode(phase, '0' = 'I', '1' = 'II'), 
  ) %>% 
  
  ## Split s by rooms
  tidyr::separate(s, into = c("s_r1", "s_r2", "s_r3", "s_r4", "s_r5", "s_r6", "s_r8", "s_ext", "null"), sep = ";", fill = "right") %>%
  dplyr::select(-starts_with("null")) %>% ## this is the last split string which corresponds to nothing
  
  ## Split each room by installation and energy(human*green*fossil)
  dplyr::mutate(
    
    ## Room 1
    s_r1_install = dplyr::if_else(!grepl("!", s_r1), s_r1, stringr::str_split(s_r1, "!", simplify = TRUE)[, 1]),
    s_r1_energy = dplyr::if_else(!grepl("!", s_r1), NA_character_, stringr::str_split(s_r1, "!", simplify = TRUE)[, 2]),
    s_r1_variant = dplyr::if_else(!grepl("!", s_r1), NA_character_, stringr::str_split(s_r1, "!", simplify = TRUE)[, 3]),
    s_r1_human = dplyr::if_else(!grepl("\\*", s_r1_energy), NA_character_, stringr::str_split(s_r1_energy, "\\*", simplify = TRUE)[, 1]),
    s_r1_green = dplyr::if_else(!grepl("\\*", s_r1_energy), NA_character_, stringr::str_split(s_r1_energy, "\\*", simplify = TRUE)[, 2]),
    s_r1_fossil = dplyr::if_else(!grepl("\\*", s_r1_energy), NA_character_, stringr::str_split(s_r1_energy, "\\*", simplify = TRUE)[, 3]),
    s_r1 = NULL,
    s_r1_energy = NULL,
    
    ## Room 2
    s_r2_install = dplyr::if_else(!grepl("!", s_r2), s_r2, stringr::str_split(s_r2, "!", simplify = TRUE)[, 1]),
    s_r2_energy = dplyr::if_else(!grepl("!", s_r2), NA_character_, stringr::str_split(s_r2, "!", simplify = TRUE)[, 2]),
    s_r2_variant = dplyr::if_else(!grepl("!", s_r2), NA_character_, stringr::str_split(s_r2, "!", simplify = TRUE)[, 3]),
    s_r2_human = dplyr::if_else(!grepl("\\*", s_r2_energy), NA_character_, stringr::str_split(s_r2_energy, "\\*", simplify = TRUE)[, 1]),
    s_r2_green = dplyr::if_else(!grepl("\\*", s_r2_energy), NA_character_, stringr::str_split(s_r2_energy, "\\*", simplify = TRUE)[, 2]),
    s_r2_fossil = dplyr::if_else(!grepl("\\*", s_r2_energy), NA_character_, stringr::str_split(s_r2_energy, "\\*", simplify = TRUE)[, 3]),
    s_r2 = NULL,
    s_r2_energy = NULL,
    
    ## Room 3
    s_r3_install = dplyr::if_else(!grepl("!", s_r3), s_r3, stringr::str_split(s_r3, "!", simplify = TRUE)[, 1]),
    s_r3_energy = dplyr::if_else(!grepl("!", s_r3), NA_character_, stringr::str_split(s_r3, "!", simplify = TRUE)[, 2]),
    s_r3_variant = dplyr::if_else(!grepl("!", s_r3), NA_character_, stringr::str_split(s_r3, "!", simplify = TRUE)[, 3]),
    s_r3_human = dplyr::if_else(!grepl("\\*", s_r3_energy), NA_character_, stringr::str_split(s_r3_energy, "\\*", simplify = TRUE)[, 1]),
    s_r3_green = dplyr::if_else(!grepl("\\*", s_r3_energy), NA_character_, stringr::str_split(s_r3_energy, "\\*", simplify = TRUE)[, 2]),
    s_r3_fossil = dplyr::if_else(!grepl("\\*", s_r3_energy), NA_character_, stringr::str_split(s_r3_energy, "\\*", simplify = TRUE)[, 3]),
    s_r3 = NULL,
    s_r3_energy = NULL,
    
    ## Room 4
    s_r4_install = dplyr::if_else(!grepl("!", s_r4), s_r4, stringr::str_split(s_r4, "!", simplify = TRUE)[, 1]),
    s_r4_energy = dplyr::if_else(!grepl("!", s_r4), NA_character_, stringr::str_split(s_r4, "!", simplify = TRUE)[, 2]),
    s_r4_variant = dplyr::if_else(!grepl("!", s_r4), NA_character_, stringr::str_split(s_r4, "!", simplify = TRUE)[, 3]),
    s_r4_human = dplyr::if_else(!grepl("\\*", s_r4_energy), NA_character_, stringr::str_split(s_r4_energy, "\\*", simplify = TRUE)[, 1]),
    s_r4_green = dplyr::if_else(!grepl("\\*", s_r4_energy), NA_character_, stringr::str_split(s_r4_energy, "\\*", simplify = TRUE)[, 2]),
    s_r4_fossil = dplyr::if_else(!grepl("\\*", s_r4_energy), NA_character_, stringr::str_split(s_r4_energy, "\\*", simplify = TRUE)[, 3]),
    s_r4 = NULL,
    s_r4_energy = NULL,
    
    ## Room 5
    s_r5_install = dplyr::if_else(!grepl("!", s_r5), s_r5, stringr::str_split(s_r5, "!", simplify = TRUE)[, 1]),
    s_r5_energy = dplyr::if_else(!grepl("!", s_r5), NA_character_, stringr::str_split(s_r5, "!", simplify = TRUE)[, 2]),
    # s_r5_variant = dplyr::if_else(!grepl("!", s_r5), NA_character_, stringr::str_split(s_r5, "!", simplify = TRUE)[, 3]), # no variants
    s_r5_human = dplyr::if_else(!grepl("\\*", s_r5_energy), NA_character_, stringr::str_split(s_r5_energy, "\\*", simplify = TRUE)[, 1]),
    s_r5_green = dplyr::if_else(!grepl("\\*", s_r5_energy), NA_character_, stringr::str_split(s_r5_energy, "\\*", simplify = TRUE)[, 2]),
    s_r5_fossil = dplyr::if_else(!grepl("\\*", s_r5_energy), NA_character_, stringr::str_split(s_r5_energy, "\\*", simplify = TRUE)[, 3]),
    s_r5 = NULL,
    s_r5_energy = NULL,
    
    ## Room 6
    s_r6_install = dplyr::if_else(!grepl("!", s_r6), s_r6, stringr::str_split(s_r6, "!", simplify = TRUE)[, 1]),
    s_r6_energy = dplyr::if_else(!grepl("!", s_r6), NA_character_, stringr::str_split(s_r6, "!", simplify = TRUE)[, 2]),    
    # s_r6_variant = dplyr::if_else(!grepl("!", s_r6), NA_character_, stringr::str_split(s_r6, "!", simplify = TRUE)[, 3]),
    s_r6_human = dplyr::if_else(!grepl("\\*", s_r6_energy), NA_character_, stringr::str_split(s_r6_energy, "\\*", simplify = TRUE)[, 1]),
    s_r6_green = dplyr::if_else(!grepl("\\*", s_r6_energy), NA_character_, stringr::str_split(s_r6_energy, "\\*", simplify = TRUE)[, 2]),
    s_r6_fossil = dplyr::if_else(!grepl("\\*", s_r6_energy), NA_character_, stringr::str_split(s_r6_energy, "\\*", simplify = TRUE)[, 3]),
    s_r6 = NULL,
    s_r6_energy = NULL,
    
    ## Room 8
    s_r8_install = dplyr::if_else(!grepl("!", s_r8), s_r8, stringr::str_split(s_r8, "!", simplify = TRUE)[, 1]),
    s_r8_energy = dplyr::if_else(!grepl("!", s_r8), NA_character_, stringr::str_split(s_r8, "!", simplify = TRUE)[, 2]),
    # s_r8_variant = dplyr::if_else(!grepl("!", s_r8), NA_character_, stringr::str_split(s_r8, "!", simplify = TRUE)[, 3]),
    s_r8_human = dplyr::if_else(!grepl("\\*", s_r8_energy), NA_character_, stringr::str_split(s_r8_energy, "\\*", simplify = TRUE)[, 1]),
    s_r8_green = dplyr::if_else(!grepl("\\*", s_r8_energy), NA_character_, stringr::str_split(s_r8_energy, "\\*", simplify = TRUE)[, 2]),
    s_r8_fossil = dplyr::if_else(!grepl("\\*", s_r8_energy), NA_character_, stringr::str_split(s_r8_energy, "\\*", simplify = TRUE)[, 3]),
    s_r8 = NULL,
    s_r8_energy = NULL,
    
    ## Ext
    s_ext_install = dplyr::if_else(!grepl("!", s_ext), s_ext, stringr::str_split(s_ext, "!", simplify = TRUE)[, 1]),
    s_ext_energy = dplyr::if_else(!grepl("!", s_ext), NA_character_, stringr::str_split(s_ext, "!", simplify = TRUE)[, 2]),
    # s_ext_variant = dplyr::if_else(!grepl("!", s_ext), NA_character_, stringr::str_split(s_ext, "!", simplify = TRUE)[, 3]), # no variant in huntin
    s_ext_human = dplyr::if_else(!grepl("\\*", s_ext_energy), NA_character_, stringr::str_split(s_ext_energy, "\\*", simplify = TRUE)[, 1]),
    # s_ext_green = dplyr::if_else(!grepl("\\*", s_ext_energy), NA_character_, stringr::str_split(s_ext_energy, "\\*", simplify = TRUE)[, 2]), # no green energy in hunting
    # s_ext_fossil = dplyr::if_else(!grepl("\\*", s_ext_energy), NA_character_, stringr::str_split(s_ext_energy, "\\*", simplify = TRUE)[, 3]), # no fossil energy in hunting 
    s_ext = NULL,
    s_ext_energy = NULL,
  ) %>% 
  
  ## Separate the news
  tidyr::separate(n, into = c("n_visualized", "n_appeared"), sep = "\\*", fill = "right") %>% 
  
  ## Change values of card columns to code (ex. 1:chicken, 2:cow, ...)
  dplyr::mutate_at(dplyr::vars(ends_with("_install")), ~dplyr::recode(., !!!card_code))

# dir.create("./output/data", showWarnings = T)
# write.csv(dft, "./output/data/TBALL_EventVariable_start_until2023_07_18_TRIEES_GAMESTATES.csv")

### 'variable' column ----

## Convert the column 'variable' into distinct columns (long to wide format)
dfv <- dfs %>%
  
  ## Create empty variable columns
  `is.na<-`(variable_list) %>% 
  
  ## Spread the values to the corresponding columns
  # table(dfs$variable)
  tidyr::spread(key = variable, value = value) %>% 
  dplyr::select(-`<NA>`) %>% 
  
  ## Fill down all the values
  tidyr::fill(variable_list) %>%
  tidyr::fill(variable_list, .direction = "up") %>% # for the first line
  
  ## Filter out typeName == Single
  # filter(typeName == "String") # without gameEvents (nrow = 22516)
  filter(typeName == "String" | is.na(typeName)) # with gameEvents (dfv = 161286 x 64)

### Data type ----

dft <- dfv %>% 
  
  ## change phase, human, green, fossil, char to num
  dplyr::mutate_at(dplyr::vars(variable_list, tidyselect::ends_with('_human'), tidyselect::ends_with('_green'), tidyselect::ends_with('_fossil')), convertToNum) %>% 
  # tableall(dft, names(dft[c(15, 18:58, 60:64)]))
  
  ## format time
  dplyr::mutate(
    DATETIME = as.POSIXct(paste(as.Date(DATE), format(TIME, "%H:%M:%S")), tz = "UTC")) %>% 
  dplyr::group_by(session_ID) %>% 
  dplyr::mutate(
    playTimeInSec = as.numeric(difftime(DATETIME, dplyr::first(DATETIME), units = "secs")),
  ) %>% 
  dplyr::ungroup()

## Clean ----

### Duplicate ----

## For session_ID == 3cd02c91-6fa4-45fe-b72b-3539d66f33d4 & timeStamp == 20230620T11:56:25, there are 16 identical GameState
# nrow(dft[(dft$session_ID == "3cd02c91-6fa4-45fe-b72b-3539d66f33d4" & dft$timeStamp == "20230620T11:56:25" & !is.na(dft$typeName)),]) 

# dfd <- dft %>%
#   dplyr::distinct(timeStamp, type, verb, source, typeName, GameState, n_visualized, .keep_all= TRUE)

## Tests: all tests should return TRUE

# nrow(dfd[(dfd$session_ID == "3cd02c91-6fa4-45fe-b72b-3539d66f33d4" & dfd$timeStamp == "20230620T11:56:25" & !is.na(dfd$typeName)),]) == 1
# nrow(dfd) == nrow(dft) - 17403

### playTime ----

# tail(table(dft$playTimeInSec)) ## check the largest playtime
# dft$session_ID[dft$playTimeInSec == 81383] # returns 9a6a73fa-3808-4d7d-8f7b-ee7cc6a36403
# tail(dft$playTimeInSec[dft$session_ID == "9a6a73fa-3808-4d7d-8f7b-ee7cc6a36403" & dft$playTimeInSec >= 800 & dft$playTimeInSec <= 79000 ], n = 1000) # we can see that there's a gap between 819 & 78860
# plot(dft$playTimeInSec[dft$session_ID == "9a6a73fa-3808-4d7d-8f7b-ee7cc6a36403"])
# plot(dft$EnergyFossil[dft$session_ID == "9a6a73fa-3808-4d7d-8f7b-ee7cc6a36403"])
# plot(dft$EnergyGreen[dft$session_ID == "9a6a73fa-3808-4d7d-8f7b-ee7cc6a36403"])
# plot(dft$EnergyHuman[dft$session_ID == "9a6a73fa-3808-4d7d-8f7b-ee7cc6a36403"])
# head(dft$playTimeInSec[dft$session_ID == "9a6a73fa-3808-4d7d-8f7b-ee7cc6a36403"], n = 35) # there's a gap of 22h, decision = filter it out

dfp <- dft %>%
  dplyr::filter(session_ID != "9a6a73fa-3808-4d7d-8f7b-ee7cc6a36403")

# dfpp <- dfp %>%
#   # dplyr::filter(
#     # (session_ID == "011013f3-46be-4059-8c5c-787cc528224a" |
#     #             session_ID == "01ee06e0-25de-4df6-8e3c-cc4d6ff14b53"),
#                 # typeName == "String") %>%
#   group_by(session_ID) %>% 
#   dplyr::filter(playTimeInSec >= first(playTimeInSec[EnergyFossil != 0 | EnergyGreen != 0 | EnergyHuman != 0])) %>% 
#   ungroup()

# first(n=-1, dfpp$playTimeInSec[dfpp$EnergyFossil != 0 | dfpp$EnergyGreen != 0 | dfpp$EnergyHuman != 0])

## Plot max playtime
plot_maxPlayTime <- dfp %>% 
  group_by(session_ID) %>% 
  summarize(maxplayTimeInSec = max(playTimeInSec)) %>% 
  # filter(maxplayTimeInSec > 4000) %>%
  ggplot(aes(x= forcats::fct_reorder(session_ID, desc(maxplayTimeInSec)), y = maxplayTimeInSec)) +
  geom_col(fill = "cyan", color = "black") +
  geom_text(aes(label=maxplayTimeInSec),vjust=-0.25) +
  labs(title="Max Play Time across game sessions", x="session_ID") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

## Plot max playtime by device : no problem per device
plot_maxPlayTimeGroupedByDevice <- dfp %>% 
  group_by(session_ID) %>% 
  summarize(maxplayTimeInSec = max(playTimeInSec)) %>% 
  # filter(maxplayTimeInSec > 4000) %>%
  merge(., dfp, by = "session_ID") %>%
  select(session_ID, maxplayTimeInSec, device) %>% 
  distinct(session_ID, maxplayTimeInSec, device, .keep_all= TRUE) %>% 
  ggplot(aes(x= forcats::fct_reorder(session_ID, desc(maxplayTimeInSec)), y = maxplayTimeInSec)) +
  geom_col() +
  facet_wrap( ~ device) +
  labs(title="Max playtime grouped by device", x="session_ID") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_y_continuous(labels = scales::label_comma()) # no scientific notation

## See below the plots created, we still have game sessions of more than two hours but we can leave this as is since it is not related to a specific device
plot_maxPlayTime
plot_maxPlayTimeGroupedByDevice

## New variables ----

df <- dfp %>% 
  
  ## rename session_ID
  dplyr::mutate(session_ID = paste(substr(session_ID,1,2),substr(session_ID,nchar(session_ID)-1,nchar(session_ID)), sep = "")) %>% 
  
  ## mutate energies between 0 and 1
  dplyr::mutate(
    EnergyFossilProp = round(EnergyFossil / max(unique(EnergyFossil)), digits = 2),
    EnergyGreenProp = round(EnergyGreen / max(unique(EnergyGreen)), digits = 2),
    EnergyHumanProp = round(EnergyHuman / max(unique(EnergyHuman)), digits = 2),
    playTimeInMin = round(playTimeInSec/60, digits = 2),
  ) %>% 
  
  ## select columns
  dplyr::select(
    device, session_ID, DATETIME, playTimeInSec, playTimeInMin,
    11:14, GameState,
    15, FoodProduction,FoodLoss,
    Health, Pollution, Co2Emission, FoodDiversity,
    WellBeing, WorkAmount, Pleasure,
    EnergyFossil, EnergyFossilProp, EnergyGreen, EnergyGreenProp, EnergyHuman, EnergyHumanProp,
    18:51,
    n_visualized, n_appeared
  )

## Creates a df with differences between n and n-1
# df_diff <- dfp %>%
#     dplyr::mutate_at(
#     .vars = names(dft)[sapply(dft, is.numeric)],
#     .funs = list(diff = ~ . - lag(.))
#   ) %>%
#   dplyr::select(order(c(names(dft), paste(names(dft)[sapply(dft, is.numeric)], "_diff"))))
# names(df_diff)

## Tests ----

## If all tests return TRUE, we remove all the unecessary dfs and play sound
if (
  ## dfg
  ## Only one start for 79e90533-9391-403a-b575-e4ca0657749d
  table(dfg$session_ID[dfg$verb == "Start" & dfg$session_ID == "79e90533-9391-403a-b575-e4ca0657749d"])[[1]] == 1
  ## We kept the same nrow along the mutate
  & length(df_raw$session_ID[df_raw$session_ID == "79e90533-9391-403a-b575-e4ca0657749d"]) == length(dfg$session_ID[dfg$session_ID == "79e90533-9391-403a-b575-e4ca0657749d"]) + length(dfg$session_ID[dfg$session_ID == "79e90533-9391-403a-b575-e4ca0657749e"]) 
  ## dfs
  & length(setdiff(names(dfs), names(dfg)) == c("phase", "n_visualized","n_appeared","s_r1_install","s_r1_variant","s_r1_human","s_r1_green","s_r1_fossil","s_r2_install","s_r2_variant","s_r2_human","s_r2_green","s_r2_fossil","s_r3_install","s_r3_variant","s_r3_human","s_r3_green","s_r3_fossil","s_r4_install","s_r4_variant","s_r4_human","s_r4_green","s_r4_fossil","s_r5_install","s_r5_human","s_r5_green","s_r5_fossil","s_r6_install","s_r6_human","s_r6_green","s_r6_fossil","s_r8_install","s_r8_human","s_r8_green","s_r8_fossil","s_ext_install","s_ext_human")) == 37## dfv
  & nrow(dfs)-nrow(dfv) == 75166
  & length(names(dfs)) == 53
  ## df
  & length(unique(table(df$session_ID))) == 159 ) {
  ## remove if all TRUE
  rm(dfg, dfs, dfv, dft, dfp, plot_maxPlayTime, plot_maxPlayTimeGroupedByDevice)
  beep(5)
  if (menu(c("Yes", "No"), title="Work completed! Do you want to save the current environment under './output/data'?") == 1) {
    save.image(gsub(" ","",paste("output/data/environment_mem_",gsub("-","",Sys.Date()),".RData")))
  } else {
    cat("Thank you!")
  }
} else {
  print("Something went wrong, please check.")
}