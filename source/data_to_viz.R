# *************************************************************************
# NBA - PR2 Visualització de dades
# Authors: Marc Mejías Muñoz
# Data: 08/06/2024
# Description:
#     Projecte de visualització.
# *************************************************************************

# INITIALIZE---------------------------------------------------------------

# Packages
library(stringr)
library(dplyr)

# Load data

setwd("C:/Users/Marc/OneDrive/Escritorio/Marc/Visualitzacio_dades/PR")
nba <- read.csv("all_seasons.csv", header = TRUE)

# First look
str(nba)
head(nba, 10)
tail(nba, 10)
summary(nba)


# NORMALIZE VARIABLE COUNTRY-------------------------------------------------------------

table(nba$country)

# Normalize coutries
nba$country <- str_replace_all(nba$country, c("Great Britain" = "England", 
                                              "United Kingdom" = "England"))
nba$country <- str_replace(nba$country, "DRC", "Democratic Republic of the Congo")
nba$country[nba$country == "Sudan (UK)"] <- "Sudan"
nba$country[nba$country == "Bosnia & Herzegovina"] <- "Bosnia and Herzegovina"
nba$country[nba$country == "Bosnia"] <- "Bosnia and Herzegovina"
nba$country <- str_replace(nba$country, "U.S. Virgin Islands", "US Virgin Islands")
nba$country <- str_replace(nba$country, "USSR", "Russia")
# Split congo obs
congo <- nba[nba$country == "Congo", ]
n3 <- nrow(congo)
rdc <- round(2/3*n3)
rc <- n3 - rdc
set.seed(33)
index2 <- sample(n3, n3)
congo$country[index2[1:rdc]] <- "Democratic Republic of the Congo"
congo$country[index2[(rdc +1):n3]] <- "Republic of the Congo"
nba[nba$country == "Congo", ] <- congo 
# Split Serbia and Montegro obs
sim <- nba[nba$country == "Serbia and Montenegro", ]
n <- nrow(sim)
s_c <- round(2/3*n)
m_c <- n - s_c
set.seed(33)
index <- sample(n, n)
sim$country[index[1:s_c]] <- "Serbia"
sim$country[index[(s_c +1):n]] <- "Montenegro"
nba[nba$country == "Serbia and Montenegro", ] <- sim
# Split yugoslavia obs
yugoslavia <- nba[nba$country == "Yugoslavia", ]
n2 <- nrow(yugoslavia)
countries <- c("Bosnia and Herzegovina", "Croatia", "Macedonia", "Montenegro", 
               "Serbia", "Slovenia")
set.seed(33)
yugoslavia$country <- sample(countries, n2, replace = TRUE)
nba[nba$country == "Yugoslavia", ] <- yugoslavia

# Check countries
table(nba$country)

# CREATE VARIABLES ------------------------------------------------------------
# set foreign field
nba$foreign <- 1L
nba$foreign[nba$country == "USA"] <- 0L

# check variable college
sort(unique(nba$college))
# set university field
nba$university <- 1L
nba$university[nba$college == "None"] <- 0L

# CONVERT VARIABLES TO FACTOR-------------------------------------------------
# variable season
nba$season <- as.factor(nba$season)
# variable foreign
nba$foreign <- as.factor(nba$foreign)
# variable university
nba$university <- as.factor(nba$university)
# variable X
nba$X <- as.factor(nba$X)

# CHANGE DECIMALS------------------------------------------------------------

nba <- nba %>% mutate(across(where(is.numeric), ~ format(., decimal.mark = ",")))


# check results
str(nba)



# save outputdata
write.csv(nba, "nba.csv", row.names = FALSE)


# CREATE DF EVOLUTION----------------------------------------------------------

nba_evo <- nba %>% group_by(season) %>% 
  summarize(across(where(is.numeric), mean), n_foreign = sum(foreign == 1), 
            n_no_uni = sum(university == 0))
nba_evo <- nba_evo %>%
  mutate(across(where(is.numeric), ~ format(round(., 3), decimal.mark = ",")))


# check results
str(nba_evo)
head(nba_evo, 10)
tail(nba_evo, 10)

# Save output data
write.csv(nba_evo, "nba_evo.csv", row.names = FALSE)


# CREATE FOREIGN------------------------------------------------------------

nba_country <- nba %>% select (player_name, country) %>% distinct() %>% 
  filter (country != "USA")


# check results
table(nba_country$country)

# Save output data
write.csv(nba_country, "nba_country.csv", row.names = FALSE)
