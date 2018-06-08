## Analysis used to supplement assignment SA2, English 121
## Author: Pierce Kelaita
## Date: 5/20/2018

library(dplyr)
library(ggplot2)
library(RColorBrewer)

# Import State data
state.data <- data.frame(
  read.csv("data/State-Breakdown.csv"),
  stringsAsFactors = FALSE)

# Analyze dataset to find missing values
ref <- state.data %>%
  filter(State == "Washington") %>%
  rename(Per = !!names(.[7]))
t <- ref %>% filter(Per != "")
rm(t, ref)
ot.amt <- 914 # (These two values were found manually) 
ot.per <- 5.1 

# Standardize Washington data
wa.per <- state.data %>%
  rename(Change = !!names(.[6]),
    Per = !!names(.[7])) %>%  # get rid of long column names
  filter(State == "Washington",
         Per != "") %>%
  select(ID, State, Variable, X2013, Per) %>%
  mutate(ID = c("ci",
                "cf",
                "fa",
                "yo",
                "un",
                "ve"),
         X2013 = as.numeric(gsub(",","", X2013)),
         Per = as.numeric(gsub("%", "", Per)),
         Variable = sapply(Variable, as.character)
         ) %>%
  replace(., is.na(.), "") %>%
  rbind(list( # assumes missing data is non-chronic individuals
    "in", "Washington", "Individuals", ot.amt, ot.per)) %>%
  arrange(ID)
wa.per[7,3] <- "Unaccompanied children"

total <- sum(wa.per$X2013)

# Import National data
na.data <- read.csv("data/National.csv")

suppressWarnings( # NA values OK when adding rows

# Standardize National data
na.per <- data.frame(sapply(na.data[9,], as.character),
                       stringsAsFactors = FALSE) %>%
  rename(Stat = !!names(.[1])) %>%
  mutate(Stat = as.numeric(gsub("%", "", Stat)),
         Group = rownames(.)) %>%
  
  # get rid of irrelavant rows
  filter(!grepl("Percent", Group)) %>%
  mutate(index = c(1:13)) %>%
  filter(!(index %in% c(1,5,6,7))) %>%
  
  # Calculate relative percentages
  rbind( as.numeric(.[1,]) -
           as.numeric(.[8,]) -
           as.numeric(.[9,])) %>%
  replace(., is.na(.), "N_VY") %>%
  mutate(Per_1 = 100 * Stat / Stat[Group == "Overall"],
         Per_2 = Per_1 * (Per_1[Group == "Sheltered"] / 100),
         Per_3 = Per_2 * (Per_1[Group == "N_VY"] / 100),
         Per = c(
           Per_1[1],NA,Per_1[3],
           Per_3[4:7],   # assumes groups are spread evenly
           Per_2[8:9],NA # accross population clusters
         )) %>%
  
  # Standardize
  filter(!is.na(Per), index != 2) %>%
  mutate(ID = c("un",
                "in",
                "ci",
                "fa",
                "cf",
                "ve",
                "yo")) %>%
  select(ID, Per) %>%
  arrange(ID)
)

# Join the two tables

all <- left_join(wa.per, na.per, by = "ID") %>%
  rename(Per.wa = Per.x,
         Per.us = Per.y) %>%
  mutate(Diff = Per.wa - Per.us) %>%
  select(Variable, Per.wa, Per.us, Diff)

# Create two different visualizations

# Bar plot relating Seattle data to US data
par(mar=c(5,14.5,4,0))
g1 <- barplot(all$Diff,
              names.arg = all$Variable,
              col = ifelse(all$Diff > 0,
                           "green",
                           "red"),
              las=2,
              xlim = c(-35,35),
              horiz = TRUE)
text(x = all$Diff, y = g1,
     label = paste0(
       ifelse(all$Diff > 0, "+ ", "- "),
       format(round(abs(all$Diff), 2),
              nsmall = 2),
       "%"),
     pos = ifelse(all$Diff > 0, 4, 2),
     col = ifelse(all$Diff > 0, "black", "red"),
     cex = .7, font=4
)
title(
  main = paste0(
    "Breakdown of the Homeless Populations\n",
    "in Seattle vs. the US"
  ))
title(ylab="Homeless subgroup",
      line=13, font = 4)
title(xlab="% Difference Seattle vs US",
      line=3.3, font = 4)

# Seattle Pie Chart
par(mar=c(0,11,3,13))
slices <- wa.per %>% arrange(Per)
g2 <- pie(slices$Per,
          labels = paste0(
            slices$Variable, " (",
            slices$Per, " %)"),
          main="Seattle's Homeless Population",
          col=brewer.pal(
            n = nrow(slices),
            name = "Accent"))


## Source: https://data.world/alice-c/homelessness-in-usa