library(readr)
library(dplyr)
library(scales)
library(tidyverse)
library(lubridate)
library(ggthemes)

# Loading the number of births, only keeping important information for the study : sex, birth weight and birth month

#births2018 <-
#  read_fwf("Task3/Nat2018PublicUS.c20190509.r20190717.txt",
#           fwf_positions(start = c(475,504,13,9),
#                         end = c(475,507,14,12),
#                         col_names = c("Sex","Weight","Month","Year")
#           )
#  )

#births2017 <-
#  read_fwf("Task3/Nat2017PublicUS.c20180516.r20180808.txt",
#           fwf_positions(start = c(475,504,13,9),
#                         end = c(475,507,14,12),
#                         col_names = c("Sex","Weight","Month","Year")
#           )
#  )

# Merging the two
full <- bind_rows(births2017, births2018)
save("full", file="Task3/fullbirths.RData")

# Summarizing for sex and year for comparison
compsy <- full %>% group_by(Sex, Year) %>% summarize("Number" = n())
compsy <- compsy %>% group_by(Year) %>% mutate(Total=sum(Number), Ratio=Number*100/Total)
compsy$Year <- as.character(compsy$Year)

# Plot it, for betteer visualization
ggplot(compsy, aes(x = Year, y = Ratio, fill = Sex)) +
  geom_bar(stat = "identity", width = .6) +
  coord_flip() +
  ggtitle("Proportion of births of boys and girls for 2017 and 2018") +
  ylab("Percentage") +
  theme(plot.title = element_text(hjust = .5), 
        axis.ticks = element_blank()) + 
  scale_fill_brewer(palette = "Purples") +
  theme_hc()

# Average birth weight (excluding not informed weight || Weight == 9999)
full$Weight <- as.numeric(full$Weight)
avbirweight <- mean(full$Weight[full$Weight!=9999])

#Density plot - Weight by sex in 2018
births2018$Weight <- as.numeric(births2018$Weight)
ggplot(births2018, aes(x=round(Weight, digits=1), fill=Sex)) + 
  geom_density(alpha=0.4) +
  ggtitle("Density of birth weight per sex in 2018") +
  xlab("Weight (rounded to the first digit)") +
  ylab("Density") +
  theme_hc() +
  scale_fill_brewer(palette = "Purples") +
  scale_x_log10()

#Does the average birth weight vary by month and year?
abwmy <- full %>% group_by(Year, Month) %>% summarize("Average_Birth_Weight" = mean(Weight))
abwmy$Month <- as.numeric(abwmy$Month)
abwmy$Year <- as.character(abwmy$Year)
ggplot(abwmy, aes(x=Month, y=Average_Birth_Weight, color=Year)) +
  geom_line(size=2) +
  ggtitle("Average birth weight per month") +
  xlab("Month") +
  ylab("Average Birth Weight") +
  scale_color_brewer(palette = "Purples") +
  theme_hc()