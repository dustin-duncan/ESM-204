# npv and SCC
# Mauricio Collado

# erase
rm(list = ls(all = TRUE)) 

# packages
library(tidyverse)
library(purrr) # to use map

# read
warming_data <- read.csv("esm204_week8.csv")

# npv function

# function
npv = function(discount, value, time) {
  result = value / (1 + discount)**time
  return(result)
}

# lets calculate npv for each year at a discount rate of 0.02
damages_PV <- warming_data %>% 
  mutate(pv = npv(0.05, loss, year-2015))

# lets graph it
P1 = ggplot(data=damages_PV) +
  geom_line(aes(x=year,y=pv))
P1

# what is the scc?

SCC_simplecase <- damages_PV  %>%
  mutate(SCC = sum(pv, na.rm=TRUE))

################SENSITIVITY

r <-  seq(0.01,0.08,0.01)

### METHOD 1 ################################################################

damages_PV3 <- data.frame()

# Loop over the discount rates
for (rate in r) {
  # Compute the NPV for each discount rate
  pv <- npv(rate, damages_PV$loss, damages_PV$year - 2015)
  
  # Create a temporary data frame to hold the results for the current discount rate
  temp_df <- data.frame(
    r = rate,
    pv = pv,
    year = damages_PV$year
  )
  
  # Append the temporary data frame to the main results data frame
  damages_PV3 <- rbind(damages_PV3, temp_df)
}

# calculate SCC per each discount rate

SCC <- damages_PV3 %>%
  group_by(r)%>%
  summarise(SCC = sum(pv, na.rm=TRUE))

# graph

sensitivity_graph = ggplot(data=SCC) +
  geom_line(aes(x=r,y=SCC)) +
  labs(x = "Discount rate", y = "Social cost of carbon ($ per ton)")

sensitivity_graph 


### METHOD 2 ################################################################

# Check the cheatsheet of purr that i uploaded to Canvas

# Create a list of arguments for pmap
args_list <- list(discount = r, value = list(damages_PV$loss), time = list(damages_PV$year - 2015))

# Apply npv using pmap and bind columns and rows
damages_PV2 <- pmap(args_list, ~ data.frame(r = ..1, pv = npv(..1, ..2, ..3), year = damages_PV$year))

# Bind rows to combine all data frames into one
damages_PV2 <- bind_rows(damages_PV2)

# calculate SCC per each discount rate

SCC_2 <- damages_PV2 %>%
  group_by(r)%>%
  summarise(SCC = sum(pv, na.rm=TRUE))

# graph

sensitivity_graph_2 = ggplot(data=SCC_2) +
  geom_line(aes(x=r,y=SCC)) +
  labs(x = "Discount rate", y = "Social cost of carbon ($ per ton)")

sensitivity_graph_2
