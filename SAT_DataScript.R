library(tidyverse)

#Read Data
SAT_Data <- read.csv("Data/sample-sat-ratings-2020-09.csv")

#select 
SAT_Data = SAT_Data %>% select(Assignee, Brand, Satisfaction, Survey.Date)

#correct the codes for Satisfaction to good and bad
SAT_Data$Satisfaction[SAT_Data[,3] == "good"] <- "Good"
SAT_Data$Satisfaction[SAT_Data[,3] == "bad"] <- "Bad"

#Set Survey.Date as date
SAT_Data$Survey.Date <- as.Date(SAT_Data$Survey.Date, format = "%Y-%m-%d")

#Count formula for smaller data sets
SAT_Count <- SAT_Data %>%
  count(Assignee, Satisfaction, Brand, Survey.Date, sort = TRUE) 

SAT_Count$TicketCount <- SAT_Count$n 
SAT_Count$n <- NULL 

TicketCount <- SAT_Data %>%
  count(Assignee, Satisfaction, sort = TRUE) 

# change sat to numeric
# SAT_Count$Satisfaction <- as.numeric(x = SAT_Count$Satisfaction)

#Total Number of tickets 

SumTickets <- sum(TicketCount[,3])

#Update counts

TicketCount$TicketCount <- TicketCount$n
TicketCount$n <- NULL

#Plots for SAT Data

SATchart <- SAT_Count %>% 
  ggplot(aes(fill = Satisfaction, y = TicketCount, x = Survey.Date)) + 
  geom_bar(position = "stack", stat = "identity") +
  labs(title = "Satisfaction per Day, August 2020", x = "Date of Survey",
       y = "Number of Tickets", caption = "Total # of Tickets = 467") +
  theme_bw() +
facet_wrap(.~ Assignee)


TicketChart <- TicketCount %>% 
  ggplot(aes(fill = Satisfaction, y = TicketCount, x = Assignee)) + 
  geom_bar(position = "stack", stat = "identity") + 
  labs(title = "Satisfaction per Assignee by Volume, August 2020", 
       x = "Assignee", 
       y = "Number of Tickets", caption = "Total # of Tickets = 467") +
  theme_bw() 

TicketChart2 <- TicketCount %>% 
  ggplot(aes(fill = Satisfaction, y = TicketCount, x = Assignee)) + 
  geom_bar(position = "fill", stat = "identity") + 
  labs(title = "Satisfaction per Assignee by Percentage, August 2020", 
       x = "Assignee", 
       y = "Number of Tickets", caption = "Total # of Tickets = 467") +
  theme_bw() 

print(SATchart)
print(TicketChart)
print(TicketChart2)
