library(tidyverse)
library(dplyr)

#Create a new column ChargeGap as TotalCharges - (MonthlyCharges * Tenure). Handle
#any NAs appropriately.
churned_customers <- churn %>%
  ChargeGap== MonthlyCharges*Tenure

#Categorize customers based on Age:
chrun <- customer_churn %>% when(
  "Youth" : age < 25,
  "Adult" :25 < Age < 55,
  "Senior": Age > 55,
  
)
print(churn)

#Find the top 5 cities with the highest number of churned customers.
top_churn_cities <- customer_churn %>%
  filter(Churn == "Yes") %>%
  group_by(City) %>%
  summarise(Churned_Customers = n()) %>%
  arrange(desc(Churned_Customers)) %>%
  slice_head(n = 5)

print(top_churn_cities) 


#Extract only the names and cities of customers whose TotalCharges > 3000, are on a
#Month-to-Month contract, and have churned
filtered_customers <- df %>%
  filter(TotalCharges > 3000, 
         ContractType == "Month-to-Month", 
         Churn == "Yes") %>%
  select(CustomerName, City)

print(filtered_customers)

#Create a table that shows the average tenure and total revenue (sum(TotalCharges)) for
#each ContractType.
contract_summary <- df %>%
  group_by(ContractType) %>%
  summarize(
    avg_tenure = mean(Tenure, na.rm = TRUE),
    total_revenue = sum(TotalCharges, na.rm = TRUE)
  )

print(contract_summary)
