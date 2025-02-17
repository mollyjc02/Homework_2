if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate)

# 1)How many hospitals filed more than one report in the same year? 
## Show your answer as a line graph of the number of hospitals over time. 
multi_report_counts <- duplicate.hcris %>%
group_by(fyear) %>%
summarise(num_hospitals = n_distinct(provider_number))

## Plot the number of hospitals filing multiple reports by year
ggplot(multi_report_counts, aes(x = fyear, y = num_hospitals)) +
geom_line(color = "blue", size = 1) +
geom_point(color = "red", size = 2) +
labs(
title = "Hospitals Filing Multiple Reports Per Year",
x = "Year",
y = "Number of Hospitals",
caption = "Source: HCRIS Data (1996 & 2010 Versions)"
) +
theme_minimal()


# 2)Count the number of unique hospital IDs
unique_hospital_count <- final.hcris.data %>%
distinct(provider_number) %>%
nrow()

## Print the result
cat("Number of unique hospital IDs (Medicare provider numbers):", unique_hospital_count, "\n")


# 3)What is the distribution of total charges in each year? 
ggplot(final.hcris.data, aes(x = factor(year), y = tot_charges)) +
geom_violin(fill = "lightblue", color = "darkblue", alpha = 0.6) +
scale_y_log10() + # Apply log scale to handle skewed distributions
labs(
title = "Distribution of Total Charges by Year",
x = "Year",
y = "Total Charges (log scale)",
caption = "Source: HCRIS Data (1996 & 2010 Versions)"
) +
theme_minimal()


# 4)What is the distribution of estimated prices in each year?
## Step 1: Calculate discount factor
final.hcris.data <- final.hcris.data %>%
  mutate(
    discount_factor = 1 - tot_discounts / tot_charges,
    price_num = (ip_charges + icu_charges + ancillary_charges) * discount_factor - tot_mcare_payment,
    price_denom = tot_discharges - mcare_discharges,
    price = if_else(price_denom > 0, price_num / price_denom, NA_real_) # Avoid division by zero or negative denominator
  )

## Step 2: Filter out negative or invalid prices and any other outliers (e.g., top 1% of prices)
final.hcris.data <- final.hcris.data %>%
  filter(!is.na(price) & price > 0)

## Step 3: Create a violin plot to show the distribution of estimated prices by year
final.hcris.data <- final.hcris.data_clean %>%
  mutate(log_price = log(price))

ggplot(final.hcris.data, aes(x = as.factor(year), y = log_price)) +
  geom_violin(trim = TRUE, fill = "skyblue", color = "black") +
  labs(
    title = "Distribution of Estimated Prices by Year",
    x = "Year",
    y = "Estimated Price"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# 5)Calculate the average price among penalized versus non-penalized hospitals. 
final.hcris.data <- final.hcris.data %>%
  mutate( discount_factor = 1-tot_discounts/tot_charges,
          price_num = (ip_charges + icu_charges + ancillary_charges)*discount_factor - tot_mcare_payment,
          price_denom = tot_discharges - mcare_discharges,
          price = price_num/price_denom)

final.hcris.2012 <- final.hcris.data %>% ungroup() %>%
  filter(price_denom>100, !is.na(price_denom), 
         price_num>0, !is.na(price_num),
         price<100000, 
         beds>30, year==2012) %>%  #<<
  mutate( hvbp_payment = ifelse(is.na(hvbp_payment),0,hvbp_payment),
          hrrp_payment = ifelse(is.na(hrrp_payment),0,abs(hrrp_payment)), #<<
    penalty = (hvbp_payment-hrrp_payment<0)) #<<

mean.pen <- round(mean(final.hcris.2012$price[which(final.hcris.2012$penalty==1)]),2)
mean.nopen <- round(mean(final.hcris.2012$price[which(final.hcris.2012$penalty==0)]),2)
print(mean.pen)
print(mean.nopen)


# 6)Split hospitals into quartiles based on bed size. Provide a table of the average price among treated/control groups for each quartile. 
## Define penalty: HVBP + HRRP < 0
final.hcris.2012 <- final.hcris.2012 %>%
mutate(
hvbp_payment = ifelse(is.na(hvbp_payment), 0, hvbp_payment),
hrrp_payment = ifelse(is.na(hrrp_payment), 0, hrrp_payment),
penalty = (hvbp_payment + hrrp_payment) < 0
)

## Calculate bed size quartiles
bed_quartiles <- quantile(final.hcris.2012$beds, probs = c(0.25, 0.50, 0.75), na.rm = TRUE)

## Assign each hospital to a bed size quartile
final.hcris.2012 <- final.hcris.2012 %>%
mutate(
Q1 = ifelse(beds <= bed_quartiles[1] & beds > 0, 1, 0),
Q2 = ifelse(beds > bed_quartiles[1] & beds <= bed_quartiles[2], 1, 0),
Q3 = ifelse(beds > bed_quartiles[2] & beds <= bed_quartiles[3], 1, 0),
Q4 = ifelse(beds > bed_quartiles[3], 1, 0)
) 

## Calculate average prices by quartile and penalty status
quartile_summary <- final.hcris.2012 %>%
mutate(bed_quartile = case_when(
Q1 == 1 ~ "Q1",
Q2 == 1 ~ "Q2",
Q3 == 1 ~ "Q3",
Q4 == 1 ~ "Q4"
)) %>%
group_by(bed_quartile, penalty) %>%
summarise(avg_price = mean(price, na.rm = TRUE), .groups = "drop") %>%
pivot_wider(names_from = penalty, values_from = avg_price, names_prefix = "penalty_")

print(quartile_summary)


# 7)Find the average treatment effect using each of the following estimators: 
## Nearest matching neighbor with inverse variance distance based on quartiles of bed size
install.packages("Matching")
install.packages("MatchIt")
install.packages("WeightIt")

colnames(final.hcris.2012)

lp.covs <- final.hcris.2012 %>%
  select(Q1, Q2, Q3, Q4) %>%
  na.omit()

lp.vars <- final.hcris.2012 %>%
  select(price, penalty) %>%
  na.omit()

m.nn.var <- Matching::Match(Y=lp.vars$price,
                            Tr=lp.vars$penalty,
                            X=lp.covs,
                            M=1, 
                            Weight=1,
                            estimand="ATE")


v.name=data.frame(new=c("Q1", "Q2", "Q3", "Q4"))

### plot
install.packages("cobalt")
library(cobalt)


love.plot(bal.tab(m.nn.var, covs = lp.covs, treat = lp.vars$penalty), 
          threshold=0.1, 
          var.names=v.name,
          grid=FALSE, sample.names=c("Unmatched", "Matched"),
          position="top", shapes=c("circle","triangle"),
          colors=c("black","blue")) + 
  theme_bw()

## Nearest neighbor matching with Mahalanobis distance based on quartiles of bed size
m.nn.md <- Matching::Match(Y=lp.vars$price,
                           Tr=lp.vars$penalty,
                           X=lp.covs,
                           M=1,
                           Weight=2,
                           estimand="ATE")

## Inverse propensity weighting, where the propensity scores are based on quartiles of bed size
logit.model <- glm(penalty ~ Q1 + Q2 + Q3, family=binomial, data=final.hcris.2012)
ps <- fitted(logit.model)



m.nn.ps <- Matching::Match(Y=lp.vars$price,
                           Tr=lp.vars$penalty,
                           X=ps,
                           M=1,
                           estimand="ATE")

## Simple linear regression, adjusting for quartiles of bed size using dummy variables and appropriate interactions as discussed in class 
ggplot(lp.vars, aes(x=ps)) + geom_histogram() + 
  facet_wrap(~ penalty, ncol=1) +
  theme_bw()
  