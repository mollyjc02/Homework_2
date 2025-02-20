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
fig.unique <- final.hcris.data %>% group_by(year) %>% 
  filter(year >= 1999 & year <= 2017) %>%   
  summarize(unique_hospital_count=n()) %>%
  ggplot(aes(x=as.factor(year), y=unique_hospital_count, group=1)) + 
  geom_line() + 
  labs(
    x="Year",
    y="Number of Hospitals",
    title=""
  ) + theme_bw() + 
  scale_y_continuous(labels=comma,limits=c(0,6500)) + 
  theme(axis.text.x = element_text(angle=70, hjust=1))
print(fig.unique)


# 3)What is the distribution of total charges in each year? 
charge.data <- final.hcris.data %>% 
  group_by(year) %>% 
  mutate(tot_charges_low=quantile(tot_charges, probs=0.01, na.rm=TRUE), 
         tot_charges_high=quantile(tot_charges, probs=0.99, na.rm=TRUE)) %>%
  filter(tot_charges<tot_charges_high, tot_charges>tot_charges_low, 
        !is.na(tot_charges), year>1997) %>% 
  mutate(log_charge=log(tot_charges))

ggplot(charge.data, aes(x = factor(year), y = tot_charges)) +
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
## Calculate discount factor
price.data <- final.hcris.data %>%
  mutate(discount_factor=1 - tot_discounts / tot_charges,
    price_num = (ip_charges + icu_charges + ancillary_charges)*discount_factor - tot_mcare_payment,
    price_denom=tot_discharges - mcare_discharges,
    price=price_num/price_denom) %>% 
  filter(price_denom>100, !is.na(price_denom), 
    price_num>0, !is.na(price_num), 
    price<100000, 
    beds>30, !is.na(beds))


## Create a violin plot to show the distribution of estimated prices by year
ggplot(price.data, aes(x = as.factor(year), y = price)) +
  geom_violin(trim = TRUE, fill = "skyblue", color = "black") +
  labs(
    title = "Distribution of Estimated Prices by Year",
    x = "Year",
    y = "Estimated Price"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 70, hjust = 1))


# 5)Calculate the average price among penalized versus non-penalized hospitals. 
price.data <- price.data %>%
  mutate( discount_factor = 1-tot_discounts/tot_charges,
          price_num = (ip_charges + icu_charges + ancillary_charges)*discount_factor - tot_mcare_payment,
          price_denom = tot_discharges - mcare_discharges,
          price = price_num/price_denom)

final.hcris.2012 <- price.data %>% ungroup() %>%
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

## Clean by bed size and price 
bed_lower <- quantile(final.hcris.2012$beds, 0.05, na.rm = TRUE)
bed_upper <- quantile(final.hcris.2012$beds, 0.95, na.rm = TRUE) 
price_cutoffs <- quantile(final.hcris.2012$price, probs = c(0.05, 0.95), na.rm = TRUE)

final.hcris.2012 <- final.hcris.2012 %>%
filter(beds > bed_lower & beds < bed_upper) %>%
filter(price >= price_cutoffs[1] & price <= price_cutoffs[2]) %>%
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


quartile_summary <- quartile_summary %>%
  rename(
    `No Penalty` = `penalty_FALSE`, 
    `Penalty` = `penalty_TRUE`)

view(quartile_summary)


# 7)Find the average treatment effect using each of the following estimators: 
install.packages("Matching")
install.packages("MatchIt")
install.packages("WeightIt")
install.packages("cobalt")
library(Matching)
library(cobalt)
library(dplyr)


## Nearest matching neighbor with inverse variance distance based on quartiles of bed size
near.match <- Matching::Match(Y=final.hcris.2012$price,
                            Tr=final.hcris.2012$penalty,
                            X=(final.hcris.2012 %>% dplyr::select(Q1, Q2, Q3)),
                            M=1, 
                            Weight=1,
                            estimand="ATE")
summary(near.match)


## Nearest neighbor matching with Mahalanobis distance based on quartiles of bed size
mal.match <- Matching::Match(Y=final.hcris.2012$price,
                            Tr=final.hcris.2012$penalty,
                            X=(final.hcris.2012 %>% dplyr::select(Q1, Q2, Q3)),
                            M=1, 
                            Weight=2,
                            estimand="ATE")
summary(mal.match)

## Inverse propensity weighting, where the propensity scores are based on quartiles of bed size
logit.model <- glm(penalty ~ Q1 + Q2 + Q3, family=binomial, data=final.hcris.2012)
ps <- fitted(logit.model)

# Calculate inverse propensity weights (IPW)
final.hcris.2012 <- final.hcris.2012 %>%
  mutate(ipw = case_when(
    penalty == 1 ~ 1 / ps,         # For treated group (penalty == 1)
    penalty == 0 ~ 1 / (1 - ps),   # For control group (penalty == 0)
    TRUE ~ NA_real_               # Handle any missing values
  ))

### Compute weighted average prices for treated (penalty == 1) and control (penalty == 0) groups
mean.t1 <- final.hcris.2012 %>% filter(penalty == 1) %>%
  dplyr::select(price, ipw) %>%
  summarize(mean_p = weighted.mean(price, w = ipw))

mean.t0 <- final.hcris.2012 %>% filter(penalty == 0) %>%
  dplyr::select(price, ipw) %>%
  summarize(mean_p = weighted.mean(price, w = ipw))

ipw.diff <- mean.t1$mean_p - mean.t0$mean_p
ipw.diff


## Simple linear regression, adjusting for quartiles of bed size using dummy variables and appropriate interactions as discussed in class 
reg.data <- final.hcris.2012 %>% ungroup() %>%
  mutate(Q1_diff = penalty * (Q1 - mean(Q1)),
         Q2_diff = penalty * (Q2 - mean(Q2)),
         Q3_diff = penalty * (Q3 - mean(Q3)))

#### Fit the regression model with quartile differences and penalty
reg <- lm(price ~ penalty + Q1 + Q2 + Q3 + 
            Q1_diff + Q2_diff + Q3_diff,
          data = reg.data)
summary(reg)

ate <- coef(reg)["penaltyTRUE"]
ate

##rm(list=c("final.hcris.2012", "final.hcris.data", "duplicate.hcris", "bed_quartiles", "bed_lower", "bed_upper", "price_cutoffs", "logit.model", "ps"))
##save.image("final_submission/Hwk2_workspace.RData")
