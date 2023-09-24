install.packages("ggplot2")
install.packages("dplyr")
install.packages("e1071")
install.packages("stargazer")
install.packages("MonteCarlo")
install.packages("data.table")
install.packages("fixest")
install.packages("magrittr")
install.packages("stats4")
library(ggplot2)
library(dplyr)
library(e1071)
library(stargazer)
library(MonteCarlo)
library(data.table)
library(fixest)
library(magrittr)
library(stats4)

df_new_gini <- read.csv("C:/Users/DELL/Downloads/ECO/Dataset A2/Gini_New.csv")
df_new_gini$State <- apply(df_new_gini["State"],2,tolower)
View(df_new_gini)

#Q1

df_prev <- read.csv("C:/Users/DELL/Downloads/ECO/Dataset A2/group_18.csv")
View(df_prev)

df_agg_exp_data <- read.csv("C:/Users/DELL/Downloads/ECO/Dataset A2/Aggregate Expenditure.csv")

df_agg_exp_data$State <- apply(df_agg_exp_data["State"],2,tolower)
View(df_agg_exp_data)


df_cap_exp_data <- read.csv("C:/Users/DELL/Downloads/ECO/Dataset A2/Capital Expenditure.csv")
df_cap_exp_data$State <- apply(df_cap_exp_data["State"],2,tolower)
View(df_cap_exp_data)


df_election_margin <- read.csv("C:/Users/DELL/Downloads/ECO/Dataset A2/Margin Of Election Victory - Sheet3.csv")
df_election_margin$State <- apply(df_election_margin["State"],2,tolower)
View(df_election_margin)


df_merged_data1 <- merge(df_election_margin,df_cap_exp_data, by="State")
View(df_merged_data1)
df_merged_data1

df_merged_data2 <- merge(df_agg_exp_data,df_merged_data1, by="State")
View(df_merged_data2)


df_finalData <- merge(df_merged_data2,df_prev,by="State")
View(df_finalData)

colnames(df_finalData)[2] <- "Avg Aggregate Expenditure"
colnames(df_finalData)[3] <- "District"
colnames(df_finalData)[4] <- "Victory Margin Average"
colnames(df_finalData)[5] <- "Avg Capital Expenditure"

View(df_finalData)

SDP = df_finalData$SDP
GWL = df_finalData$Ground_water_level
gini = df_finalData$gini
electionMargin = df_finalData$`Victory Margin Average`
capital_exp = df_finalData$`Avg Capital Expenditure`
agg_exp = df_finalData$`Avg Aggregate Expenditure`



df_finalData$SDP <- as.numeric(df_finalData$SDP)
df_finalData$gini <- as.numeric(df_finalData$gini)

df_finalData$Yearcode <- as.numeric(df_finalData$Yearcode)
df_finalData$`Avg Aggregate Expenditure`<-as.numeric(df_finalData$`Avg Aggregate Expenditure`)
df_finalData$`Victory Margin Average`<-as.numeric(df_finalData$`Victory Margin Average`)
df_finalData$`Avg Capital Expenditure`<-as.numeric(df_finalData$`Avg Capital Expenditure`)

#Q2

df_q2_data <- merge(df_finalData,df_new_gini,by="State")
View(df_q2_data)

df_q2_data$newgini <- as.numeric(df_q2_data$newgini)
df_q2_data$Yearcode <- as.numeric(df_q2_data$Yearcode)
df_q2_data$`Avg Aggregate Expenditure`<-as.numeric(df_q2_data$`Avg Aggregate Expenditure`)
df_q2_data$`Victory Margin Average`<-as.numeric(df_q2_data$`Victory Margin Average`)
df_q2_data$`Avg Capital Expenditure`<-as.numeric(df_q2_data$`Avg Capital Expenditure`)
df_q2_data$SDP <- as.numeric(df_q2_data$SDP)
df_q2_data$gini <- as.numeric(df_q2_data$gini)

SDP <- ifelse(SDP == "-", 0, SDP)
SDP <- as.numeric(SDP)

SDP_2 = SDP ^ 2
SDP_3 = SDP ^ 3

model_2 <- lm(GWL~SDP + SDP_2 + SDP_3 + electionMargin + capital_exp+agg_exp + gini)

summary(model_2)

SDP = df_q2_data$SDP
GWL = df_q2_data$Ground_water_level
gini = df_q2_data$gini
electionMargin = df_q2_data$`Victory Margin Average`
capital_exp = df_q2_data$`Avg Capital Expenditure`
agg_exp = df_q2_data$`Avg Aggregate Expenditure`
newgini = df_q2_data$newgini

SDP <- ifelse(SDP == "-", 0, SDP)
SDP <- as.numeric(SDP)

SDP_2 = SDP ^ 2
SDP_3 = SDP ^ 3

model_3 <- lm(GWL~SDP + SDP_2 + SDP_3 + electionMargin + capital_exp+agg_exp + newgini)

summary(model_3)

#6


model_2<-glm(GWL~SDP+SDP_2+SDP_3+gini+electionMargin+capital_exp+agg_exp,family=poisson(link="identity"))
summary(model_2)

model_3<-glm(GWL~SDP+SDP_2+SDP_3+gini+electionMargin+capital_exp+agg_exp,family=gaussian(link="identity"))
summary(model_3)

model_4<-glm(GWL~SDP+SDP_2+SDP_3+gini+electionMargin+capital_exp+agg_exp,family=quasi(link="identity"))
summary(model_4)


#5
df_prev$SDP <- as.numeric(df_prev$SDP)
df_prev$gini <- as.numeric(df_prev$gini)
df_prev$Yearcode <- as.numeric(df_prev$Yearcode)


df_prev<-na.omit(df_prev)

SDP = df_prev$SDP
GWL = df_prev$Ground_water_level

total<-nrow(df_prev)
model_1 <- lm(GWL~SDP)
set.seed(1)
summary(model_1)
coef_summary <- summary(model_1)$coefficients

# Extract the SDP and Intercept estimates from the coefficient summary

SDP_estimate <- coef_summary["SDP", "Estimate"]
Intercept_estimate <- coef_summary["(Intercept)", "Estimate"]
SDP_estimate
Intercept_estimate


ols_estimates<-function(n,beta_0,beta_1){
  i<-floor(runif(100,1,total))
  X_i<-df[i,]$SDP
  Y_i<-df[i,]$Ground_water_level
  U_i<-Y_i-beta_0-(beta_1*X_i)
  
  data_i<-data.table(Y=Y_i,X=X_i)
  ols_i<-feols(data=data_i,Y~X)
  slope<-ols_i$coefficients[2]
  return(list("beta_1"=unname(slope)))
  
}

X_i <- as.numeric(X_i)

ols_estimates(n=100,beta_0=Intercept_estimate,beta_1=SDP_estimate)

n_grid<-c(100,500,1000,4000)
beta0_grid<-c(Intercept_estimate)
beta1_grid<-c(SDP_estimate)

param_list<-list("n"=n_grid,"beta_0"=beta0_grid,"beta_1"=beta1_grid)

Monte_Carlo<-MonteCarlo(func=ols_estimates,
                        nrep=100,
                        param_list=param_list,
                        ncpus=1,
                        time_n_test = FALSE)
res<-Monte_Carlo$results
res1<-res$beta_1

n_100<-res1[1,,,]
n_500<-res1[2,,,]
n_1000<-res1[3,,,]
n_4000<-res1[4,,,]


hist(n_100,xlim=c(-5e-06,1e-05))
sd(n_100)
mean(n_100)
hist(n_500,xlim=c(-5e-06,1e-05))
sd(n_500)
mean(n_500)
hist(n_1000,xlim=c(-5e-06,1e-05))
sd(n_1000)
mean(n_1000)
hist(n_4000,xlim=c(-5e-06,1e-05))
sd(n_4000)
mean(n_4000)


df = df_prev



df$SDP_2 = df$SDP ^ 2
df$SDP_3 = df$SDP ^ 3


# Defining the lists of states in each zone
Zone_North <- c("uttar pradesh", "rajasthan", "haryana", "punjab", "uttarakhand", "himachal pradesh", "delhi", "chandigarh")
Zone_South <- c("telangana", "kerala", "andhra pradesh", "karnataka", "tamil nadu", "puducherry", "goa")
Zone_East <- c("arunachal pradesh", "bihar", "assam", "west bengal", "tripura", "meghalaya", "nagaland")
Zone_West <- c("maharashtra", "gujarat")
Zone_Central <- c("madhya pradesh", "chhattisgarh", "jharkhand")

# Add a new column to indicate the zone of each state
df$zone <- NA
df$zone[df$State %in% Zone_North] <- "North"
df$zone[df$State %in% Zone_South] <- "South"
df$zone[df$State %in% Zone_West] <- "West"
df$zone[df$State %in% Zone_East] <- "East"
df$zone[df$State %in% Zone_Central] <- "Central"

# Subset data by zones
data_South <- df %>% filter(zone == "South")
data_North <- df %>% filter(zone == "North")
data_East <- df %>% filter(zone == "East")
data_West <- df %>% filter(zone == "West")
data_Central <- df %>% filter(zone == "Central")

# Remove rows with missing values
data_North <- na.omit(data_North)
data_South <- na.omit(data_South)
data_East <- na.omit(data_East)
data_West <- na.omit(data_West)
data_Central <- na.omit(data_Central)

# Do t-tests to compare GWL between different pairs of zones
t.test(data_North$Ground_water_level, data_South$Ground_water_level)
t.test(data_East$Ground_water_level, data_West$Ground_water_level)
t.test(data_North$Ground_water_level, data_Central$Ground_water_level)
t.test(data_Central$Ground_water_level, data_South$Ground_water_level)
t.test(data_North$Ground_water_level, data_East$Ground_water_level)
t.test(data_North$Ground_water_level, data_West$Ground_water_level)
t.test(data_South$Ground_water_level, data_East$Ground_water_level)
t.test(data_South$Ground_water_level, data_West$Ground_water_level)

# Chow test
library(strucchange)

# Perform Chow tests to compare regression coefficients between different pairs of zones
data_NS <- rbind(data_North, data_South)
sctest(Ground_water_level ~ SDP + SDP_2 + SDP_3 + gini, data = data_NS, type = "Chow")

data_EW <- rbind(data_East, data_West)
sctest(Ground_water_level ~ SDP + SDP_2 + SDP_3 + gini, data = data_EW, type = "Chow")

data_NC <- rbind(data_North, data_Central)
sctest(Ground_water_level ~ SDP + SDP_2 + SDP_3 + gini, data = data_NC, type = "Chow")

data_SC <- rbind(data_South, data_Central)
sctest(Ground_water_level ~ SDP + SDP_2 + SDP_3 + gini, data = data_SC, type = "Chow")

data_NE <- rbind(data_North, data_East)
sctest(Ground_water_level ~ SDP + SDP_2 + SDP_3 + gini, data = data_NE, type = "Chow")

data_SW <- rbind(data_South, data_West)
sctest(Ground_water_level ~ SDP + SDP_2 + SDP_3 + gini, data = data_SW, type = "Chow")

data_EC <- rbind(data_East, data_Central)
sctest(Ground_water_level ~ SDP + SDP_2 + SDP_3 + gini, data = data_EC, type = "Chow")

data_WC <- rbind(data_West, data_Central)
sctest(Ground_water_level ~ SDP + SDP_2 + SDP_3 + gini, data = data_WC, type = "Chow")



##### 
# PART - 7 

library(lmtest)

model <- lm(Ground_water_level ~ SDP + zone, data = df)

# Do the Breusch-Pagan test
bp_test <- bptest(model, ~ SDP * zone, data = df)
print(bp_test)

