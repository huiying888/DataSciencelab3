
#Now change dataset to churn_train
#Activity 1 – Basic Imputation Methods

library(readr)
churn <- read_csv("C:/Users/HP/OneDrive - Universiti Teknologi PETRONAS/2rd 1st/DS/LAB/Churn_Train.csv")
View(churn)
summary(churn)
churn$`Total Charges`
library(ggplot2)
library(dplyr)
library(cowplot)
summary(is.na(churn))

churn$`Total Charges`[is.na(churn$`Total Charges`)]<-mean(churn$`Total Charges`,na.rm = TRUE)

ggplot(churn, aes(`Total Charges`)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  ggtitle("Variable distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))

value_imputed<-data.frame(
  original=churn$`Total Charges`,
  imputed_zero=replace(churn$`Total Charges`,is.na(churn$`Total Charges`),0),
  imputed_mean=replace(churn$`Total Charges`,is.na(churn$`Total Charges`),mean(churn$`Total Charges`,na.rm = TRUE)),
  imputed_median=replace(churn$`Total Charges`,is.na(churn$`Total Charges`),median(churn$`Total Charges`,na.rm = TRUE))
  
  
)
value_imputed

h1<-ggplot(value_imputed, aes(x = original)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position = 
                   "identity") +
  ggtitle("Original distribution") +
  theme_classic()
h2 <- ggplot(value_imputed, aes(x = imputed_zero)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position = 
                   "identity") +
  ggtitle("Zero-imputed distribution") +
  theme_classic()
h3 <- ggplot(value_imputed, aes(x = imputed_mean)) +
  geom_histogram(fill = "#1543ad", color = "#000000", position = 
                   "identity") +
  ggtitle("Mean-imputed distribution") +
  theme_classic()
h4 <- ggplot(value_imputed, aes(x = imputed_median)) +
  geom_histogram(fill = "#ad8415", color = "#000000", position = 
                   "identity") +
  ggtitle("Median-imputed distribution") +
  theme_classic()
plot_grid(h1, h2, h3, h4, nrow = 2, ncol = 2)

library(mice)
churn_numeric <- churn %>%
  select(`Monthly Charges`,`Total Charges`)
md.pattern(churn_numeric)
mice_imputed <- data.frame(
  original = churn$`Total Charges`,
  imputed_pmm = complete(mice(churn_numeric, method = 
                                "pmm"))$`Total Charges`,
  imputed_cart = complete(mice(churn_numeric, method = 
                                 "cart"))$`Total Charges`,
  imputed_lasso = complete(mice(churn_numeric, method = 
                                  "lasso.norm"))$`Total Charges`)

mice_imputed

h1 <- ggplot(mice_imputed, aes(x = original)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position = 
                   "identity") +
  ggtitle("Original distribution") +
  theme_classic()
h2 <- ggplot(mice_imputed, aes(x = imputed_pmm)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position = 
                   "identity") +
  ggtitle("Pmm-imputed distribution") +
  theme_classic()
h3 <- ggplot(mice_imputed, aes(x = imputed_cart)) +
  geom_histogram(fill = "#1543ad", color = "#000000", position = 
                   "identity") +
  ggtitle("Cart-imputed distribution") +
  theme_classic()
h4 <- ggplot(mice_imputed, aes(x = imputed_lasso)) +
  geom_histogram(fill = "#ad8415", color = "#000000", position = 
                   "identity") +
  ggtitle("Lasso-imputed distribution") +
  theme_classic()
plot_grid(h1, h2, h3, h4, nrow = 2, ncol = 2)

#Activity 3 – Imputation with R missForest Package
library(missForest)
missForest_imputed<-data.frame(
  original=churn_numeric$`Total Charges`,
  imputed_missForest=missForest(churn_numeric)$ximp$`Total Charges`
)
missForest_imputed

h1 <- ggplot(mice_imputed, aes(x = original)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position = 
                   "identity") +
  ggtitle("Original distribution") +
  theme_classic()
h2 <- ggplot(missForest_imputed, aes(x = imputed_missForest)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position = 
                   "identity") +
  ggtitle("missForest-imputed distribution") +
  theme_classic()
plot_grid(h1, h2, nrow = 1, ncol = 2)

#Activity 4: Normalize data with scaling methods
log_scale=log(as.data.frame(churn$`Total Charges`))

library(caret)
process<-preProcess(as.data.frame(churn$`Total Charges`),method=c("range"))

norm_scale<-predict(process,as.data.frame(churn$`Total Charges`))

scale_data<-as.data.frame(scale(churn$`Total Charges`))

#Activity 5: Feature Encoding
#label encoding
#process of replacing the different levels of a categorical variable with dummy numbers
gender_encode<-ifelse(churn$Gender=="Male",1,0)
table(gender_encode)

churn_encode<-ifelse(churn$`Multiple Lines`=="Yes",1,
                     ifelse(churn$`Multiple Lines`=="No",2,
                            ifelse(churn$`Multiple Lines`=="No phone service",3,0)))
table(churn_encode)

#One hot encoding
new_dat=data.frame(churn$`Total Charges`,churn$Gender,churn$`Multiple Lines`)
summary(new_dat)

library(caret)
dmy<-dummyVars("~.",data=new_dat,fullRank = T)
dat_transformed<-data.frame(predict(dmy,newdata=new_dat))

glimpse(dat_transformed)

#Encoding Continuous(or Numeric) variables
summary(new_dat$churn..Total.Charges.)

#create a vector of cut-off points based on 1st Quarter value and 3rd Quarter values
bins<-c(-Inf,7.91,31.00,Inf)
#gives the respective names to these cut-off points
bin_names<-c("Low","Mid50","High")
# cut() function to break the vector using the cut-off points
new_dat$new_Fare<-cut(new_dat$churn..Total.Charges.,breaks=bins,labels=bin_names)

summary(new_dat$churn..Total.Charges.)
summary(new_dat$new_Fare)


churn %>%
  eda_web_report(target="Total Charges",subtitle="Churn_Train",
                 output_dir="C:/Users/HP/OneDrive - Universiti Teknologi PETRONAS/2rd 1st/DS/LAB/lab4",
                 output_file="EDA.html",theme="blue")
