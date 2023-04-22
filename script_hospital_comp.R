


#housekeeping

library(tidyverse)
library(h2o)


# read in data
df <- read_csv('hospital_readmissions.csv')
head(df)
colnames(df)

# most common diagnosis by age group ====

# table

df %>%
  filter(diag_1 != 'Missing') %>% 
  select(age,diag_1) %>%
  group_by(age,diag_1) %>% 
  summarise(diag_count = length(diag_1)) %>% 
  ungroup() %>% 
  group_by(age) %>% 
  mutate(diag_share = round(diag_count/sum(diag_count),3 ) ) %>% 
  rename(Age = age,
         `Primary Diagnosis` = diag_1,
         `Diagnosis Share` = diag_share) %>% 
  group_by(Age) %>% 
  filter(`Diagnosis Share` == max(`Diagnosis Share`)) %>% 
  select(-diag_count) %>% 
  mutate(`Diagnosis Share` = paste(round(`Diagnosis Share`*100),'%'))

# plot 
p<- df %>%
  filter(diag_1 != 'Missing') %>% 
  select(age,diag_1) %>%
  group_by(age,diag_1) %>% 
  summarise(diag_count = length(diag_1)) %>% 
  ungroup() %>% 
  group_by(age) %>% 
  mutate(diag_share = round(diag_count/sum(diag_count),3 ) ) %>% 
  rename(Age = age,
         `Primary Diagnosis` = diag_1,
         `Diagnosis Share` = diag_share) %>% 
  ggplot(aes(Age,`Diagnosis Share`, fill = `Primary Diagnosis`, text = paste("Diagnosis Share:",paste(`Diagnosis Share`*100,'%'))))+geom_bar(stat = 'identity')+theme_minimal()+
  scale_y_continuous(labels = scales::percent)+ ggtitle('Shares of Primary Diagnoses by Age')


plotly::ggplotly(p, tooltip= c('x','text','fill')) 




# combine with the other diagnoses
# make a neighbor category: most likely pairing across the three columns


# Load the circlize library
library(circlize)

# Make the circular plot
interaction_diag<- df %>% select(diag_1,diag_2,diag_3) %>% 
  filter(diag_1 != 'Missing',
         diag_2!= 'Missing',
         diag_3!= 'Missing') %>% sample_n(1000)

chordDiagram(interaction_diag, transparency = 0.5)




# what diagnosis have the highests readmission rate?



# Some doctors believe diabetes might play a central role in readmission. =====
# Explore the effect of a diabetes diagnosis on readmissions rates


# 
df %>%
  filter(diag_1 != 'Missing') %>% 
  mutate(readmitted = ifelse(readmitted == 'yes', 1,0)) %>% 
  group_by(diag_1) %>% 
  summarise(readmit_perc = mean(readmitted)) %>% 
  arrange(readmit_perc %>% desc()) %>% 
  rename(`Primary Diagnosis` = 1, `Readmittance Rate` = 2)


df$readmitted %>% unique()



df_log_reg<- df %>% 
  select(readmitted,age,diag_1, diag_2, diag_3, glucose_test,
         diabetes_med, A1Ctest, medical_specialty, n_lab_procedures,n_medications, change) %>% 
  mutate(readmitted = as.factor(readmitted),
         age = as.factor(age),
         diag_1 = as.factor(diag_1),
         diag_2 = as.factor(diag_2),
         diag_3 = as.factor(diag_3),
         glucose_test = as.factor(glucose_test),
         diabetes_med= as.factor(diabetes_med),
         A1Ctest= as.factor(A1Ctest),
         medical_specialty= as.factor(medical_specialty),
         change = as.factor(change),
         diag_1_treat = ifelse(diag_1 == 'Diabetes',1,0 ),
         diag_2_treat = ifelse(diag_2 == 'Diabetes',1,0 ),
         diag_3_treat = ifelse(diag_3 == 'Diabetes',1,0 ),
         medical_specialty_treat = ifelse(medical_specialty %in% c('Emergency/Trauma ','Family/GeneralPractice'),1,0)) 


df_log_reg$medical_specialty_treat %>% unique()

# with the raw data

fit1<- glm(data = df_log_reg, readmitted ~ diag_1,
    family = 'binomial')

fit2<-glm(data = df_log_reg, readmitted ~ diag_1 + glucose_test,
    family = 'binomial')

fit3<-glm(data = df_log_reg, readmitted ~ diag_1 + glucose_test + diabetes_med,
    family = 'binomial')

fit4<-glm(data = df_log_reg, readmitted ~ diag_1 + glucose_test + diabetes_med + A1Ctest + medical_specialty,
    family = 'binomial')

fit5<-glm(data = df_log_reg, readmitted ~ diag_1 + glucose_test + diabetes_med + A1Ctest + medical_specialty + change,
    family = 'binomial')

fit6<-glm(data = df_log_reg, readmitted ~ diag_1 + glucose_test + diabetes_med + A1Ctest + medical_specialty + change + age,
          family = 'binomial')

stargazer::stargazer(fit1,fit2,fit3,fit4,fit5,fit6, type = 'text')


# data is cleaned up

# make dummy variables
# rename the vector names


fit1<- glm(data = df_log_reg, readmitted ~ diag_1_treat,
           family = 'binomial')

fit2<-glm(data = df_log_reg, readmitted ~ diag_1_treat + glucose_test,
          family = 'binomial')

fit3<-glm(data = df_log_reg, readmitted ~ diag_1_treat + glucose_test + diabetes_med,
          family = 'binomial')

fit4<-glm(data = df_log_reg, readmitted ~ diag_1_treat + glucose_test + diabetes_med + A1Ctest + medical_specialty_treat,
          family = 'binomial')

fit5<-glm(data = df_log_reg, readmitted ~ diag_1_treat + glucose_test + diabetes_med + A1Ctest + medical_specialty_treat + change,
          family = 'binomial')


fit6<-glm(data = df_log_reg, readmitted ~ diag_1_treat + glucose_test + diabetes_med + A1Ctest + medical_specialty_treat + change+ age,
          family = 'binomial')

fit7<-glm(data = df_log_reg, readmitted ~ diag_1_treat+diag_2_treat+diag_3_treat + glucose_test + diabetes_med + A1Ctest + medical_specialty_treat + change+ age,
          family = 'binomial')



fit8 <- glm(data = df_h2o,readmitted ~ diag_1+ diag_2 + diag_3+ age + time_in_hospital+ time_in_hospital_sq + 
      n_procedures+ 
      n_medications*n_medications_dummy+
      n_outpatient +
      n_inpatient + n_inpatient_sq + 
      n_emergency*n_emergency_dummy+
      medical_specialty+
      glucose_test+
      A1Ctest+
      change+
      diabetes_med, family = 'binomial' )

stargazer::stargazer(fit1,fit2,fit3,fit4,fit5,fit6,fit7,fit8, type = 'text')







# then get the marginal effect of diabetes, or interpret it correctly


# Q 3 ======


df_h2o<- df %>% 
  select(readmitted,age,diag_1,diag_2, diag_3,glucose_test,
         diabetes_med,diabetes_med,A1Ctest,medical_specialty,
         change) %>% 
  mutate(readmitted = as.factor(readmitted),
         age = as.factor(age),
         diag_1 = as.factor(diag_1),
         diag_2 = as.factor(diag_2),
         diag_3 = as.factor(diag_3),
         glucose_test = as.factor(glucose_test),
         diabetes_med= as.factor(diabetes_med),
         A1Ctest= as.factor(A1Ctest),
         medical_specialty= as.factor(medical_specialty),
         change = as.factor(change))


df_h2o<- df_fit %>% 
  filter(diag_1 != 'Missing',
         diag_2 != 'Missing',
         diag_3 != 'Missing') %>% 
  mutate(readmitted = as.factor(readmitted),
         time_in_hospital_sq = time_in_hospital^2,
         n_medications_dummy = ifelse(n_medications < 50,1,0),
         n_medications_interaction = n_medications*n_medications_dummy,
         n_inpatient_sq = n_inpatient^2,
         n_emergency_dummy = ifelse(n_emergency < 25,1,0),
         n_emergency_interaction = n_emergency*n_emergency_dummy,
         diag_1_diab_med_interaction = paste(diabetes_med,diag_1),
         age = as.factor(age),
         diag_1 = as.factor(diag_1),
         diag_2 = as.factor(diag_2),
         diag_3 = as.factor(diag_3),
         glucose_test = as.factor(glucose_test),
         diabetes_med= as.factor(diabetes_med),
         A1Ctest= as.factor(A1Ctest),
         medical_specialty= as.factor(medical_specialty),
         change = as.factor(change),
         diag_1_diab_med_interaction = paste(diabetes_med,diag_1),
         diag_2_diab_med_interaction = paste(diabetes_med,diag_2),
         diag_3_diab_med_interaction = paste(diabetes_med,diag_3),
         unique_3_diags = paste(diag_1,diag_2,diag_3)) %>% 
  select(readmitted, diag_1,diag_2,diag_3,  age, time_in_hospital, time_in_hospital_sq,
      n_procedures,n_medications, n_medications_dummy, n_medications_interaction,
      n_outpatient, n_inpatient, n_inpatient_sq , n_emergency , n_emergency_dummy)












# do it through H20

#n_inpatient, n_outpatient, n_emergency, diabetes_med, diag_1

localH2O = h2o.init()

# go to localhost:54321 to see H20 in action




ind<- sample(c(T,F),nrow(df_h2o),replace = T,prob = c(0.7,0.3))

train<- df_h2o[ind,]
test<- df_h2o[! ind,]

#test_clean<- test %>% 
# select(Pclass,Sex,Age,SibSp,Parch,Fare,Embarked) %>% 
#  drop_na() 




#this sends to H20
train.hex <- as.h2o(train, destination_frame = 'train.hex')
validate.hex <- as.h2o(test, destination_frame = 'validate.hex')

# build up the model
# we need response and predictor columns

response <- 'readmitted'
predictors<- colnames(df_h2o)
predictors <- predictors[! predictors %in% response]

# run an auto ml model
# this will run multiple ML models and then choose the best one for you

model<- h2o.automl(x = predictors,
                   y = response,
                   training_frame = train.hex,
                   validation_frame = validate.hex,
                   max_runtime_secs = 360 # default is one hour 
)

#record the leading model AUC in the dataset
leader <- model@leader
# click on view
View(leader)
# look at model id, then look it up in 'Models / All Models'
auc = h20.auc(leader, train = F, xval = T) # sometimes will not run because there is no auc

h2o.explain(leader, validate.hex) # confusion matrix
h2o.model_correlation_heatmap(model, validate.hex)
h2o.varimp_heatmap(model)
h2o.pd_multi_plot(model, validate.hex, 'Age')
h2o.shap_summary_plot(leader, validate.hex)
h2o.ice_plot(leader, validate.hex, "Age")

# right now model is about 65%, those are rookie numbers
# look into feature engineering





# 









