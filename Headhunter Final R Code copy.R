## Task: Predict whether a current employee is looking for a new job or not
#----------------------------------------------------------------------------------------
# Install packages and import library
install.packages('tidyverse')
install.packages('Amelia')
install.packages('ggplot2')
library(ggplot2)
library(Amelia)
library(tidyverse)
#----------------------------------------------------------------------------------------
## Import dataset
getwd() #check current directory

# read data and replace "" with "NA"
df = read.csv('Headhunter Final CSV Dataset.csv',na.strings = c("", "NA")) 

# take out the 'employee_id' and 'city' variables
df = df[,3:14]


#----------------------------------------------------------------------------------------
## 1.Basic Information
dim(df) # number of rows/columns
names(df) # names of all the columns
str(df) # compact description of the dataset
        # 4 continuous variables, 10 categorical variables
# Take a closer look at the continuous variables
summary(df$city_development_index)
summary(df$training_hours)

#----------------------------------------------------------------------------------------
## 2.Preparing data for EDA
# Fix company_size and plot
unique(df$company_size)

small_cap = c('<10', '10/49','50-99')
medium_cap = c('100-500','500-999')
large_cap = c('1000-4999','5000-9999','10000+')
all_size = c(small_cap,medium_cap,large_cap)


for (i in all_size){
        if (i %in% small_cap){
                df$company_size[df$company_size == i] = "Small Cap"
        }
        else if (i %in% medium_cap){
                df$company_size[df$company_size == i] = "Medium Cap"
        }
        else if (i %in% large_cap){
                df$company_size[df$company_size == i] = "Large Cap"
        }
}

ggplot(df, aes(company_size)) + geom_bar(fill = 'blue',alpha = 0.6) +
        geom_text( stat='count', aes(label=..count..), vjust=-0.5)



# Fix experience
unique(df$experience)

entry_level = c('<1','1','2','3')
mid_level = c('4','5','6','7','8','9')
senior_level = c('10','11','12','13','14','15','16','17','18','19','20','>20')
all_level = c(entry_level,mid_level,senior_level)
all_level

for (i in all_level){
        if (i %in% entry_level){
                df$experience[df$experience == i] = "Entry Level"
        }
        else if (i %in% mid_level){
                df$experience[df$experience == i] = "Mid Level"
        }
        else if (i %in% senior_level){
                df$experience[df$experience == i] = "Senior Level"
        }
}

ggplot(df, aes(experience)) + geom_bar(fill = 'blue',alpha = 0.6) +
        geom_text( stat='count', aes(label=..count..), vjust=-0.5)

# Fix last_new_job 
unique(df$last_new_job)
df$last_new_job[df$last_new_job == ">4"] = "5+"
df$last_new_job[df$last_new_job == "never"] = "0"

ggplot(df, aes(last_new_job)) + geom_bar(fill = 'blue',alpha = 0.6) +
        geom_text( stat='count', aes(label=..count..), vjust=-0.5)

# Fix major_discipline 
unique(df$major_discipline)
other_major = c('Arts','Business Degree','Humanities','No Major')
for (i in other_major){
        if (i %in% other_major){
                df$major_discipline[df$major_discipline == i] = "Other"
}}
levels(df$company_type)

ggplot(df, aes(major_discipline)) + geom_bar(fill = 'blue',alpha = 0.6) +
        geom_text( stat='count', aes(label=..count..), vjust=-0.5)

# Fix company_type
unique(df$company_type)

other_company_type = c("Early Stage Startup","Funded Startup","NGO","Public Sector")
for (i in other_company_type){
        df$company_type[df$company_type == i] = "Other"
}

ggplot(df, aes(company_type)) + geom_bar(fill = 'blue',alpha = 0.6) +
        geom_text( stat='count', aes(label=..count..), vjust=-0.5)

# Fix target
unique(df$target)
df$target[df$target == "1"] = "Looking for job change"
df$target[df$target == "0"] = "Not looking for job change"
unique(df$target)


#----------------------------------------------------------------------------------------
## EDA
# plot distribution of target variable 
df$target = as.factor(df$target)
ggplot(df, aes(target)) + geom_bar(fill = 'blue',alpha = 0.6) +
        geom_text( stat='count', aes(label=..count..), vjust=-0.5)

# Proportion of "Looking for job change" is 0.2464427
4607/(4607+14087)

# Proportion of "Not looking for job change" is 0.7535573
14087/(4607+14087)

# plot missing data
missing = is.na(df)
sum(missing)
missmap(df, main = 'Missing Map', col = c('red', 'black')) #might take a while

# Fill in missing data for gender
df$gender = as.character(df$gender)
df$gender = replace_na(df$gender,'Other')
sum(is.na(df$gender))
unique(df$gender)

# Fill in missing data for enrolled_university
df$enrolled_university = replace_na(df$enrolled_university,'Unknown')
sum(is.na(df$enrolled_university))
unique(df$enrolled_university)

# Fill in missing data for education_level
df$education_level = replace_na(df$education_level,'Unknown')
sum(is.na(df$education_level))
unique(df$education_level)

# Fill in missing data for company_size
df$company_size = replace_na(df$company_size,'Unknown')
sum(is.na(df$company_size))
unique(df$company_size)

# Fill in missing data for major_discipline 
df$major_discipline  = replace_na(df$major_discipline ,'Other')
sum(is.na(df$major_discipline ))
unique(df$major_discipline )

# Fill in missing data for company_type 
df$company_type  = replace_na(df$company_type ,'Other')
sum(is.na(df$company_type ))
unique(df$company_type )

# Drop missing values in experience and last_new_job since the amounts are small

df = df[is.na(df$experience) != TRUE, ]

df = df[is.na(df$last_new_job) != TRUE, ]

# Check missing data again (There should be no missing data)
missmap(df, main = 'Missing Map', col = c('red', 'black'))


#----------------------------------------------------------------------------------------------
# plot gender
df$gender[df$target == "1"] = "Looking for job change"

ggplot(df, aes(gender)) + geom_bar(aes(fill = target)) +
        labs(y= "Count", x= 'Gender')+ 
        geom_text( stat='count', aes(label=..count..), vjust=-1)

ggplot(df, aes(gender)) + geom_bar(aes(fill = target), position = 'fill') +
        labs(y= "Percentage", x= 'Gender')

# plot relevant experience
ggplot(df, aes(relevent_experience)) + 
        geom_bar(aes(fill = target))+
        geom_text( stat='count', aes(label=..count..), vjust=-1)

ggplot(df, aes(relevent_experience)) + geom_bar(aes(fill = target), position = 'fill') +
        labs(y= "Percentage", x= 'Relevent Experience')

# plot experience
ggplot(df, aes(experience)) + 
        geom_bar(aes(fill = target))+
        geom_text( stat='count', aes(label=..count..), vjust=-1)

ggplot(df, aes(experience)) + geom_bar(aes(fill = target), position = 'fill') +
        labs(y= "Percentage", x= 'Experience')

# plot education and percentage
ggplot(df, aes(education_level)) + geom_bar(aes(fill = target)) +
        labs(y= "Count", x= 'Education Level')+ geom_text( stat='count', aes(label=..count..), vjust=-1)

ggplot(df, aes(education_level)) + geom_bar(aes(fill = target), position='fill') +
        labs(y= "Percentage", x= 'Education Level')

# plot major
ggplot(df, aes(major_discipline)) + 
        geom_bar(aes(fill = target))+
        geom_text( stat='count', aes(label=..count..), vjust=-1)

ggplot(df, aes(major_discipline)) + geom_bar(aes(fill = target), position='fill') +
        labs(y= "Percentage", x= 'Major')

# plot company_type
ggplot(df, aes(company_type)) + 
        geom_bar(aes(fill = target))+
        geom_text( stat='count', aes(label=..count..), vjust=-1)

ggplot(df, aes(company_type)) + geom_bar(aes(fill = target), position='fill') +
        labs(y= "Percentage", x= 'Company Type')

# plot city_development_index
ggplot(df, aes(city_development_index, fill = target))+ geom_density(alpha = 0.5)

# plot training_hours
ggplot(df, aes(training_hours, fill = target))+ geom_density(alpha = 0.5)



# Model Building----------------------------------------------------------------------------------------

#1 decision tree
library(tree)
cat_var = c("gender","relevent_experience","enrolled_university","education_level",
            "major_discipline","experience","company_size","company_type","target")
for(i in cat_var){
  df[,i]=as.factor(df[,i])
}
set.seed(1) # for reproducibility purposes
str(df)
# Train test split (80/20)
train.index = sample(1:nrow(df),nrow(df)*0.80)
train = df[train.index,]
test = df[-train.index,]
summary(df$target)
str(train)

# Build a decision tree model
model = tree(target~.,data = train)
summary(model)

# Plot the tree
plot(model)
text(model)

# 10-fold cross-validation 
best.tree = cv.tree(model,K=10)
best.tree

# Plot the error by tree size
plot(best.tree$size,best.tree$dev,xlab="tree size",ylab="deviance",type="b",pch=20,col="red")



# Prune the tree and plot it again
model.pruned = prune.tree(model,best=10)
summary(model.pruned)
plot(model.pruned)
text(model.pruned)

# Using your pruned tree, make predictions on the test set
pred.class = predict(model.pruned,test,type="class")
pred.class

# Confusion matrix
c.matrix = table(test$target,pred.class)
c.matrix

acc = mean(pred.class==test$target) 
sens.yes = c.matrix[1]/(c.matrix[1]+c.matrix[3])
prec.yes = c.matrix[1]/(c.matrix[2]+c.matrix[1])

# Show all the results
data.frame(acc,sens.yes,prec.yes)
str(df)

#2 Random Forest--------------------------------------------------------------------------
library(randomForest)
summary(as.factor(train$target))

# randomForest with 500 Trees
rf.model = randomForest(target~.,data=train, importance = TRUE)
rf.model
plot(rf.model,main="Error as tree increase")

# Choose 50 as the best model
rf.modeltree50=randomForest(target~.,data=train,ntree=50,importance = TRUE)
rf.modeltree50

# Predict on the test dataset
pred.rf=predict(rf.modeltree100,test)
c.matrix=table(test$target,pred.rf)
c.matrix

# Evaluation
acc = mean(pred.rf==test$target) # Calculate accuracy
sens.yes = c.matrix[1]/(c.matrix[1]+c.matrix[3]) # Calculate sensitivity
prec.yes = c.matrix[1]/(c.matrix[1]+c.matrix[2]) # Calculate precision

# Show all the results
data.frame(acc,sens.yes,prec.yes)


#3. KNN ----------------------------------------------------------------------------------------------------------------------------------------------
# Check the min and max values for the continuous variables
str(df)
int = c('city_development_index','training_hours')
summary(df[,int])
str(df)

# Create a function that takes a set of values, and returns normalized values
normalize = function(x) {
  return((x-min(x))/(max(x)-min(x)))
}

# Normalize city_development_index;last_new_job;training_hours
norm.city_development_index = normalize(df$city_development_index)
norm.training_hours = normalize(df$training_hours)


# Combine them into one df with city_development_index and training_hours variables
norm.df = cbind(df[,2:12],norm.city_development_index,norm.training_hours)
summary(norm.df)
summary(norm.df) # Show our new df with normalized variables
norm.df = cbind(norm.df[,1:9],norm.df[,11:13])
str(norm.df)
# Convert the categorical variables into dummy variables
#gender
norm.df$Female.gender = ifelse(norm.df$gender == "Female", 1, 0)
norm.df$Male.gender= ifelse(norm.df$gender == "Male", 1, 0)
#relevent_experience
norm.df$Has_releventexperience.relevent_experience = ifelse(norm.df$relevent_experience == "Has relevent experience", 1, 0)
norm.df$No_elevent_experience= ifelse(norm.df$relevent_experience == "No relevent experience", 1, 0)

# enrolled_university
norm.df$Full_time_course.enrolled_university = ifelse(norm.df$ enrolled_university == "Full time course", 1, 0)
norm.df$Part_time_course.enrolled_university = ifelse(norm.df$ enrolled_university == "Part time course", 1, 0)
norm.df$no_enrollment= ifelse(norm.df$relevent_experience == "no_enrollment", 1, 0)
# education_level
norm.df$Graduate.education_level = ifelse(norm.df$education_level == "Graduate", 1, 0)
norm.df$High_School.education_level= ifelse(norm.df$education_level == "High School", 1, 0)
norm.df$Masters.education_level= ifelse(norm.df$education_level == "Masters", 1, 0)
norm.df$Phd.education_level= ifelse(norm.df$education_level == "Phd", 1, 0)
norm.df$Phd.education_level= ifelse(norm.df$education_level == "Primary School", 1, 0)

#  major_discipline
norm.df$STEM.major_discipline = ifelse(norm.df$major_discipline == "STEM", 1, 0)

#experience
norm.df$Entry_Level.experience = ifelse(norm.df$experience == "Entry Level", 1, 0)
norm.df$Senior_Level.experience= ifelse(norm.df$experience == "Senior Level", 1, 0)
norm.df$Mid_Level.experience = ifelse(norm.df$experience == "Mid Level", 1, 0)

#company_size
norm.df$Large_Cap.company_size = ifelse(norm.df$company_size == "Large Cap", 1, 0)
norm.df$Medium_Cap.company_size= ifelse(norm.df$company_size == "Medium Cap", 1, 0)
norm.df$Small_Cap.company_size = ifelse(norm.df$company_size == "Small Cap", 1, 0)

#company_type
norm.df$Pvt_Ltd.company_type = ifelse(norm.df$company_type == "Pvt Ltd", 1, 0)

#last_new_job !!!!!
norm.df$zero.last_new_job = ifelse(norm.df$company_type == "0", 1, 0)
norm.df$one.last_new_job = ifelse(norm.df$company_type == "1", 1, 0)
norm.df$two.last_new_job = ifelse(norm.df$company_type == "2", 1, 0)
norm.df$three.last_new_job = ifelse(norm.df$company_type == "3", 1, 0)
norm.df$four.last_new_job = ifelse(norm.df$company_type == "4", 1, 0)

str(norm.df)
# Model building and comparison
library(class)

set.seed(1)
train.index = sample(1:nrow(norm.df),nrow(norm.df)*0.80)

train = norm.df[train.index,]
test = norm.df[-train.index,]

# training data for age, balance, duration, campaign, housing, cellular.contact,telephone.contact predictors
train.x = train[,c(11:36)] 
test.x = test[,c(11:36)] # test data as same as predictors in training data
train.cl = train[,10]# class labels in training data

# find k 
# Set the stage for 10 different sets of metrics for odd K's between 1-20.
rep = seq(1,20,2) 
rep.acc = rep
rep.sens = rep
rep.prec = rep

# Create index for 5-fold cross validation
k=5
fold = sample(1:k,nrow(train.x),replace=TRUE)

# Nested for loop
## Outer loop for KNN models with different K
## Inner loop for k-fold cross validation
iter=1 # index for rep iteration
for (K in rep) {
  
  # Space to store metrics from each iteration of k-fold cv
  kfold.acc = 1:k
  kfold.sens = 1:k
  kfold.prec = 1:k
  
  for (i in 1:k) {
    
    # data for test and training sets
    test.kfold = train.x[fold==i,]
    train.kfold = train.x[fold!=i,]
    
    # class labels for test and training sets
    test.cl.actual = train.cl[fold==i]
    train.cl.actual = train.cl[fold!=i]
    
    # make predictions on class labels for test set
    pred.class = knn(train.kfold,test.kfold,train.cl.actual,k=K)
    
    # evaluation metrics: accuracy, sensitivity, and precision (for "yes")
    c.matrix = table(test.cl.actual,pred.class)
    acc = mean(pred.class==test.cl.actual)
    sens.yes = c.matrix[1]/(c.matrix[1]+c.matrix[3])
    prec.yes = c.matrix[1]/(c.matrix[1]+c.matrix[2])
    
    # store results for each k-fold iteration
    kfold.acc[i] = acc
    kfold.sens[i] = sens.yes
    kfold.prec[i] = prec.yes
  }
  
  # store average k-fold performance for each KNN model
  rep.acc[iter] = mean(kfold.acc)
  rep.sens[iter] = mean(kfold.sens)
  rep.prec[iter] = mean(kfold.prec)
  iter=iter+1
}

# plot the results for each KNN model.
par(mfrow=c(1,3))
metric = as.data.frame(cbind(rep.acc,rep.sens,rep.prec))
color = c("blue","red","gold")
title = c("Accuracy","Sensitivity","Precision")

for (p in 1:3) {
  plot(metric[,p],type="b",col=color[p],pch=20,
       ylab="",xlab="K",main=title[p],xaxt="n")
  axis(1,at=1:10,labels= rep,las=2)
}
results = as.data.frame(cbind(rep,rep.acc,rep.sens,rep.prec))
names(results) = c("K","accuracy","sensitivity","precision")
results

pred.class = knn(train.x,test.x,train.cl,k=19)
c.matrix = table(test$target,pred.class)
c.matrix


acc = mean(pred.class==test$target)
sens.yes = c.matrix[1]/(c.matrix[1]+c.matrix[3])
prec.yes = c.matrix[1]/(c.matrix[1]+c.matrix[2])
as.data.frame(cbind(acc,sens.yes,prec.yes))


#4. Logistic Regression---------------------------------------------------------------------------
df$target = as.factor(df$target)
train.index = sample(1:nrow(df),nrow(df)*0.8)
train = df[train.index,]
test = df[-train.index,]
str(train)

# Number of observations in training and test set:
data.frame(full.size=nrow(df),train.size=nrow(train),test.size=nrow(test))

# Build Full Logistic Regression Model
log_model1 = glm(target~.,data=train,family=binomial)
summary(log_model1)

# Build Reduced Logistic Regression Model
log_model2 = glm(target~city_development_index+education_level+experience+company_size,data=train,
                 family = binomial)
summary(log_model2)

# Build One Variable Logistic Regression Model
log_model3 = glm(target~city_development_index,data=train,
                 family = binomial)
# Choose the best model based on AIC score
data.frame(full.model=AIC(log_model1),reduced.model=AIC(log_model2),one.model = AIC(log_model3))

# Use the full model to predict (since it has the lowest AIC score)
pred.prob = predict(log_model1,test,type="response")

# View first 10 predictions (probabilities of NOT looking for a job)
pred.prob[1:20]

# Setting a threshold of prob > 0.5 for NOT looking for a job
pred.class = pred.prob

pred.class[pred.prob>0.5] = "Not looking for job change"
pred.class[!pred.prob>0.5] = "Looking for job change"

pred.class[1:10] # View first 10 predictions

# Confusion Matrix
c.matrix = table(actual=test$target,prediction = pred.class)
c.matrix

acc = mean(pred.class==test$target)
sens.yes = c.matrix[1]/(c.matrix[3]+c.matrix[1])
prec.yes = c.matrix[1]/(c.matrix[2]+c.matrix[1])

data.frame(acc,sens.yes,prec.yes)

