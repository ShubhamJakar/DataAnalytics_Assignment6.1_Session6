#Assignment6.1_Session6

#Problem 

#1. Import the Titanic Dataset from the link Titanic Data Set.
#Perform the following:
#  a. Preprocess the passenger names to come up with a list of titles that represent families
#  and represent using appropriate visualization graph.
#  b. Represent the proportion of people survived from the family size using a graph.
#  c. Impute the missing values in Age variable using Mice Library, create two different
#graphs showing Age distribution before and after imputation.

#Answer1

#a)

pass_titles <- ""
for(i in 1:891){
  pass_titles <- c(pass_titles,pass_names[[i]][2])
}
pass_titles <- pass_titles[-1]
titles <- ""
for( i in 1:891){
  
  temp <- strsplit(pass_titles[i],"\\.")
  titles <- c(titles,temp)
}
titles[1] <- NULL
title_category <- ""
for(i in 1:891){
  title_category <- c(title_category,titles[[i]][1])
}
title_category <- title_category[-1]
title_category <- as.factor(title_category)
levels(title_category)
#check for summary
summary(title_category)
others <- 0
for(i in 1:17){
  if(i == 8 || i == 9 || i == 12 || i == 13 ) 
  {
    next()
  }
  else{
    others <- others + summary(title_category)[i]
  }
}
#grouping
# We have grouped small categories such as Capt , Col ,Don.....etc into 'others'

library(plotrix)
title_category <- c(summary(title_category)[8],summary(title_category)[9],summary(title_category)[12],summary(title_category)[13],others)
labelname <- c("Master","Miss","Mr.","Mrs.","Others")
pie3D(title_category,labels = labelname,main="List of Titles")


#b)

pass_names <- ""
for( i in 1:891){
  
  temp <- strsplit(titanic_train$Name[i],",")
  pass_names <- c(pass_names,temp)
}  
pass_names[[1]] <- NULL
pass_surnames <- ""
for(i in 1:891){
  pass_surnames <- c(pass_surnames,pass_names[[i]][1])
}
pass_surnames <- pass_surnames[-1]

class(pass_surnames)
pass_surnames <- as.factor(pass_surnames)
pass_surnames_list <- levels(pass_surnames)
pass_surnames_list_count <- rep(0,times=667)
count <- 0
for(i in 1:667){
  n1 <- pass_surnames_list[i]
  for(j in 1:891){
    n2 <- pass_surnames[j]
    if(n1 == n2)
      count <- count + 1
  }
  pass_surnames_list_count[i] <- count
  count <- 0
}
pass_surnames_list_count <- as.factor(pass_surnames_list_count)
family_size <- levels(pass_surnames_list_count)

#check for summary 
#summary(pass_surnames_list_count)

family_size_count <- table(pass_surnames_list_count)
titanic_survived <- data.frame(titanic_train$Survived,pass_surnames)
count<- rep(0,times=891)
titanic_survived <- data.frame(titanic_survived,count)
colnames(titanic_survived) <- c("survived","surnames","family_size")


for(i in 1:891){
  n <- titanic_survived$surnames[i]
  for(j in 1:667){
    if(n == pass_surnames_list[j]){
      titanic_survived$family_size[i] <- pass_surnames_list_count[j]
      break
    }
  }
}

s1 <- table(titanic_survived$survived,titanic_survived$family_size)
barplot(s1,xlab="Family Size",col=c("red","green"),legend=c("Not Survived","Survived"),beside = T,main = "Proportion of People Survived")


#c)

library(titanic)
sum(is.na(titanic_train$Age))
install.packages("mice")
library(mice)
md.pattern(titanic_train)
# We found there are total 177 missing values in AGE attribute of titanic_train dataset

mice_imputes = mice(titanic_train, m=5, maxit = 40)

Imputed_data=complete(mice_imputes,5)

hist(titanic_train$Age, freq=T, main='Original Data of age ', 
     col='green')
hist(Imputed_data$Age, freq=T, main='Imputed Data of age', 
     col="red")

