
#Beer: Problem 2 #

####Loading the data####
library(readr)
beer_reviews <- read_csv("C:/Users/manit_patel/Downloads/Spring 18/R SCRIPTS REPOSITORY/J.Jill Data Challenge/Files/beer_reviews/beer_reviews.csv")
View(beer_reviews)

#Checking for missing values

colSums(is.na(beer_reviews))

####Exploring missing values####

library(Rcpp)
library(mice)

md.pattern(beer_reviews)

install.packages("VIM")

library(VIM)

mice_plot = aggr(beer_reviews, sortVars=TRUE,
                 labels=names(beer_reviews), cex.axis=.7,
                 gap=3, ylab=c("Missing data","Pattern"))


#### Structure and summary####

str(beer_reviews)

summary(beer_reviews)

attach(beer_reviews)

####Imputing the missing values in beer_abv and review_profilename####


colSums(is.na(beer_reviews))
library(Hmisc)

#Imputing the profile_name with mode
beer_reviews$review_profilename= with(beer_reviews, impute(review_profilename, mode))

hist(beer_abv)
#Imputing the missing values with median value of beer_abv
beer_reviews$beer_abv= with(beer_reviews, impute(beer_abv, median))


####Plotting histograms of review_time, review_overall, review_aroma, review_appearance, review_palate, review_taste, and beer_abv####

hist(review_time)
hist(review_overall)
hist(review_aroma)
hist(review_appearance)
hist(review_palate)
hist(review_taste)
hist(beer_abv)



####Q2.	Which brewery produces the strongest beers by ABV%?####

library(dplyr)

#Grouping the brewery_name and calculating the average beer_abv

x=(summarise(group_by(beer_reviews, brewery_name), mean(beer_abv)))

class(x)

# Schorschbräu produces the strongest beers.
#Highest beer_abv is 57.7


####Q3	If you had to pick 3 beers to recommend using only this data, which would you pick?####


#Creating a new variable : Sum of all reviews

beer_reviews$sum_reviews= review_overall+review_appearance+review_aroma+review_palate+review_taste

library(dplyr)


#check how many observations with each beer_name

x= beer_reviews %>%
  group_by(beer_name) %>%
  summarize(count=n(), mean_reviews = mean(sum_reviews)) %>%
  arrange(count,mean_reviews)

quantile(x$count)

X_More_than_1000= subset(x, x$count > 1000)



####Q4 Which of the factors (aroma, taste, appearance, palette) are most important in determining the overall quality of a beer?####

# Checking Correlations among variables
D=data.frame(beer_reviews[,c("review_time","review_overall","review_aroma","review_appearance","review_palate","review_taste","beer_abv")])
cor(D)


#Creating a new variable that determines the quality of beer. The overall review greater than or equal to 4 are recoded as 1 (Good Quality) and reviews less than 4 are recoded as 0 (Bad Quality).

beer_reviews$Quality= ifelse (beer_reviews$review_overall >= 4 ,1, 0)

barchart(beer_reviews$Quality)


#### Splitting the Data Set ####
ratio = sample(1:nrow(beer_reviews), size = 0.25*nrow(beer_reviews))
Test = beer_reviews[ratio,] #Test dataset 25% of total
Training = beer_reviews[-ratio,] #Train dataset 75% of total


#### MODEL : DECISION TREES ####

library(rpart)
library(rpart.plot)

CART_model=rpart(Training$Quality~ review_appearance+review_aroma+review_palate+review_taste , data=Training, method="class")
prp(CART_model)
summary(CART_model)


#Accuracy of CART model on test data

predict_CART_test=predict(CART_model,newdata=Test, type="class")
table(Test$Quality,predict_CART_test)
(103147+226697)/nrow(Test)
# Accuracy = 0.831

#AUC
pred_CART_test=predict(CART_model, newdata=Test)
pred_prob_Test_CART=pred_CART_test[, 2]

library(ROCR)

pred_input_test_CART=prediction(pred_prob_Test_CART,Test$Quality)
AUC= performance(pred_input_test_CART,"auc")
print(AUC@y.values)

#AUC = 0.847





####Q5: Lastly, if I typically enjoy a beer due to its aroma and appearance, which beer style should I try?####

#Creating a new variable of total reviews of aroma and appearance

beer_reviews$sum_aroma_appearance= review_appearance+ review_aroma

y=beer_reviews %>%
  group_by(beer_style) %>%
  summarize(count= n(), mean_aroma_appearance= mean(sum_aroma_appearance))
