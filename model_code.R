#the first part was to forecast the budget for all missing inputs
# We split the original data into two dataset the first one called 'films' which contained the movies with a budget and the second one called budget_test_df which did not have a budget
films = budget_train

#The next lines of code is part of data cleaning and preparation
# first we rename some variables to make it more instructive
require(reshape)
films = rename(films,c(main_actor1_known_for= 'main_actor1_best_movie',main_actor2_known_for= 'main_actor2_best_movie',main_actor3_known_for = 'main_actor3_best_movie',day.of.the.week='day_of_week'))

# Second we dummify all categorical variables
attach(films)
summary(budget)
#dummified categorical variable 
# note that day_of_week is created from the excel file 
films$main_actor1_name=as.factor(films$main_actor1_name) 
films$main_actor2_name=as.factor(films$main_actor2_name) 
films$main_actor3_name=as.factor(films$main_actor3_name) 

films$main_actor1_best_movie = as.factor(films$main_actor1_best_movie)
films$main_actor2_best_movie = as.factor(films$main_actor2_best_movie)
films$main_actor3_best_movie = as.factor(films$main_actor3_best_movie)

films$main_spoken_language=as.factor(films$main_spoken_language) 


films$main_director_name = as.factor(films$main_director_name)

films$main_producer_name = as.factor(films$main_producer_name)

films$editor_name = as.factor(films$editor_name)

films$main_production_company = as.factor(films$main_production_company)

films$main_production_country = as.factor(films$main_production_country)

films$day_of_week = as.factor(films$day_of_week)

films$release_month = as.factor(films$release_month)

# this part creates new variables based on experience buckets of director, editor, producer and country
# we also create a variable that dummify if a movie is in english or other language
# This is done to reduce the number of varibales. Imagine feeding a model with all producer name 

attach(films)
# Relabeling all categorical variable in bundles instead of unique values
###main_director_name
#package to use count funtion
#install.packages("plyr")
require(plyr)
#install.packages('ave')
require(ave)
#counts occurances of all levels in a variable
count(films, var = 'main_director_name')

##create a column with main_director_name counts 
films = transform(films, director_count = ave(genre_action, main_director_name, FUN = length))
attach(films)

##create the categorical variable with condition
films$director_experience = ifelse(films$director_count <= 1 , 'beginner',
                                   ifelse(films$director_count <=  3, 'novice',
                                          ifelse(films$director_count <= 5, 'intermediate',
                                                 ifelse(films$director_count <= 10, 'advanced',
                                                        ifelse(films$director_count > 10, 'expert','N/A')))))

####main_production_company
films = transform(films, production_company_count = ave(genre_action, main_production_company, FUN = length))
attach(films)

#check counts
count(films, var = 'main_production_company')

##crate the ctegorical varible with condition
films$production_company_experience = ifelse(films$production_company_count <= 9 , 'beginner',
                                             ifelse(films$production_company_count <  100, 'advanced',
                                                    ifelse(films$production_company_count >= 100, 'expert','NA')))


####main production country

films = transform(films, main_country_count = ave(genre_action, main_production_country, FUN = length))
attach(films)

###create variable

films$production_country_popularity = ifelse(films$main_country_count <= 9 , 'unpopular',
                                             ifelse(films$main_country_count <  100, 'popular',
                                                    ifelse(films$main_country_count >= 100, 'very popular','NA')))


films$is_english = ifelse(films$main_spoken_language == 'Englsih' , 1,
                          ifelse(films$main_spoken_language !=  'English', 0, 'NA'
                          ))

films$main_spoken_language_is_english = ifelse(films$is_english == 0 , 1,
                                               ifelse(films$is_english ==  'NA', 0, 'NA'
                                               ))

#counts occurances of all levels in a variable
count(films, var = 'main_producer_name')

##create a column with main_producer_name counts 
films = transform(films, prodcuer_count = ave(genre_action, main_producer_name, FUN = length))
attach(films)
##create the categorical variable with condition
films$producer_experience = ifelse(films$prodcuer_count <=  1, 'beginner',
                                   ifelse(films$prodcuer_count <=  3, 'novice',
                                          ifelse(films$prodcuer_count <= 5, 'intermediate',
                                                 ifelse(films$prodcuer_count <= 10, 'advanced',
                                                        ifelse(films$prodcuer_count <= 100, 'expert', 'no producer')))))


#counts occurances of all levels in a variable
count(films, var = 'editor_name')

##create a column with editor_name counts 
films = transform(films, editor_count = ave(genre_action, editor_name, FUN = length))
attach(films)
##create the categorical variable with condition
films$editor_experience = ifelse(films$editor_count <=  1, 'beginner',
                                 ifelse(films$editor_count <=  3, 'novice',
                                        ifelse(films$editor_count <= 5, 'intermediate',
                                               ifelse(films$editor_count <= 10, 'advanced',
                                                      ifelse(films$editor_count <= 100, 'expert', 'no editor')))))



attach(films)


plot(total_number_of_genres,budget)
#create a regression to forecast budget
mreg_budget =lm(budget ~ poly(duration_minutes,3) + main_actor1_star_meter + main_actor2_star_meter +
                  main_actor3_star_meter  + total_number_of_directors  + total_number_of_producers  + 
                  total_number_of_production_countries + genre_action + genre_adventure + genre_animation +
                  imdbRating + genre_biography +genre_comedy +genre_crime + genre_documentary + release_year + 
                  production_company_experience + production_country_popularity + director_experience + 
                  release_month + genre_drama +genre_family +genre_filmnoir+genre_history+genre_horror+genre_music+
                  genre_musical+genre_mystery+genre_realitytv+genre_romance+genre_scifi+genre_shortfilm+genre_sport+
                  genre_thriller+genre_war+genre_western + total_number_of_genres + main_actor1_is_female + main_actor2_is_female + 
                  main_actor3_is_female + total_number_of_production_companies + editor_experience + producer_experience+
                  genre_action*main_actor2_is_female + genre_family*genre_animation)
summary(mreg_budget)

#test the egresiion with crossval
require(caTools)
library(boot)
sample=sample.split(films$budget, SplitRatio=0.5) ###Step 1
train=subset(films, sample==TRUE) ###Step 2
test=subset(films, sample==FALSE) ###Step 2

test$pred=predict(mreg_budget, test) ###Step 4
test$res=(test$budget - test$pred) ###Step 4
test$res_sq=(test$res)^2 ###Step 4
mse = mean(test$res_sq) ###Step 4 
sqrt(mse)



detach(films)

# feed the the data that did not have a budget with the forecasting model created above
budget_test_df = budget_test
attach(budget_test_df)

budget_test_df = rename(budget_test_df,c(main_actor1_known_for= 'main_actor1_best_movie',main_actor2_known_for= 'main_actor2_best_movie',main_actor3_known_for = 'main_actor3_best_movie',day.of.the.week='day_of_week'))

attach(budget_test_df)

budget_test_df$main_actor1_name=as.factor(budget_test_df$main_actor1_name) 
budget_test_df$main_actor2_name=as.factor(budget_test_df$main_actor2_name) 
budget_test_df$main_actor3_name=as.factor(budget_test_df$main_actor3_name) 

budget_test_df$main_actor1_best_movie = as.factor(budget_test_df$main_actor1_best_movie)
budget_test_df$main_actor2_best_movie = as.factor(budget_test_df$main_actor2_best_movie)
budget_test_df$main_actor3_best_movie = as.factor(budget_test_df$main_actor3_best_movie)

budget_test_df$main_spoken_language=as.factor(budget_test_df$main_spoken_language) 


budget_test_df$main_director_name = as.factor(budget_test_df$main_director_name)

budget_test_df$main_producer_name = as.factor(budget_test_df$main_producer_name)

budget_test_df$editor_name = as.factor(budget_test_df$editor_name)

budget_test_df$main_production_company = as.factor(budget_test_df$main_production_company)

budget_test_df$main_production_country = as.factor(budget_test_df$main_production_country)

budget_test_df$day_of_week = as.factor(budget_test_df$day_of_week)

budget_test_df$release_month = as.factor(budget_test_df$release_month)

attach(budget_test_df)
# Relabeling all categorical variable in bundles instead of unique values
###main_director_name
#package to use count funtion
#install.packages("plyr")
require(plyr)
#install.packages('ave')
require(ave)
#counts occurances of all levels in a variable
count(budget_test_df, var = 'main_director_name')

##create a column with main_director_name counts 
budget_test_df = transform(budget_test_df, director_count = ave(genre_action, main_director_name, FUN = length))
attach(budget_test_df)

##create the categorical variable with condition
budget_test_df$director_experience = ifelse(budget_test_df$director_count <= 1 , 'beginner',
                                            ifelse(budget_test_df$director_count <=  3, 'novice',
                                                   ifelse(budget_test_df$director_count <= 5, 'intermediate',
                                                          ifelse(budget_test_df$director_count <= 10, 'advanced',
                                                                 ifelse(budget_test_df$director_count > 10, 'expert','N/A')))))

####main_production_company
budget_test_df = transform(budget_test_df, production_company_count = ave(genre_action, main_production_company, FUN = length))
attach(budget_test_df)

#check counts
count(budget_test_df, var = 'main_production_company')

##crate the ctegorical varible with condition
budget_test_df$production_company_experience = ifelse(budget_test_df$production_company_count <= 9 , 'beginner',
                                                      ifelse(budget_test_df$production_company_count <  100, 'advanced',
                                                             ifelse(budget_test_df$production_company_count >= 100, 'expert','NA')))


####main production country

budget_test_df = transform(budget_test_df, main_country_count = ave(genre_action, main_production_country, FUN = length))
attach(budget_test_df)

###create variable

budget_test_df$production_country_popularity = ifelse(budget_test_df$main_country_count <= 9 , 'unpopular',
                                                      ifelse(budget_test_df$main_country_count <  100, 'popular',
                                                             ifelse(budget_test_df$main_country_count >= 100, 'very popular','NA')))


budget_test_df$is_english = ifelse(budget_test_df$main_spoken_language == 'Englsih' , 1,
                                   ifelse(budget_test_df$main_spoken_language !=  'English', 0, 'NA'
                                   ))

budget_test_df$main_spoken_language_is_english = ifelse(budget_test_df$is_english == 0 , 1,
                                                        ifelse(budget_test_df$is_english ==  'NA', 0, 'NA'
                                                        ))

#counts occurances of all levels in a variable
count(budget_test_df, var = 'main_producer_name')

##create a column with main_producer_name counts 
budget_test_df = transform(budget_test_df, prodcuer_count = ave(genre_action, main_producer_name, FUN = length))
attach(budget_test_df)
##create the categorical variable with condition
budget_test_df$producer_experience = ifelse(budget_test_df$prodcuer_count <=  1, 'beginner',
                                            ifelse(budget_test_df$prodcuer_count <=  3, 'novice',
                                                   ifelse(budget_test_df$prodcuer_count <= 5, 'intermediate',
                                                          ifelse(budget_test_df$prodcuer_count <= 10, 'advanced',
                                                                 ifelse(budget_test_df$prodcuer_count <= 100, 'expert', 'no producer')))))


#counts occurances of all levels in a variable
count(budget_test_df, var = 'editor_name')

##create a column with main_producer_name counts 
budget_test_df = transform(budget_test_df, editor_count = ave(genre_action, editor_name, FUN = length))
attach(budget_test_df)
##create the categorical variable with condition
budget_test_df$editor_experience = ifelse(budget_test_df$editor_count <=  1, 'beginner',
                                          ifelse(budget_test_df$editor_count <=  3, 'novice',
                                                 ifelse(budget_test_df$editor_count <= 5, 'intermediate',
                                                        ifelse(budget_test_df$editor_count <= 10, 'advanced',
                                                               ifelse(budget_test_df$editor_count <= 100, 'expert', 'no editor')))))



attach(budget_test_df)



#predicting budget
pred = predict(mreg_budget, newdata = budget_test_df )

budget3 <- c(pred)

budget_test_df$budget = budget3

attach(budget_test_df)

names(budget_test_df) == names(films)
detach(budget_test_df)
full_data = rbind(films, budget_test_df)

attach(full_data)
summary(budget)

full_data$budget = ifelse(full_data$budget <=  0, 10000000 , budget)
attach(full_data)

summary(budget)


# end of data preparation & cleaning


# analyzing linearity for quant variables
# All single reg 
budget_reg = lm(imdbRating ~ budget)
duration_reg = lm(imdbRating ~ duration_minutes)
total_number_of_spoken_languages_reg = lm(imdbRating ~ total_number_of_spoken_languages)
total_number_of_genres_reg = lm(imdbRating ~ total_number_of_genres)
main_actor1_star_meter_reg = lm(imdbRating ~ main_actor1_star_meter)
main_actor2_star_meter_reg = lm(imdbRating ~ main_actor2_star_meter)
main_actor3_star_meter_reg = lm(imdbRating ~ main_actor3_star_meter)
total_number_of_actors_reg = lm(imdbRating ~ total_number_of_actors)
total_number_of_directors_reg = lm(imdbRating ~ total_number_of_directors)
total_number_of_producers_reg = lm(imdbRating ~ total_number_of_producers)
total_number_of_production_companies_reg = lm(imdbRating ~ total_number_of_production_companies)
total_number_of_production_countries_reg = lm(imdbRating ~ total_number_of_production_countries)


#testing for linearity
#install.packages('car')
library(car)
residualPlots(budget_reg) #not linear 
residualPlots(duration_reg) #linear 
residualPlots(total_number_of_spoken_languages_reg) #linear o
residualPlots(total_number_of_genres_reg) # not linear o
residualPlots(main_actor1_star_meter_reg) #  linear x
residualPlots(main_actor2_star_meter_reg) # linear o
residualPlots(main_actor3_star_meter_reg) # linear o
residualPlots(total_number_of_actors_reg) # not linear o
residualPlots(total_number_of_directors_reg) # linear o
residualPlots(total_number_of_producers_reg) # is not linear o
residualPlots(total_number_of_production_companies_reg) # linear x
residualPlots(total_number_of_production_countries_reg) # linear x

# finding the right poly degree for the non linear

#total_number_of_genres
library(boot)
cv.error=rep(0,4)
for (i in 1:4) {
  
  mreg=glm(imdbRating~poly(total_number_of_genres,i), data=full_data)
  cv.error[i]=cv.glm(full_data, mreg, K=52 )$delta[1]
}

plot(cv.error, col="red")

x = which.min(cv.error)
min_error = min(cv.error)
sqrt(min_error)
b_reg1=lm(imdbRating~ poly(total_number_of_genres,1))
b_reg2=lm(imdbRating~ poly(total_number_of_genres,2))
b_reg3=lm(imdbRating~ poly(total_number_of_genres,3)) # best
test$pred=predict(b_reg3, test) 
test$res=(test$imdbRating - test$pred) 
test$res_sq=(test$res)^2 
mean(test$res_sq) 

anova(b_reg1, b_reg2, b_reg3)

#duration
#install.packages("boot")

library(boot)
cv.error=rep(0,10)
for (i in 1:10) {
  
  mreg=glm(imdbRating~poly(budget,i), data=full_data)
  cv.error[i]=cv.glm(full_data, mreg, K=52 )$delta[1]
}
plot(cv.error, col="red")

x = which.min(cv.error)
min_error = min(cv.error)
sqrt(min_error)
d_reg1=lm(imdbRating~ poly(budget,1))
d_reg2=lm(imdbRating~ poly(budget,2))
d_reg3=lm(imdbRating~ poly(budget,3))
d_reg4=lm(imdbRating~ poly(budget,4)) #best
test$pred=predict(d_reg4, test) 
test$res=(test$imdbRating - test$pred)
test$res_sq=(test$res)^2 
mean(test$res_sq)
d_reg5=lm(imdbRating~ poly(budget,5))
d_reg6=lm(imdbRating~ poly(budget,6))
d_reg7=lm(imdbRating~ poly(budget,7))
d_reg8=lm(imdbRating~ poly(budget,8))
d_reg9=lm(imdbRating~ poly(budget,9))
d_reg10=lm(imdbRating~ poly(budget,10))
anova(d_reg1, d_reg2, d_reg3, d_reg4, d_reg5,d_reg6,d_reg7,d_reg8,d_reg9,d_reg10)
# degree 4 is the best 

# total number of actors
par(mfrow=c(1,1))

library(boot)
cv.error=rep(0,8)
for (i in 1:8) {
  mreg=glm(imdbRating~poly(total_number_of_actors,i) , data=full_data)
  cv.error[i]=cv.glm(full_data, mreg, K=52 )$delta[1]
}
plot(cv.error, col="red")

x = which.min(cv.error)
min_error = min(cv.error)
sqrt(min_error)

a_reg1=lm(imdbRating~ poly(total_number_of_actors,1))
a_reg2=lm(imdbRating~ poly(total_number_of_actors,2)) #best
test$pred=predict(a_reg2, test)
test$res=(test$imdbRating - test$pred) 
test$res_sq=(test$res)^2 
mean(test$res_sq)
a_reg3=lm(imdbRating~ poly(total_number_of_actors,3))
a_reg4=lm(imdbRating~ poly(total_number_of_actors,4))
a_reg5=lm(imdbRating~ poly(total_number_of_actors,5))
a_reg6=lm(imdbRating~ poly(total_number_of_actors,6))
a_reg7=lm(imdbRating~ poly(total_number_of_actors,7))
a_reg8=lm(imdbRating~ poly(total_number_of_actors,8))
a_reg9=lm(imdbRating~ poly(total_number_of_actors,9))
a_reg10=lm(imdbRating~ poly(total_number_of_actors,10))
anova(a_reg1, a_reg2, a_reg3, a_reg4, a_reg5,a_reg6,a_reg7,a_reg8,a_reg9,a_reg10)

# total number of actors

par(mfrow=c(1,1))

library(boot)
cv.error=rep(0,8)
for (i in 1:8) {
  mreg=glm(imdbRating~poly(total_number_of_producers,i) , data=full_data)
  cv.error[i]=cv.glm(full_data, mreg, K=52 )$delta[1]
}
plot(cv.error, col="red")

x = which.min(cv.error)
min_error = min(cv.error)
sqrt(min_error)

p_reg1=lm(imdbRating~ poly(total_number_of_producers,1))
p_reg2=lm(imdbRating~ poly(total_number_of_producers,2))
p_reg3=lm(imdbRating~ poly(total_number_of_producers,3))
p_reg4=lm(imdbRating~ poly(total_number_of_producers,4))
p_reg5=lm(imdbRating~ poly(total_number_of_producers,5)) #degree 5 is the best
test$pred=predict(p_reg5, test) 
test$res=(test$imdbRating - test$pred) 
test$res_sq=(test$res)^2 
mean(test$res_sq) 
p_reg6=lm(imdbRating~ poly(total_number_of_producers,6))
p_reg7=lm(imdbRating~ poly(total_number_of_producers,7))
p_reg8=lm(imdbRating~ poly(total_number_of_producers,8))
p_reg9=lm(imdbRating~ poly(total_number_of_producers,9))
p_reg10=lm(imdbRating~ poly(total_number_of_producers,10))
anova(p_reg1, p_reg2, p_reg3, p_reg4, p_reg5,p_reg6,p_reg7,p_reg8,p_reg9,p_reg10)

# tryin spline regression on the non linear variable above and checking the difference in mse with their 
#respective polynomial regression abouve

#getting the knots position 
quantile(total_number_of_actors, c(0.5))
quantile(total_number_of_producers, c(0.5))
quantile(budget, c(0.5))
quantile(total_number_of_genres, c(0.5))
summary(budget)

quantile(total_number_of_actors, c(0.33,.66))
quantile(total_number_of_producers, c(0.33,0.66))
quantile(budget, c(0.33,0.66))
quantile(total_number_of_genres, c(0.33,0.66))

library(splines)

# running a loop to identify the best combination of degree and knot for each of the non linear variables studied
cv.error = rep(4848454441541,1)
values <- c(7,8,9,10,11,12,13,14)
values[1]
for (a1 in  1:2){
  for (a2 in 1:2){
    for (a3 in 1:2){
      for (a4 in 1:2){
        for (b1 in 2:4){
          for(b2 in 2:4){
            for(b3 in 2:4){
              for(b4 in 2:4){
                
                if (a1 == 1) {
                  ucq <- c(16)
                }
                if (a1 == 2){
                  ucq <- c(13,20)
                }
                if (a2 == 1){
                  ccq <- c(1)
                }
                if (a2 == 2){
                  ccq <- c(1,2) 
                }
                if (a3 == 1){
                  gsq <- c(13000000)
                }
                if (a3 == 2){
                  gsq <- c(13000000,25000000)
                }
                if ( a4 == 1){
                  yorq <- c(3)
                }
                if ( a4 == 2){
                  yorq <- c(2,3)
                }
                
                spliner=glm(imdbRating~bs(total_number_of_actors,knots=c(ucq), degree=b1)+bs(total_number_of_producers,knots=c(ccq), degree=b2)+ bs(budget,knots=c(gsq), degree=b3) + bs(total_number_of_genres,knots=c(yorq), degree=b4))
                mse_spline = cv.glm(full_data, spliner, K=10 )$delta[1]
                if (mse_spline < cv.error){
                  cv.error = mse_spline
                  values <- c(a1,a2,a3,a4,b1,b2,b3,b4)
                }
              }
            }
          }
        }
      }
    }
  }
}


cv.error
values 

# returning their respective mse to compare with the polynomial regression results
poly3splinreg=lm(imdbRating~bs(total_number_of_actors,knots=c(0.33,.66), degree=2))
summary(poly3splinreg)
test$pred=predict(poly3splinreg, test) ###Step 4
test$res=(test$imdbRating - test$pred) ###Step 4
test$res_sq=(test$res)^2 ###Step 4
mean(test$res_sq) ###Step 4 

poly4splinreg=lm(imdbRating~bs(total_number_of_producers,knots=c(1,2), degree=3))
summary(poly4splinreg)
test$pred=predict(poly4splinreg, test) ###Step 4
test$res=(test$imdbRating - test$pred) ###Step 4
test$res_sq=(test$res)^2 ###Step 4
mean(test$res_sq) ###Step 4

poly5splinreg=lm(imdbRating~bs(budget,knots=c(13000000,25000000), degree=2))
summary(poly5splinreg)
test$pred=predict(poly5splinreg, test) ###Step 4
test$res=(test$imdbRating - test$pred) ###Step 4
test$res_sq=(test$res)^2 ###Step 4
mean(test$res_sq) ###Step 4

poly6splinreg=lm(imdbRating~bs(total_number_of_genres,knots=c(2,3), degree=2))
summary(poly6splinreg)
test$pred=predict(poly6splinreg, test) ###Step 4
test$res=(test$imdbRating - test$pred) ###Step 4
test$res_sq=(test$res)^2 ###Step 4
mean(test$res_sq) ###Step 4

# conclude that polynomial regression had better result for each variable


#running models 

# k -fold
library(boot)

sample=sample.split(full_data$film_title, SplitRatio=0.5) ###Step 1
train=subset(full_data, sample==TRUE) ###Step 2
test=subset(full_data, sample==FALSE) ###Step 2

#initial regression
Kfitlin=lm(imdbRating~ poly(duration_minutes,3) + poly(budget,4) + editor_experience +total_number_of_spoken_languages + poly(total_number_of_genres,2) + 
             main_actor2_star_meter + main_actor3_star_meter+ poly(total_number_of_actors,2) + total_number_of_directors + 
             poly(total_number_of_producers,5) + release_month + day_of_week + main_spoken_language_is_english + main_actor1_is_female + 
             main_actor3_is_female + director_experience + genre_action + genre_adventure+genre_comedy+genre_crime +
             genre_drama + genre_horror + genre_romance + genre_adventure*main_actor1_is_female + genre_documentary*main_spoken_language_is_english + release_year*genre_action + release_year*genre_comedy ,data=full_data)
summary(Kfitlin)
test$pred=predict(Kfitlin, test) ###Step 4
test$res=(test$imdbRating - test$pred) ###Step 4
test$res_sq=(test$res)^2 ###Step 4
mean(test$res_sq) ###Step 4 


#simplified regression
Kfitlin2=lm(imdbRating~ poly(duration_minutes,6) + poly(total_number_of_actors,2) + poly(budget,4) + 
              editor_experience + day_of_week + director_experience +genre_action +genre_drama+genre_horror + 
              genre_documentary*main_spoken_language_is_english+release_year*genre_action +  release_year*genre_comedy 
            + main_actor2_is_female*main_actor1_is_female , data=full_data)
test$pred=predict(Kfitlin2, test)
test$res=(test$imdbRating - test$pred) 
test$res_sq=(test$res)^2
mean(test$res_sq)

#Factor contribution to mse dimnution: duration(0.88),budget (-0.10), editor (-.01), total_numer_of_Actor (-.02), day_of_week (-.01), direcotr_experience (-.03), genre_action (-.01), genre_crime(-0.1), genre_horror (-.03),genre_documentary*main_spoken_language_is_english(-.02), release_year*genre_action(-.01),main_actor2_is_female*main_actor1_is_female(-.04)

summary(Kfitlin2)

#k-fold cross validation of simplified regression
Kfitlin3=glm(imdbRating~ poly(duration_minutes,6) + poly(total_number_of_actors,2) + poly(budget,4) + 
               editor_experience + day_of_week + director_experience +genre_action +genre_drama+genre_horror + 
               genre_documentary*main_spoken_language_is_english+release_year*genre_action +  release_year*genre_comedy 
             + main_actor2_is_female*main_actor1_is_female , data=full_data)
cv.error=cv.glm(full_data, Kfitlin3, K=52)$delta[1]
cv.error


# testing for Heteroskedasticity of the simplified model 
plot(predict(Kfitlin2), residuals(Kfitlin2), col="red")
abline(0,0, lty=2)
ncvTest(Kfitlin2) #pvalue is low

#correcting Heteroskedasticity
#install.packages("lmtest")
#install.packages("plm")
require(lmtest)
require(plm)
# heteroskedasticity makes our estimates look more significant than they really are. therefore we correct and obtain the following coefficiants
coeftest(Kfitlin2, vcov=vcovHC(Kfitlin2, type='HC1'))

#identifying outliers
qqPlot(Kfitlin2)
outlierTest(Kfitlin2)
#removing outliers
full_data_2=full_data[-c(2948, 3765,748,1310,3463,4288,3300,4597,4378,5042), ]
sample=sample.split(full_data_2$film_title, SplitRatio=0.5) ###Step 1
train=subset(full_data_2, sample==TRUE) ###Step 2
test=subset(full_data_2, sample==FALSE) ###Step 2

#running a model without the outlier and hetero
Kfitlin4=lm(imdbRating~ poly(duration_minutes,6) + poly(total_number_of_actors,2) + poly(budget,4) + 
              editor_experience + day_of_week + director_experience +genre_action +genre_drama+genre_horror + 
              genre_documentary*main_spoken_language_is_english+release_year*genre_action +  release_year*genre_comedy 
            + main_actor3_is_female*main_actor1_is_female , data=full_data_2)
summary(Kfitlin4)
test$pred=predict(Kfitlin4, test)
test$res=(test$imdbRating - test$pred) 
test$res_sq=(test$res)^2
mean(test$res_sq)

#k-fold final mse
Kfitlin5=glm(imdbRating~ poly(duration_minutes,6) + poly(total_number_of_actors,2) + poly(budget,4) + 
               day_of_week + director_experience +genre_action +genre_drama+genre_horror +
               genre_documentary*main_spoken_language_is_english+release_year*genre_action +  release_year*genre_comedy 
             + main_actor3_is_female*main_actor1_is_female + main_actor2_star_meter*release_year + main_actor1_is_female*genre_adventure, data=full_data_2)
cv.error=cv.glm(full_data_2, Kfitlin5, K=74)$delta[1]
cv.error

#my final mse is 0.6523999 given a k-fold cross val with k = 70 
detach(full_data_2)

# predicting the rating of the new movies coming out in 2019 
#preparing the new data
attach(Midterm.Movies)

Midterm.Movies$day_of_week = as.factor(Midterm.Movies$day_of_week)

Midterm.Movies$release_month = as.factor(Midterm.Movies$release_month)

Midterm.Movies$main_spoken_language_is_english = as.factor(Midterm.Movies$main_spoken_language_is_english)

attach(Midterm.Movies)

#predicting
pred_new_movie = predict(Kfitlin5, newdata = Midterm.Movies )

rating <- c(pred_new_movie)

#storing in original table 
Midterm.Movies$imdbRating = rating

attach(Midterm.Movies)



