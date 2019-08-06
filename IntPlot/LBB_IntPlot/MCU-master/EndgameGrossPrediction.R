# Please refer to my article at Linkedin for detailed explanation.

# The link to the article is https://www.linkedin.com/pulse/avengers-endgame-surpass-avatar-simple-regression-analysis-kumavat
# Alternate link is: https://medium.com/@pramodk.resume/will-avengers-endgame-surpass-avatar-a-simple-regression-analysis-part-1-9e4f9fdc60b3

# ------------- Part 1 - Predict DomesticGross of Avengers: Endgame ---------------------

rm(list = ls())

data = read.csv("MarvelUniverse.csv")
head(data)

dim(data)
str(data)

summary(data)

# Let's now load our test data.

data2 = read.csv("AvengersEndgame.csv")
head(data2)

# I must drop the redundant Movie variable. I'll also drop the 
# OverseasGross and WorldwideGross variables as they are not yet
# determined while we make predictions.

# Final train dataset
X = subset(data, select = -c(Movie,OverseasGross,WorldwideGross))

# Final test dataset
y = subset(data2, select = -c(Movie,OverseasGross,WorldwideGross,DomesticGross))

str(X)
str(y)

plot(data$Budget, data$DomesticGross) 
plot(data$YearOfRelease, data$DomesticGross) 
plot(data$RunningTime, data$DomesticGross) 
plot(data$DomesticScreens, data$DomesticGross) 
plot(data$WeekendGross, data$DomesticGross) 

#hist(data$Budget)
#hist(data$WeekendGross)

# Let's build our first model.

linearmodel = lm(DomesticGross ~ .,data = X)
summary(linearmodel)
# This is giving me ridiculous numbers. I hate it!

# Let's build our regression model to predict our response variable i.e DomesticGross
pred = predict(linearmodel, y)
head(pred)
# As dreaded, yes the prediction is absurd. "I honestly don't line this one!" ;)


# I now decide to keep it simple and select only the continious independent variables.

linearmodel2 = lm(DomesticGross ~ YearOfRelease + Budget + RunningTime + DomesticScreens +
                     WeekendGross, data = X)
summary(linearmodel2)

# Well.. "I like this one!" ;)


y = subset(data2, select = c(YearOfRelease, Budget, RunningTime, DomesticScreens,
                                WeekendGross))
#summary(y)

pred = predict(linearmodel2, y)
pred


# Predicting Avengers: Infinity war's domestic Gross.
#X
#dim(X)

# Removing Avengers: Infinity war from the train dataset.
X_new = X[-c(13),]
dim(X_new)

y_new = X[c(13),]
y_new

y_new = subset(y_new, select = c(YearOfRelease, Budget, RunningTime, DomesticScreens,
                                 WeekendGross))
y_new

linearmodel_new = lm(DomesticGross ~ YearOfRelease + Budget + RunningTime + DomesticScreens +
                    WeekendGross, data = X_new)
summary(linearmodel_new)


pred_new = predict(linearmodel_new, y_new)
pred_new


# --------------- Part 2 - Predict WorldwideGross for Avengers: Endgame ------------------------

# Link for Part 2: https://www.linkedin.com/pulse/avengers-endgame-surpass-avatar-simple-regression-analysis-kumavat-1d


# Now let's use the ratio of domestic gross to worldwide gross to predict worldwide gross of Avengers: Endgame.

# First we check the current variables present in train datastet.
dim(X)
colnames(X)

# Now we append the OverseasGross variable from original dataset into our train dataset.

X$OverseasGross = data$OverseasGross
head(X)

# Clipping the redundant categorical variables.
X = subset(X, select = c(YearOfRelease, Budget, RunningTime, DomesticScreens, 
                         DomesticGross, WeekendGross, OverseasGross))

head(X)

linearmodel_final = lm(OverseasGross ~ YearOfRelease + Budget + RunningTime
                       + DomesticScreens + DomesticGross + WeekendGross, data = X)
summary(linearmodel_final)

y
pred

y$DomesticGross = pred
y

pred_final = predict(linearmodel_final,y)
pred_final

# 1553967291 +- 106900000



# Removing the Avengers: Infinity War record from train dataset.
X_new = X[-c(13),]

# Adding the Avengers: Infinity War record in test dataset.
y_new = X[c(13),]

linearmodel_new = lm(OverseasGross ~ YearOfRelease + Budget + RunningTime 
                     + DomesticScreens + DomesticGross + WeekendGross, data = X_new)
summary(linearmodel_new)

y_new
y_new = subset(y_new, select = c(YearOfRelease, Budget, RunningTime, 
                                 DomesticScreens, DomesticGross, WeekendGross))
y_new

pred_new = predict(linearmodel_new, y_new)
pred_new

# 1138390290 +- 98480000
# Actual -> 1369544272

colnames(X)

#--------------- Adding OverseasWeekend to the dataset ----------

data3 = read.csv("OverseasWeekend.csv")
head(data3)

X$OverseasWeekend = data3$OverseasWeekend

colnames(X)

linearmodel_final = lm(OverseasGross ~ YearOfRelease + Budget + RunningTime
                       + DomesticScreens + DomesticGross + WeekendGross
                       + OverseasWeekend, data = X)
summary(linearmodel_final)

y
y$OverseasWeekend = 859000000 # Official overseas weekend gross
# https://www.boxofficemojo.com/news/?id=4507

str(y)

pred_final = predict(linearmodel_final,y)
pred_final

# Overseas Gross for Avengers: Endgame: 2150095923 +- 94830000

# Let's predict OverseasGross for Avengers: Infinity War.

X_new = X[-c(13),]
y_new = X[c(13),]

linearmodel_new = lm(OverseasGross ~ YearOfRelease + Budget + RunningTime 
                     + WeekendGross + DomesticGross + DomesticScreens
                     + OverseasWeekend, data = X_new)
summary(linearmodel_new)

y_new

y_new = subset(y_new, select = c(YearOfRelease, Budget, RunningTime, 
                                 DomesticGross, WeekendGross, 
                                 DomesticScreens, OverseasWeekend))
#y_new

pred_new = predict(linearmodel_new, y_new)
pred_new

# 1227955171 +- 94360000 = 1322315171 which is less by almost 50 million from
# the actual value of 1369544272

plot(OverseasGross ~ OverseasWeekend, data = X)
abline(lm(OverseasGross ~ OverseasWeekend, data = X))
abline(lm(OverseasGross ~ OverseasWeekend, data = X_new), col = 'blue')

# As you can see from the plots, if we remove the top-right corner observation,
# the lm line gets a downward tilt, and hence the skewness in prediction.
# Anyways, let's move ahead with introducing more information in our dataset.

X_new = X_new[-c(15),]

plot(OverseasGross ~ OverseasWeekend, data = X[-c(16),])
abline(lm(OverseasGross ~ OverseasWeekend, data = X[-c(16),]))
abline(lm(OverseasGross ~ OverseasWeekend, data = X_new), col = 'red')


# Now let's quickly try to predict Avengers: Infinity War with the available data before we move ahead to predict Avengers: Endgame.

# y_new will remain the same
y_new

linearmodel_new = lm(OverseasGross ~ YearOfRelease + Budget + RunningTime 
                     + WeekendGross + DomesticGross + DomesticScreens
                     + OverseasWeekend, data = X_new)
summary(linearmodel_new)

pred_new = predict(linearmodel_new, y_new)
pred_new

# 1334826029 +- 92040000 which is perfect range for our actual value of 1369544272! Yayy!

# Now we can rest assured and go ahead with predicting the OverseasGross for Avengers: Endgame!

X
X = X[-c(16),]
#X

#y

linearmodel_final = lm(OverseasGross ~ YearOfRelease + Budget + RunningTime + DomesticScreens
                       + WeekendGross + DomesticGross + OverseasWeekend, data = X)
summary(linearmodel_final)

predict_final = predict(linearmodel_final,y)
predict_final

# 2452652300 +- 88320000 is the range for OverseasGross for Avengers: Endgame.
