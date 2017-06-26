library("memisc")
library("dplyr")
library("psych")
library("lmtest")
library("sjPlot")
library("sgof")
library("ggplot2")
library("foreign")
library("car")
library("hexbin")
library("devtools")
library("rlms")

h <- read.rlms("r21i_os25f.sav")
saveRDS(h, "r21i_os25f.rds")

# рост, вес, год рождения, пол
h2 <- select(h, qm1, qm2, qh6, qh5)
describe(h2)
# переименовываем переменные
h3 <- rename(h2, weight=qm1, height=qm2, sex=qh5, birthday_year=qh6)
# преобразуем год рождения в возраст
h3 <- mutate(h3, year=2012-birthday_year)
describe(h3)
summary(h3$sex)

# выборка
h4 <- filter(h3, sex=="МУЖСКОЙ")

# графики 
qplot(data=h4, height, weight)
qplot(data=h4, weight)


# TEST TEST TEST TEST
data <- diamonds
glimpse(data)
min(data$price)
data2 = filter(data, cut=="Premium")
model <- glm(data=data, price~carat)
summary(model)
linearHypothesis(model, "carat=0")
model2 <- glm(data=data, price~carat+x+y+table)
summary(model2)
model3 <- glm(data=data, price~y)
model4 <- glm(data=data, price~carat+y)
mtable(model3, model4)
model5 <- glm(data=data, price~carat+y+x)
summary(model5)
-1148.61-36.31*qt(0.95,53940-4)
