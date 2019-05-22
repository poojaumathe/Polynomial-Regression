# Import the position_salary data
dataset <- read.csv("C:/Users/Rahul/Downloads/Position_Salaries.csv")

dataset=dataset[2:3]

# Fitting Linear Regression to the dataset

lin_reg=lm(formula=Salary ~. , data=dataset)
summary(lin_reg)

# Fitting Polynomial Regression to the Salary dataset
#(In order to generate the polynomial term, we need to build a a new variable level3, level4)

# polynomial level3

dataset$level2=dataset$Level^2
dataset$level3=dataset$Level^3
poly_reg=lm(formula=Salary ~. , data=dataset)
summary(poly_reg)

# Visualizing the Linear Regression results

library(ggplot2)

g1=ggplot()+
  geom_point(aes(x=dataset$Level , y=dataset$Salary), 
             colour="blue")+
  geom_line(aes(x=dataset$Level, y=predict(lin_reg, newdata= dataset)),
            colour="pink")+
  ggtitle("Truth or Bluff(Linear Regression)")+
  xlab('Level')+
  ylab("Salary")
g1


# Predict the salary with Linear Regression prediction

y_linear_pred=predict(lin_reg, data.frame(Level=6.5))
y_linear_pred

# Visualizing the Poly Regression

# results with level 3

g2=ggplot()+
  geom_point(aes(x=dataset$Level , y=dataset$Salary), 
             colour="black")+
  geom_line(aes(x=dataset$Level, y=predict(poly_reg, newdata= dataset)),
            colour="red")+
  ggtitle("Truth or Bluff(Poly Regression)")+
  xlab('Level')+
  ylab("Salary")
g2

y_poly3_pred=predict(poly_reg, data.frame(Level=6.5, level2=6.5^2, level3=6.5^3))
y_poly3_pred


#results with level 4

dataset1=dataset
dataset1$level4=dataset1$Level^4
poly_reg1=lm(formula=Salary ~. , data=dataset1)
summary(poly_reg1)

g3=ggplot()+
  geom_point(aes(x=dataset$Level , y=dataset$Salary), 
             colour="purple")+
  geom_line(aes(x=dataset$Level, y=predict(poly_reg1, newdata= dataset1)),
            colour="pink")+
  ggtitle("Truth or Bluff(Poly Regression)")+
  xlab('Level')+
  ylab("Salary")
g3


y_poly4_pred=predict(poly_reg1, data.frame(Level=6.5, level2=6.5^2, level3=6.5^3, level4=6.5^4))
y_poly4_pred

# conclusion: As we can see the polynominal regession is better than the linear regression, 
#in addition, 4th polynomial regression is better than the 3rd polynomial regression. 
#We can make the final conclusion that the new employee told us the truth, 
#because the predicted value is 158862.5 dollars