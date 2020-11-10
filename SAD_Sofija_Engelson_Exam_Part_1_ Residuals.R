#-------------------------Explaining Residuals----------------------------
#
# Leading question: What are residuals?
#
# Goal: contributing to Henrik's statistics wiki to help to understand better what 
#       exactly residuals are
#
# Sofija Engelson, 18.12.2019
#
# When I read the text about residuals in Henrik's statistics wiki 
# (https://sustainabilitymethods.org/index.php/Residuals) for the first time, I found it 
# hard to understand. I feel like some plots and examples could help to understand the 
# term "residuals" as well as the problem of non-normally distributed residuals that 
# Henrik discribes.
#
#---------------------------Prelimenaries------------------------------------

  library(ggplot2)

#---------------------------Introduction-------------------------------------
#
# It's says in the wiki that "residuals are the sum (of squares) of the deviance the 
# individual data points have from a perfectly fitted model". In order to understand 
# this sentence we need indivdual data points and a perfectly fitted model.
#
#---------------------------Generating data-------------------------------------
#
# I am generating two data sets:
# 1.

  set.seed (123)
  Err_1 <- data.frame("X1" = seq(1, 200), "X2" = rnorm(200, mean = 1:200, sd = 200))

# This data is called "Err_1" with an index "X1" running from 1 to 200 and a randomly 
# generated number "X2". The mean is increasing with the sample size. The standard 
# deviation is set at 200 and doesn't change with sample size.

# 2.

  Err_2 <- data.frame("X1" = seq(1, 200), "X2" = rnorm(200, mean = 1:200, sd = 1:200))

# This data is called "Err_2" with an index "X1" running from 1 to 200 and a randomly 
# generated number "X2". The mean is increasing with the sample size. The difference 
# between "Err_1" and "Err_2" is that the standard deviation increases with sample size.
#  
# I admit the distribution of the data is not randomly choosen, but this will become
# clear later.
#
#---------------------------Creating linear models-------------------------------------
#
# I am creating two linear models to fit my two data sets using the function "lm". Since
# the models include information about the predicted data points and the residuals, I 
# safe that information for each data point in my data sets.

# 1.
  model_Err_1 <- lm(X2 ~ X1, Err_1)
  Err_1$predicted <- predict(model_Err_1)
  Err_1$residuals <- residuals(model_Err_1)
  
# 2.
  model_Err_2 <- lm(X2 ~ X1, Err_2)
  Err_2$residuals <- residuals(model_Err_2)
  Err_2$predicted <- predict(model_Err_2)
  
#--------------------------Understanding residuals-------------------------------------

# Now let's calculate the difference between an individual data point and the predicted 
# to evaluate whether we get the same solution for the residuals as calculated by R in the
# previous step.

  head(Err_1)

  all.equal(Err_1$X2 [1] - Err_1$predicted [1], Err_1$residuals [1])

# Looks like this is accurate.
#  
# Now let's try to understand residuals visually for the first data set:

  ggplot(Err_1, aes(x = X1 , y = X2)) +
    geom_point() +
    geom_segment(aes(xend = X1, yend = predicted, color = "red")) +
    geom_smooth(method = lm, se = FALSE) + 
    theme_light() +
    theme(plot.title = element_text(hjust = 0.5), legend.position = "none") +
    ggtitle("Understanding normally distributed residuals")
    
  
# The red lines connecting the datapoint to the predicted line are the residuals.
# As we can see some residuals are smaller, some are bigger all through the index X1.
# There is no pattern to observe.

#--------------------------Normally distributed residuals---------------------------------

# Let's look at the residuals a little closer. Henrik writes "[residuals] should 
# be normally distributed, which can be checked through a histogram".

  hist(Err_1$residuals, main = paste("Histogram of residuals"), xlab = "Residuals", 
       freq = FALSE) 
  lines(density(Err_1$residuals))
  par(col="red")
  curve(dnorm(x, mean(Err_1$residuals), sd(Err_1$residuals)), add = TRUE)

  dev.off()

# The red line shows the normal distribution, the black line is the density function of
# our residuals. Looks pretty similiar to me. But let's make sure and do the Shapiro-Wilk-
# Test.
# H0: Residuals of Err_1 are normally distributed.
# H1: Residuals of Err_1 are not normally distributed.

  shapiro.test(Err_1$residuals)

# The p-value is 22.25%. That means we can not reject H0, so we assume it's true.
# Normally distributed residuals are an important requirement for doing further analysis 
# like t-tests, F-tests or ANOVA.
#
# Remark: If the residuals are normally distributed, they are also homoskedastic. "Homo-
# skedasticity" means that the residuals have the same variance given any values of the 
# explanatory variable.
#  
# Henrik also writes: "Residuals should be distributed like stars in the sky".

  ggplot(Err_1, aes(x = X1, y = residuals)) +
    geom_point(aes(shape = 8), color = "yellow", size = 2) +
    theme(panel.background = element_rect(fill = "darkblue"), 
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5)) +
        scale_shape_identity() +
        ggtitle("Stars in the sky") +
        ylab ("Residuals")
 
# Do you see the stars?
#
#--------------------------Non-normally distributed residuals-------------------------------
 
# Now that we have understood the term "residuals" and seen how residuals are supposed to 
# look, let's check out the problems Henrik is talking about. The first problem he mentions 
# is the residuals not being normally distributed.

  ggplot(Err_2, aes(x = X1 , y = X2))+
    geom_point() +
    geom_segment(aes(xend = X1, yend = predicted, color = "red")) +
    geom_smooth(method = lm, se = FALSE) +
    theme_light() +
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
    ggtitle("Understanding non-normally distributed residuals")

# Here we can see that with small index X1 the data points are close to the predicted line
# (small residuals). As the index gets bigger the data points are further away from the 
# predicted line (big residuals). This means that the variance of the residuals is not the
# same given any value of the explanatory variable, the variance increases with X1.
# That's the pattern mentioned in the wiki. In other words this violates the assumption of 
# "homoskedacity". So the residuals are heteroskedastic which already implies that they are 
# also not normally distributed. 
# But let's check it out anyway with the help of a histogram.

  hist(Err_2$residuals, main = paste("Histogram of residuals"), xlab = "Residuals", 
       freq = FALSE) 
  lines(density(Err_2$residuals))
  par(col="red")
  curve(dnorm(x, mean(Err_2$residuals), sd(Err_2$residuals)), add = TRUE)

  dev.off()
# Remark: dev.off () turns off the set color (= red) in the code above. 

# The red line shows the normal distribution, the black line is the density function of
# our residuals. That doesn't look so great. Just to make sure, let's do the Shapiro-Wilk-
# Test.
# H0: Residuals of Err_1 are normally distributed.
# H1: Residuals of Err_1 are not normally distributed.

  shapiro.test(Err_2$residuals)

# That's a very small p-value. Therefore we reject the H0 at almost any significance level.
# Our residuals are not normally distributed.