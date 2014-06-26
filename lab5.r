
# We are still doing regression. Today, we will illustrate overfitting
# in the context of polynomial regression. Then we'll examine 
# multiple linear regression, and the problem of collinearity.

# 1) Nonlinear fits (in linear regression): Overfitting.
#
# As shown in class, linear regression is actually NOT linear when it
# comes to allowing nonlinear relationships between x and y. This is
# good news, because linear regression can fit any nonlinear data. But
# it's also bad news, because the ability to fit nonlinear data also allows
# for overfitting. In developing your own regression models of data it is
# important to assure that you are not overfitting the data, because such
# a model will have poor *predictive* capability. Toward the end of the
# quarter I will show you how to assess the *predictive* capability of a
# regression model. Here, let me first show you that linear regression
# *can* overfit (memorize) data.

   set.seed(12)                      # So that we all get the same answer.
   x = seq(0,0.9,0.1)                # Pick 10 x's between 0 and 1.
   y = x + rnorm(10,0,0.3)           # x and y are "truly" linear, plus error.
   plot(x,y)                         # Look at the data.

   lm.a = lm( y ~ x)                  # The simplest regression fit gives
   lines(x,lm.a$fitted.values)        # a decent answer.

   lm.a = lm(y ~ x + I(x^2) )         # What would a quadratic fit look like?
   lines(x,lm.a$fitted.values, col=2)     # (Use UP ARROW)

   lm.a = lm(y ~ x + I(x^2) + I(x^3) )   # A cubic?
   lines(x,lm.a$fitted.values, col=3)    # Note the fit is getting more curvy.

   lm.a = lm(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9))
   lines(x,lm.a$fitted.values, col=4)   # A 9^th order polynomial?

   summary(lm.a)                        # Check out the R2 !

# So, linear regression just overfit the data! As explained in class, this
# last model will have no predictive power. The picture looks different
# from what I draw in the lecture, because R just connects the points; in
# reality a 9th order polynomial is a smooth, though very wiggly, curve.

#########################################################################
# 2) Multiple regression and model comparison in terms of R-squared.

# Recall that SS_explained is expressed as R^2, and that the closer R^2 is 
# to 1, the "better" the fit. The closer to 0, the worse. But also keep 
# overfitting in mind.
# Also recall that the "rest" of SST is expressed as the std dev
# of the errors (also called standard dev about regression).
# Last time we used the hail data, without any nonlinear terms,
# to see what happens to R^2 and s_e. Here, we will see what 
# happens to R^2 and s_e as the model gets more complex:

   dat = read.table("http://www.stat.washington.edu/marzban/390/hail_dat.txt", header=T)
   size=dat[,3]  # Name the 3 columns in dat. BTW, size is in 100th-of-an-inch.
   rotate=dat[,2]
   diverg=dat[,1] 

                             # USE UP-ARROW
   lm.1 = lm(size ~ diverg)  # predicting size from divergence (simple regression)
   summary(lm.1)             # R^2 = 0.2719 , s_e = 75.99

   lm.2 = lm(size ~ rotate)  # predicting size from rotation (simple regression)
   summary(lm.2)             # R^2 = 0.2901 , s_e = 75.04

   lm.3=lm(size ~ diverg+rotate) # predicting size from both (multiple regression)
   summary(lm.3)             # R^2 = 0.3629 , s_e = 71.21

                             # multiple regression with interaction.
   lm.4 = lm(size ~ diverg + rotate + diverg:rotate)
   summary(lm.4)             # R^2 = 0.3745 , s_e = 70.69 

                             # multiple quadratic regression
   lm.5=lm(size~diverg + rotate + I(diverg^2) + I(rotate^2) )
   lm.5                      # Note: there are now *4* coefficients.
   summary(lm.5)             # R^2 = 0.3800 , s_e = 70.50

   lm.6=lm(size~diverg+rotate + I(diverg^2) + I(rotate^2) + diverg:rotate )
   summary(lm.6)             # R^2 = 0.3800 , s_e = 70.63

# Here is a discussion of all of the above results: It *seems* like
# - rotate is a better predictor of size than diverg.
# - The two of them together make for an even better model.
# - Quadratic terms for each, make the model even better, but not by much
#   (R^2 goes from 0.3628943 to 0.3799713).
# - An interaction term, without quadratic terms, gives a model that is
#   comparable to what we got from a quadratic model with no interaction.
# - Quadratic and interaction terms, together, "seem" to give the best model.

# At this stage in class, we don't have the proper framework for deciding
# which of the models is "best." That framework will be learned on the
# last day of the quarter! And even then, the framework will not pick out
# a unique model as the best; it may, for example, lead to the conclusions
# that all of the models we are looking at, above, are reallt equivalent,
# and that our data does not allow us to discriminate between them. THAT
# is in fact the correct way of phrasing the problem, i.e., does our data
# allow us to discriminate between different models? Anyway, for now, 
# we can use R^2 and s_e . Based on the above results, then, it seems that 
# rotate is a slightly better predictor than diverg. It also seems like the 
# two of them together make an even better model.

# I say "seem," because R^2 alone cannot tell which is the best model
# in a predictive sense, because of overfitting. In fact, note that R^2 
# never decreases as you add more and more terms to the regression model; 
# that's a form of overfitting too. The main question (which you can address
# only qualitatively at this point) is this: is the gain in R^2 big enough
# to warrant the new term, knowing that a new term can lead to overfitting.
# In this example, the gain from R^2=0.3799 to R^2=0.3800 is probably
# NOT worth the risk of overfitting. So, we should keep the simpler model. 
# That's called the principle of "Occam's Razor," which posits that one 
# should go with simpler things!

# Also, recall from last lab that the decomposition of SST into SS_explained 
# and SS_unexplained is done by anova(). For example, for the last model:

  anova(lm.6)

# In the anova() output, *each term* in the regression eqn is accompanined
# by an SS term. They are obtained from a *sequential analysis of variance.*
# In fact, these SS terms will change depending on the order of the terms
# in the regression equation! We have not discussed these SS terms, and so
# you can ignore the details.  Suffice it to say that SS_explained is the
# sum of all the SS terms, excluding the one from "Residuals" which is
# SS_unexplained (or SSE).

# Finally, recall from last lab, predicted values of y are produced this way:

  predict(lm.6) 

###########################################################################

# 3) Collinearity

# Another distressing issue that arises in *multiple* regression is 
# collinearity, i.e., a linear association *between the predictors* 
# themselves. One reason collinearity is distressing is that it renders
# the regression coefficients uninterpretable; i.e., a given beta can no 
# longer be interpreted as the average rate of change of y with respect 
# to a unit change in x. As shown in class, insisting on that kind of 
# interpretation, in the presense of collinearity, can lead to wrong 
# (or even absurd) conclusions. Collinearity also makes the predictions
# more uncertain (again, as shown in class), but here we will focus
# on the effect of collinearity on the regression coefficients.

# To that end, we'll make an R *function*, which is nothing but some lines
# of code intended to be used over and over again.
#
# The function (we'll call it make.fit(r) ) first makes data on x1, x2, and y,
# with collinearity (i.e., correlation between x1 and x2) equal to r. 
# (The way the collinear data is made here is a little more sophisticated than 
# what you were asked to do in your hw, but never mind that.)
# The function, then fits that data on y, x1, x2, and returns some stats 
# about the estimated regression coefficients. 

   ######################################################################
   # Here is our make.fit function:  Don't forget that you won't know if
   # each line of the function works properly until you call/use the whole
   # function. So, type it in a window that will allow you to correct it 
   # in case you typed-in something wrong. The TAs will show you how to
   # open a script window.
  
   make.fit = function(r)  # This function takes in r (i.e. cor between x1, x2;
   {                       # NOT between y and anything.)
   library(MASS)           # This library contains mvrnorm(); see below.
   set.seed(1)             # So, we all get the same answer.
   n=100           
   dat =  mvrnorm(n, rep(0, 2), matrix(c(1,r,r,1),2,2))
   x1 = dat[,1] ; x2 = dat[,2]
 
   # The R function mvrnorm(), above, takes a sample from a multivariate normal,
   # i.e., with multiple predictors. Don't worry about the details; just
   # note that r controls the correlation between x1 and x2; i.e., it measures
   # collinearity.
   
   y = 1 + 2*x1 + 3*x2 + rnorm(n,0,2)      # Make y, and add noise.
   dat = data.frame(x1,x2,y)      # Here is the whole data. Thanks to
   plot(dat)                      # data.frame in R, we can view all scatterplots.

   lm.1 = lm( y ~ x1 + x2)        # Here we fit a plane through the data.
   
#   return(lm.1)                   # returns the whole R object lm.1
   return( summary(lm.1) )        # returns only the summary results
#   return( summary(lm.1)$coeff )  # returns only the regression coefficients.
   }

                   ############################################
  
# Now, let's see what the data and the regression coefficients look like, for
# different amounts of collinearity.

   make.fit(0)              # No collinearity.
  
   # First look at the scatterplots. They should look like what we saw
   # in class.  This is what we would *ideally* expect, when x1 and x2 are
   # not correlated.
   # According to the returned values, the estimated regression coefficients 
   # are 1.0507, 2.0422, 2.8931 - very close to the "real" (or population)
   # answers: 1, 2, and 3. See the make.fit function, where we make y data.
   # Again, ask your TA, if you don't see what "1, 2, 3," I'm talking about.

   # Also, note "Std Error" for these coefficients. You will learn in Ch. 11,
   # where these come from. For now, suffice it to say that they tell us how
   # uncertain the estimates are. The bigger they are, the more uncertain we
   # are about their true values. Here, they are 0.2104, 0.2335, 0.21909 .
  
   # When we talk about "interpreting the regression coefficients," we meanx, 
   # e.g., the average change in y, when x1 changes by 1 unit, keeping x2
   # fixed, is about 2.0422 +- 0.2335 .
   
   make.fit(0.7)           # Some collinearity.
   
   # Now, again, look at the scatterplots, and then examine the 
   # regression coefficients and their uncertainty (Std. Err.). 
   # What do you see? Is the uncertainty increasing or decreasing?

   make.fit(0.9)           # Extreme collinearity.
   
   # And now?
 
   # And just for fun try this:
   
   make.fit(0.999)

   # When collinearity is extreme, not only the std. errors are huge, but
   # the estimated regression coefficients themselves (betas) are way off. 
   
   # So, as collinearity increases, the regression coefficients become
   # more uncertain, and so we are unable to interpret them, like we would
   # if there were no collinearity. As I said in class, the regression
   # coeffs can even end-up with signs that are physically impossible.
   
   # The regression eqn is still OK to use for predictions. But, of course,
   # the predictions will be less certain as well.

   ############################################################### 

# Note that in practice we don't control/adjust the data or the collinearity; 
# all we see are the scatterplots, and based on the scatterplots between the 
# predictors, we decide how much collinearity there is.
# For example, for the hail data:
 
  plot (dat) 

# In the scatterplots, note the collinearity in the data. If you don't
# know which scatterplot to look at, ask your TA.  As such, the # regression 
# coefficients are uninterpretable. But the regression model is still OK 
# for making predictions (as long as it doesn't overfit, of course).

##########################################################################

# 4) Plotting curved fits on a scatterplot.
#
# In part 1, above, we plotted polynomial fits, but the "curves" were
# just the result of connecting points with straight lines, and as a
# result, the "curves" did not look smooth. Getting a smoother looking
# fit on the scatterplot can be a bit tricky in R.  Here is one way:
#
# Suppose we pick a relatively simple quadratic model for the Hail data:

  lm.g = lm(size ~ rotate + I(rotate^2) )
  lm.g$coef                                 # Look at the regression coeffs.
  x = seq( min(rotate), max(rotate), .01)    # Make a fake x .
  y.fit = lm.g$coeff[1] + lm.g$coeff[2] * x + lm.g$coeff[3] * x^2
  plot(rotate,size)
  points(x , y.fit , col="red", type="l")

# Alternatively, a fancier way is as follows. You don't have to understand
# the R details! Just use it as a "black box" when you need it:
#
  x = matrix(seq(min(rotate),max(rotate),.01),byrow=T) # Make a fake x .
  colnames(x) = "rotate"                               # Necessary for predict().
  plot(rotate,size)
  lines(x, predict(lm.g, newdata=data.frame(x) ) , col=2)


