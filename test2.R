# Homework 6 (3.6/4.1) 
# 3.6 Poisson distributed if pmf is P(X = x) = p(x; μ) = [e^(-μ)μ^(x)] / x!
# x = 0, 1, 2, … to infinity and where μ > 0 and independent of each other
# E[X] = 𝜇 AND Var[X] = 𝜇 (where 𝜇 is the arrival rate)
# 4.1: Probability Density Functions (pdf’s) and Uniform Distribution
# Let X be a continuous RV with a pdf at f(x) such that we have P(A <= X <= B) = ∫ from a to b of f(x) dx
# to be valid: [ f(x) >= 0 for all x ] AND ALSO [ ∫ from -infinity to infinity of f(x) dx = 1 ]
# X is a uniform RV if its pdf is f(x) = { 1/(b-a) from a <= x <= b and 0 otherwise}
# E[x] = (1/2)(a+b) and Var[x] = (1/12)(b-a)^2
# P(X = constant) = 0

# 1.  A continuous random variable X with the following function.
#     f(x) = {
#            0.05x + 0.3 from 3 ≤ x ≤ 5
#            0		            otherwise
f <- function(x) {
  0.05 * x + 0.3
}
# 1a. Verify that the total area under the density curve is 1.
#     ∫ from 5 to 3 of 0.05x + 0.3 dx = 0.025x^2+0.3x from 5 to 3
#     = 2.125 − 1.125 = 1
# 1b. Calculate P(X ≤ 4): 
integrate(f, 3, 4)
# 1b - part b. How does this probability compare to P(X < 4): 
#             P(X ≤ 4) = P(X < 4)
#             In continuous distributions, the probability of the random variable 
#             being equal to any specific point is zero
# 1c. Calculate P(3.5 ≤ X ≤ 4.5): 
integrate(f, 3.5, 4.5)
# 1c - part b. Calculate P(4.5 < X).
integrate(f, 4.5, 5)$value
# 2. Suppose the reaction temperature X (in °C) has a uniform dist with A = −8 and B = 8.
# 2a. Compute P(X < 0): probability density function (PDF):
#     f(x) = 1 / (B − A) = 1 / [8 − (−8)] = 1 / 16 for −8 ≤ x ≤ 8
f <- function(x) {1/16}
integrate(Vectorize(f), -8, 0)
# 2b. Compute P(−4 < X < 4): 
integrate(Vectorize(f), -4, 4)
# 2c. Compute P(−6 ≤ X ≤ 7): 
integrate(Vectorize(f), -6, 7)
# 2d. For k satisfying −8 < k < k + 4 < 8, compute P(k < X < k + 4): 
#     uniform distribution probability: P(k < X < k+4) = ∫ from k to k+4 of f(x)dx
#     ∫ from k to k+4 of 1 / 16dx = 1/16 |from k to k+4 = 1 / 4
# 3. A college professor never finishes his lecture before the end of the hour 
#    and always finishes his lectures within 2 min after the hour. 
#    Let X = the time that elapses between the end of the hour and the end of the lecture 
#    and suppose the pdf of X is as follows.
#    f(x) = {
#          kx^2 from 0 ≤ x ≤ 2
#          0	    	otherwise
# 3a. Find the value of k:  
#     ∫ from 0 to 2 of kx^2 dx = 1
#     bring k out of the integral ∫ from 0 to 2 of x^2dx = x^3 / 3 | 0 to 2
#     k * 8/3 = 1
#     k = 3/8
# 3b. What is the probability that the lecture ends within 1 min of the end of the hour?
f <- function(x) {
  (3/8) * x^2
}
integrate(f, 0, 1)
# 3c. What is the probability that the lecture continues beyond the hour for between 15 and 30 sec?
integrate(f, 0.25, 0.5)
# 3d. What is the probability that the lecture continues for at least 30 sec beyond the end of the hour?
1 - integrate(f, 0, 0.5)$value
# 4. Poisson distribution
#    The expected number of failures is 1. Then X, the number of failures, has a Poisson distribution with 𝜇 = 1
# 4a. Obtain P(X ≤ 5): 
ppois(5, 1)
# 4b. Determine P(X = 1) from the pmf formula: 
dpois(1, 1)
# 4b - part b. Determine P(X = 1) from the Cumulative Poisson Probabilities table
ppois(1, 1) - ppois(0, 1)
# 4c. Determine P(1 ≤ X ≤ 4): 
ppois(4, 1) - ppois(0, 1)
# 4d.  What is the probability that X exceeds its mean by more than one sd?
#      σ = sqrt(μ) = sqrt(1) = 1
#      μ + σ = 2
#      P(X > 2) = 1 − P(X ≤ 2)
1 - ppois(2, 1)
# 5. Poisson distribution for X. Suppose that 𝜇 = 4.
# 5a. Compute both P(X ≤ 4) and P(X < 4): 
ppois(4, 4)
ppois(3, 4)
# 5b. Compute P(4 ≤ X ≤ 7) : 
ppois(7, 4) - ppois(3, 4)
# 5c. Compute P(7 ≤ X).
1 - ppois(6, 4)
# 5d. What is the probability that the number of anomalies does not exceed the mean by more than one sd?
#     σ = sqrt(μ) = sqrt(4) = 2
#     μ + σ = 4 + 2 = 6
#     P(X ≤ 6)
ppois(6, 4) 
# 6. Poisson process with rate𝛼 = 10 per hour
# 6a. Compute the probability that exactly eleven requests are received during a particular 2-hour period
#     10 per hour * 2 hours = 20 mean 
dpois(11, 20)
# 6b. If the operators of the towing service take a 30 min break for lunch, 
#     what is the probability that they do not miss any calls for assistance? 
dpois(0, 5)
# 6c. How many calls would you expect during their break? 
#     5 calls per 30 minutes from previous answer
# 7. The error involved in making a certain measurement is a continuous rv X with the following pdf.
#   f(x) = {
#          0.09375(4 − x2) from −2 ≤ x ≤ 2
#          0	                  otherwise
f <- function(x) {
  0.09375 * (4 - (x^2))
}
# 7b. Compute P(X > 0) : 
integrate(f, 0, 2)
# 7c. Compute P(−1 < X < 1): 
integrate(f, -1, 1)
# 7d. Compute P(X < −1.2 or X > 1.2)
integrate(f, -2, -1.2)$value + integrate(f, 1.2, 2)$value
# 8. The actual tracking weight of a stereo cartridge that is set to track at 3g 
#    on a particular changer can be regarded as a continuous rv X with the following pdf.
#    f(x) = {
#           k[1 − (x − 3)2] from 2 ≤ x ≤ 4
#           0	    	             otherwise
# 8a. Find the value of k: 3/4
# 8b. What is the probability that the actual tracking weight is greater than the prescribed weight:
f <- function(x) {
  (3/4) * (1 - (x - 3)^2)
}
integrate(f, 3, 4)
# 8c. What is the probability that the actual weight is within 0.4 g of the prescribed weight?
#     P(2.6 < X < 3.4)
integrate(f, 2.6, 3.4)
# 8d. What is the probability that the actual weight differs from the prescribed weight by more than 0.55 g?
#     P(X < 2.45 or X > 3.55) = P(X < 2.45) + P(X > 3.55)
integrate(f, 2, 2.45)$value + integrate(f, 3.55, 4)$value
# 9. Uniform distribution with A = 0.20 and B = 4.25 for the diameter X 
# 9a. Determine the pdf of X: 
#     f(x) = {
#           1 / 4.05 from 0.2 ≤ x ≤ 4.25
#           0	    	             otherwise
# 9b. What is the probability that diameter exceeds 2 mm?
f <- function(x) {1/4.05}
integrate(Vectorize(f), 2, 4.25)
# 9c. What is the probability that diameter is within 2 mm of the mean diameter?
#     μ = (0.2 + 4.25) / 2 = 4.45 / 2 = 2.225
#     2.225 + - 2
#     P(0.225 ≤ X ≤ 4.225)
A <- 0.20
B <- 4.25
mu <- (A + B) / 2
((mu + 2) - (mu - 2)) / (B - A)
# 9d. For any value a satisfying 0.20 < a < a + 2 < 4.25, what is P(a < X < a + 2)?
#     uniform distribution probability: P(a < X < a + 2) = ∫ from a to a+2 of f(x)dx
#     ∫ from a to a+2 of 1 / 4.05dx = 1/4.05 |from a to a+2 = 2 / 4.05

# Homework 7 (4.2 and Ch 12) 
# 4.2 CDF for a continuous RV X is F(x) = P(X ≤ x = ∫ from negative infinity to x of f(y)dy
# CDF of a uniform RV x is: ∫ from negative infinity to x of f(y)dy = ∫ from a to x of 1/(b-a)dy 
#          = y / (b-a) from a to x = [x / (b-a)] - [a/(b-a)] = (x-a) / (b-a)
#          F(x) = piecewise function { 0 where x < a,  (x-a) / (b-a) where a ≤ x ≤ b, 1 where x > b
# CDFs for Probabilities: P(X > a) = 1 - P(X ≤ a) = 1 - F(a)
#                         P(a ≤ X ≤ b) = P(x ≤ b) - P(x ≤ a) = F(b) - F(a)
# To get the PDF from the CDF, derive F(x): 
#                From uniform: F’(x) = d/dx of (x-a)/(b-a) = d/dx of [x/(b-a)] - [a/(b-a)] = [1/(b-a)] - 0
# μsubx = E[X] = ∫ from negative infinity to infinity of x * f(x) dx
# Var[X] = E[X^2] - E[X]^2
# 12.1: Simple Linear Regression Model
# Relationship between x and y: y = beta0 + (beta1 * x)
# Independent x is the predictor, dependent y is the response
# y = beta0 + (beta1 * x) + epsilon
# epsilon is a normally dist RV with E[epsilon] = 0 and Var[epsilon] = sigma^2
# epsilon = residuals -> height of point - height of line = ysubi - [beta0Hat + (beta1Hat * xsubi)]
# ysubi is the observed and [beta0Hat + (beta1Hat * xsubi)] is the predicted
# Sum of Squared Epsilon (SSE) = Sum of Squared Residuals (RSS) -> f(beta0Hat, beta1Hat) = Capital sigma from i=1 to n of [ysubi - [beta0Hat + (beta1Hat * xsubi)]^2
# Minimized values of beta0Hat and beta1Hat found by d RSS / d beta0Hat = Capital sigma of 2 (ysubi - beta0Hat - (beta1Hat * xsubi)*(-1) and setting = 0 AND THEN d RSS / d beta1Hat = Capital sigma of 2 (ysubi - beta0Hat - (beta1Hat * xsubi)*(-xsubi) and setting = 0
# Which results in: n*beta0Hat + Cap sigma(xsubi)beta1Hat =  Cap sigma (ysubi) AND THEN Cap sigma(xsubi)beta0Hat + Cap sigma(xsubi^2)beta1Hat = Cap sigma (xsubi*ysubi)
x <- c(48, 61, 65, 70, 70, 79, 95, 100, 114, 118, 124, 127, 140, 140, 140, 150, 152, 164, 198, 221)
y <- c(38, 63, 52, 67, 84, 79, 93, 106, 117, 116, 127, 114, 134, 139, 142, 170, 149, 154, 200, 215)
plot(x, y)
xbar <- mean(x)
ybar <- mean(y)
b1 <- sum((x-xbar)*(y-ybar)) / sum((x-xbar)^2); b1 # slope, equals SsubXY / SsubXX
b0 <- (sum(y) - b1 * sum(x)) / length(x); b0 # intersect, equals ybar - (beta1Hat*xbar)
model <- lm(y ~ x)
coef(model) # intersect, slope
summary(model)
abline(model, col="green", lwd=2)
rss <- sum((y - predict(model))^2); rss
tss <- sum((y - ybar)^2); tss
1 - (rss/tss)
# Residual standard error: 8.447 is the estimate for sigma^2 aka var (formula: RSS / (n-2))
# Multiple R-squared:  0.9696 where R^2 is the prop or var in the response, aka coeff of determination (formula: R^2 = 1 - (RSS / TSS) and closer to 1 is better)
# total sum of squares (TSS) = total amount of var in response = sigma[(ysubi - ybar)^2]
# R = [ Cap sigma(xsubi - xbar)(ysubi - ybar) ] / [ sqrt(Cap sigma(xsubi - xbar)^2) * sqrt(Cap sigma(ysubi - ybar)^2) ] which simplifies to SsubXY / [ sqrt(SsubXX) * sqrt(SsubYY) ]
# Properties of R: -1 <= r <= 1
#                  r = 1 if x and y are perfectly positively correlated and -1 if negatively correlated
#                  r = 0 means no linear correlation
cor(x, y)

# 1b. Does there appear to be a very strong relationship between the two types 
#     of concentration measurements? Do the two methods appear to be measuring roughly the same quantity?
#     The points fall very close to a straight line with an x-intercept of approximately 
#     0 and a slope of about 1. This suggests that the two methods are producing substantially the same concentration measurements.
# 2. x = temperature and y = elongation (%) at failure
# 2a. Construct a scatterplot in which the axes intersect at (0, 0).
#     Mark 0, 20, 40, 60, 80, and 100 on the horizontal axis 
#     and 0, 50, 100, 150, 200, and 250 on the vertical axis
x <- c(0, 59, 63, 68, 72, 74, 78, 83)
y <- c(0, 118, 182, 247, 208, 197, 135, 132)
plot(x, y) 
# 2b. Construct a scatterplot in which the axes intersect at (55, 100),
#     as was done in the cited article. 
x <- c(55, 59, 63, 68, 72, 74, 78, 83)
y <- c(100, 118, 182, 247, 208, 197, 135, 132)
plot(x, y) 
# Does this plot seem preferable to the one in part (a)? Explain your reasoning.
#      The plot in part (a) is more crowded than the plot in part (b) because of the axes range. 
#      If data points are crowded it can be difficult to see a relationship with the data. 
# 3. An article on estimating 28-day strength of concrete considered regressing 
#    y = 28-day standard-cured strength (psi) against x = accelerated strength (psi). 
#    Suppose the equation of the true regression line is y = 1,790 + 1.5x.
# 3a. What is the expected value (in pounds per square inch) of 28-day strength when accelerated strength = 2,530 psi
y <- 1790 + 1.5*2530; y
# 3c. By how much can we expect 28-day strength (in pounds per square inch) to change when accelerated strength increases by 90 psi
psi <- 1.5*90; psi
# 4. tank temperature (x) and efficiency ratio (y)
x <- c(171, 173, 174, 175, 175, 176, 177, 178, 181, 181, 181, 181, 181, 182, 182, 183, 183, 183, 183, 185, 185, 186, 187, 189)
y <- c(0.88, 1.41, 1.46, 0.95, 1.03, 0.98, 1.08, 1.82, 1.35, 1.58, 1.59, 2.23, 2.13, 0.90, 1.35, 0.88, 1.83, 1.96, 2.78, 1.53, 2.60, 3.10, 1.89, 2.98)
plot(x, y)
# 4a. Determine the equation of the estimated regression line
model <- lm(y ~ x)
coef(model) # intersect, slope
summary(model)
# 4b. Calculate a point estimate for true average efficiency ratio when tank temperature is 183
y <- -15.3810 + 0.0945*183; y
# 4c. Calculate the values of the residuals from the least squares line for the four observations for which temperature is 183
#     (183, 0.88) 
epsilon <- 0.88 - 1.9125; epsilon
#     (183, 2.78)
epsilon <- 2.78 - 1.9125; epsilon
# Why do they not all have the same sign? 
#     These residuals do not all have the same sign because in the cases of the first pair, the observed efficiency ratios were smaller than the predicted value and then larger than the predicted value.
# 4d. What proportion of the observed variation in efficiency ratio can be attributed to the simple linear regression relationship between the two variables?
#     r^2 = 1 - (sse / sst)
sse <- syy - b1Hat*sxy; sse
sst <- syy; sst
rsquared <- 1 - (sse / sst); rsquared
# 5. y = axial strength (MPa) on x = cube strength (MPa)
x <- c(112.3, 97.0, 92.7, 86.0, 102.0, 99.2, 95.8, 103.5, 89.0, 86.7)
y <- c(75.3, 71.5, 57.4, 49.2, 74.6, 73.5, 67.9, 59.2, 58.1, 48.9)
# 5a. Obtain the equation of the least squares line
sum_x <- sum(x); sum_x
sum_y <- sum(y); sum_y
sum_x2 <- sum(x^2); sum_x2
sum_y2 <- sum(y^2); sum_y2
sum_xy <- sum(x*y); sum_xy
sxx <- sum_x2 - ((sum_x^2) / length(x)); sxx
syy <- sum_y2 - ((sum_y^2) / length(y)); syy
sxy <- sum_xy - ((sum_x*sum_y) / length(x)); sxy
b1Hat <- sxy / sxx; b1Hat
b0Hat <- (sum_y / length(x)) - (b1Hat*(sum_x / length(x))); b0Hat
# y <- -31.0429 + 0.9812x
# Interpret the slope. 
#      A one MPa increase in cube strength is associated with an increase in the predicted axial strength equal to the slope.
# 5b. Calculate the coefficient of determination
sse <- syy - b1Hat*sxy; sse
sst <- syy; sst
rsquared <- 1 - (sse / sst); rsquared
# Interpret the coefficient of determination. 
#      The coefficient of determination is the proportion of the observed variation in 
#      axial strength of asphalt samples of this type that can be attributed to its linear relationship with cube strength.
# 5c. Calculate an estimate of the error standard deviation 𝜎 in the simple linear regression model
sdError <- sqrt(sse / (length(x) - 2)); sdError
# Interpret the estimate of the error standard deviation 𝜎 in the simple linear regression model. 
#      The model's prediction for axial strength will typically differ from the specimen's 
#      actual axial strength by an amount within one error standard deviation.
# 6. x = burner-area liberation rate (MBtu/hr-ft2) and y = NOx emission rate (ppm)
x <- c(100, 125, 125, 150, 150, 200, 200, 250, 250, 300, 300, 350, 400, 400)
y <- c(150, 140, 180, 210, 190, 320, 280, 400, 430, 440, 390, 600, 610, 670)
# 6a. Obtain SSE for the data from the defining formula SSE = (yi − ŷi)^2.
sum_x <- sum(x); sum_x
sum_y <- sum(y); sum_y
# xbar <- sum_x / length(x); xbar
# ybar <- sum_y / length(y); ybar
sum_x2 <- sum(x^2); sum_x2
sum_y2 <- sum(y^2); sum_y2
sum_xy <- sum(x*y); sum_xy
sxx <- sum_x2 - ((sum_x^2) / length(x)); sxx
syy <- sum_y2 - ((sum_y^2) / length(y)); syy
sxy <- sum_xy - ((sum_x*sum_y) / length(x)); sxy
b1Hat <- sxy / sxx; b1Hat
b0Hat <- (sum_y / length(x)) - (b1Hat*(sum_x / length(x))); b0Hat
# b0Hat <- (ybar) - (b1Hat*xbar); b0Hat
sse <- sum_y2 - (b0Hat * sum_y) - (b1Hat * sum_xy); sse
# Rounded to two decimal places the value from the computational formula is equal to the defining formula. 
#         Another formula for SSE = Syy - b1Hat*Sxy
# 6b. Calculate the value of total sum of squares
sst <- sum_y2 - ((sum_y^2) / length(y)); sst # NOTE THIS IS ALSO SYY
# Does the simple linear regression model appear to do an effective job of explaining variation in emission rate?
# Assume that a value of r2 greater than 0.85 is large enough to consider the model successful.
sse <- syy - b1Hat*sxy; sse
rsquared <- 1 - (sse / sst); rsquared
# Since r2 = 0.96, the simple linear regression model appears to do an effective job of explaining variation in emission rate. 
# 7. The article "Behavioural Effects of Mobile Telephone Use During Simulated Driving"† 
#    reported that for a sample of 20 experimental subjects, the sample correlation coefficient 
#    for x = age and y = time since the subject had acquired a driving license (yr) was 0.97. 
#    Why do you think the value of r is so close to 1?
# The older a subject is, the more time since they acquired their license.
# 8. Let X denote the amount of time a book on two-hour reserve is actually checked out, 
#    and suppose the cdf is the following: 
#    F(x) = { piecewise
#            0        where x < 0
#            x^2 / 25 where 0 ≤ x < 5 
#            1        where 5 ≤ x
# 8a. Calculate P(X ≤ 2): 
#     Since 2 falls between 0 - 5, use x^2 / 25
#     2^2 / 25 = 4/25 = 0.16
# 8b. Calculate P(1.5 ≤ X ≤ 2): 
#     (1.5^2) / 25 = 0.09
#     0.16 - 0.09 = 0.07
# 8c. Calculate P(X > 2.5): 
#     2.5^2 / 25 = 0.25
#     1 - 0.25 = 0.75
# 8d. What is the median checkout duration mu tilde? [solve 0.5 = F(mu tilde)].
#     0.5 = mu tilde^2 / 25
#     0.5 * 25 = 12.5
#     sqrt(12.5) = 3.535534 = mu tilde
# 8e. Obtain the density function f(x). 
#     f(x) = F'(x) = { 
#                      (2/25)x where 0 ≤ x < 5
#                      0             otherwise
# 8f. Calculate E(X). NOTE: remember E(X) = ∫ from 0 to 5 of x*f(x)dx
#     Add another x to to function
f <- function(x){
  (2/25)*x^2
}
integrate(f, 0, 5) 
ex <- integrate(f, 0, 5)$value; ex
# 8g. Calculate V(X) and 𝜎x: 
g <- function(x){(2/25)*x^3}
ex2 <- integrate(g, 0, 5)$value; ex2 # this is E[X^2]
varx <- ex2 - (ex^2); varx
sdx <- sqrt(varx); sdx
# 8h. If the borrower is charged an amount h(X) = X^2 when checkout duration is X, 
#     compute the expected charge E[h(X)].
#     Playing with the equation E[h(X)] where h(X) = X^2 then E[X^2] which we found as 12.5
# 9. Uniform distribution on the interval (7.5, 18)
# 9a. What are the mean and variance of depth?
meanX <- (7.5 + 18) / 2; meanX
varx <- (18 - 7.5)^2 / 12; varx
sdx <- sqrt(varx); sdx
# 9b. What is the cdf of depth?
#     F(x) = { piecewise
#              0                            x < 7.5
#              (x - 7.5) / (18 - 7.5) where 7.5 ≤ x < 18
#              1		                        18 ≤ x
# 9c. What is the probability that observed depth is at most 10?
#     (10 - 7.5) / (18 - 7.5) = 0.2380952
# 9d. What is the probability that observed depth is between 10 and 15? 
#     (15 - 7.5) / (18 - 7.5) = 0.7142857
#     0.7142857 - 0.2380952 = 0.4761905
# 9e. What is the probability that the observed depth is within 1 sd of the mean?
#     12.75 + 3.031089 = 15.78109
#     12.75 - 3.031089 = 9.718911
#     P(9.72 < X < 15.78)
#     (15.78109 - 7.5) / (18 - 7.5) = 0.7886752
#     (9.718911 - 7.5) / (18 - 7.5) = 0.2113249
#     0.7886752 - 0.2113249 = 0.5773503
# 9f. What is the probability that the observed depth is within 2 sd of the mean?
#     12.75 + (3.031089 * 2) = 18.81218
#     12.75 - (3.031089 * 2) = 6.687822
#     Refer back to the piecewise: 18.8 falls into 1 and 6.7 falls into 0
#     1 - 0 = 1

# Homework 8 (4.3/4.4)
# 4.3: Normal Dist
# A RV X is Normal Dist with mean μ and SD σ, where μ is finite and σ > 0, if the PDF of X is: f(x; μ, 𝛔) = [1 / sqrt(2pi) * σ] * e ^ ( [-(x-μ)2]/[2σ2] )
# The peak is at μ
# Symmetric about μ
# μ determines location, σ determines the spread/flatness
# E[x] = μ and Var[x] = σ^2
# f(x) gets close to 0, but never equals it
# Points of inflection (where the graph changes direction) are at μ +/- σ
# Standard Normal Distribution is with μ = 0 and σ = 1
# The area under the curve is always 1 
# Z is the RV for this distribution, with: Z = (x - μ) / σ where x is the raw score
# Z has a pdf of: f(z; 0, 1) = [ 1 / sqrt(2pi) ] * e^[(-z^2) / 2], where z is defined negative infinity < z < infinity 
# The CDF for Z is Φ(z) = P(Z ≤ z) = ∫ from negative infinity to z of f(y; 0, 1)dy
# If going from a z-score to a raw score, then x = Zσ + μ 
# zsub𝛂 means the area to the right of zsub𝛂 is 𝛂
# 4.4: Exponential Distribution 
# X is an exponential RV, it has a pdf of: f(x; λ) = { λe^(-λx) where x ≥ 0 and 0 otherwise
# Where λ > 0 is the scale parameter, which is the same as (1/β)*e^(-x/β) where β = 1/ λ
# exponential dist is the probability dist of time between events in a Poisson Process, ie a process where events occur independently and with a constant rate
# E[X] = 1 / λ and Var[X] = 1 / λ^2
# E[3x + 5] = E[3x] + E[5] = 3E[x] + 5 = 3*(1/lambda) + 5 remembering the function for E[X] = 1 / λ

# 1. Let Z be a standard normal random variable and calculate the following probabilities, 
#    drawing pictures wherever appropriate
# 1a. P(0 ≤ Z ≤ 2.01): 
pnorm(2.01) - pnorm(0) 
# 1d. P(−2.90 ≤ Z ≤ 2.90): 
pnorm(2.90) - pnorm(-2.90)
# 1e. P(Z ≤ 1.67): 
pnorm(1.67)
# 1f. P(−1.45 ≤ Z): 
1 - pnorm(-1.45)
# 1j. P(|Z| ≤ 2.50): 
pnorm(2.50) - pnorm(-2.50)
# 2. In each case, determine the value of the constant c that makes
#    the probability statement correct
# 2a. Φ(c) = 0.9826
qnorm(0.9826)
# 2b. P(0 ≤ Z ≤ c) = 0.3106
#     P(Z ≤ 0) = 0.5, since the mean of the standard normal distribution is 0.
#     P(0 ≤ Z ≤ c) = P(Z ≤ c) − P(Z ≤ 0), which is:
#     P(Z ≤ c) = P(0 ≤ Z ≤ c) + P(Z ≤ 0) = 0.3106 + 0.5 = 0.8106
qnorm(0.8106)
# 2c. P(c ≤ Z) = 0.1314
qnorm(1 - 0.1314)
# 2d. P(−c ≤ Z ≤ c) = 0.6424
#     P(−c ≤ Z ≤ c) = CDF:P(−c ≤ Z ≤ c) = P(Z ≤ c) − P(Z ≤ −c)
#     Then, because of the symmetry around 0: P(Z ≤ −c) = 1 − P(Z ≤ c)
#     So P(Z ≤ c) − (1 − P(Z ≤ c)) = 0.6424 
#     Which simplifies to: 2P(Z ≤ c) − 1 = 0.6424
#     Solving for P(Z ≤ c): 2P(Z ≤ c) = 1 + 0.6424 = 1.6424
#     And P(Z ≤ c) = 1.64242 = 0.8212
qnorm(0.8212)
# 2e. P(c ≤ |Z|) = 0.0128
#     2(1 − P(Z ≤ c)) = 0.0128
#     1 − P(Z ≤ c) = 0.01282 = 0.0064
#     P(Z ≤ c)= 1 − 0.0064 = 0.9936
qnorm(0.9936)
# 3. Find the following percentiles for the standard normal distribution. 
#    Interpolate where appropriate.
# 3a. 91st: 
qnorm(0.91)
# 3e. 11th: 
qnorm(0.11)
# 4. Determine z𝛼 for the following of 𝛼.
# 4a. 𝛼 = 0.0099
qnorm(1 - 0.0099)
# 4c. 𝛼 = 0.657
qnorm(1 - 0.657)
# 5. Suppose the force acting on a column that helps to support a building is a 
#    normally distributed random variable X with mean 13.0 kips and sd 1.50 kips
# 5a. P(X ≤ 13): 
pnorm(13, 13, 1.5) # value, mean, sd
# 5c. P(X ≥ 5.5): 
1 - pnorm(5.5, 13, 1.5)
# 5e. P(|X − 13| ≤ 2) = P(13−2 ≤ X ≤ 13+2): 
pnorm(15, 13, 1.5) - pnorm(11, 13, 1.5)
# 6. Suppose the maximum speed of a moped is normally distributed with mean 46.8 km/h 
#    and sd 1.75 km/h.
# 6a. What is the probability that maximum speed is at most 50 km/h?
pnorm(50, 46.8, 1.75)
# 6b. What is the probability that maximum speed is at least 48 km/h? 
1 - pnorm(48, 46.8, 1.75)
# 6c. What is the probability that maximum speed differs from the mean by at most 1.5 sd?
pnorm(46.8+1.5*1.75, 46.8, 1.75) - pnorm(46.8-1.5*1.75, 46.8, 1.75)
# 7. normally distributed with 𝜇 = 0.50 and 𝜎 = 0.06
# 7a. What is the probability that the concentration exceeds 0.60? 
1 - pnorm(0.6, 0.5, 0.06)
# 7b. What is the probability that the concentration is at most 0.30?
pnorm(0.3, 0.5, 0.06)
# 7c. How would you characterize the largest 5% of all concentration values? 
qnorm(0.95, 0.5, 0.06)
# 8. normally distributed with mean 27 mm and sd 7.6 mm.
# 8a. What is the probability that defect length is at most 20 mm? Less than 20 mm? 
pnorm(20, 27, 7.6) # they're the same 
# 8b. What is the 75th percentile of the defect length distribution, 
#     the value that separates the smallest 75% of all lengths from the largest 25%?
qnorm(0.75, 27, 7.6)
# 8c. What is the 15th percentile of the defect length distribution?
qnorm(0.15, 27, 7.6)
# 8d. What values separate the middle 80% of the defect length distribution from the smallest 10% and the largest 10%?
qnorm(0.10, 27, 7.6)
qnorm(0.90, 27, 7.6)
# 9. If bolt thread length is normally distributed, what is the probability that the thread length of a randomly selected bolt is 
# 9a. Within 1.5 SDs of its mean value?
pnorm(10+1.5*2, 10, 2) - pnorm(10-1.5*2, 10, 2) # example mean and sd values
# 9b. Farther than 1.7 SDs from its mean?
1 - (pnorm(10+1.7*2, 10, 2) - pnorm(10-1.7*2, 10, 2))
# 9c. Between 1 and 2 SDs from its mean value?
between1 <- pnorm(10+1*2, 10, 2) - pnorm(10-1*2, 10, 2)
between2 <- pnorm(10+2*2, 10, 2) - pnorm(10-2*2, 10, 2)
between2 - between1
# 10. Let X = the time between two successive arrivals at the drive-up window of a local bank. 
#     If X has an exponential distribution with 𝜆 = 1, (which is identical to a standard gamma distribution with 𝛼 = 1), 
# 10a. The expected time between two successive arrivals: 
# E[x] for exponential distribution = 1 / 𝜆 = 1
# 10b. The standard deviation of the time between successive arrivals
# Var[X] = 1 / (λ^2)
sqrt(1)
# 10c. P(X ≤ 4): 
pexp(4, 1)
# 10d. P(1 ≤ X ≤ 5): 
pexp(5, 1) - pexp(1, 1)
# 11. X has an exponential distribution with parameter 𝜆 = 0.01437.
# 11a. What is the probability that the distance is at most 100 m? Between 100 and 200 m?
pexp(100, 0.01437)
pexp(200, 0.01437) - pexp(100, 0.01437)
# 11b. What is the probability that distance exceeds the mean by more than 2 sd?
expX <- 1 / 0.01437; expX
varX <- 1 / (0.01437^2)
sqrt(varX)
exp(-0.01437 * (69.58942+2*69.58942))
# 11c. What is the value of the median distance?
medianDist <- log(2) / 0.01437; medianDist # formula for median of exp dist
# 12. Exponential distribution with mean 2.855
# 12a. least 2 hours? At most 3 hours? Between 2 and 3 hours?
lambdaX <- 1 / 2.855; lambdaX
1 - pexp(2, lambdaX)
pexp(3, lambdaX)
pexp(3, lambdaX) - pexp(2, lambdaX)
# 12b. What is the probability that rainfall duration exceeds the mean by more than 3 sd?
exp(-lambdaX * (2.855+3*2.855))
# 12c. What is the probability that it is less than the mean by more than one sd?
#      Probability that the duration is less than: μ − σ = 2.855 − 2.855 = 0
#      and P(X < 0) = 0

# Homework 9 (5.3/5.4/7.1)
# 5.3 and 5.4: Sampling Distribution of the Mean
# μ = (1 + 2 + ... + n) / n, σ = sqrt[ Σ(x-μ)2 / N ]
# Central Limit Theorem (CLT): xbar is approximately normal if: [Population is normal] OR [n ≥ 30 (or sometimes 40 in the book)]
# μxbar = μ and σxbar = σ / sqrt(n) and σxbar is also the standard error of the mean 
# 7.1: Properties of Confidence Intervals (CIs)
# C% of the CIs constructed will contain the true value of the parameter
# Format: Point estimate +/- Margin of Error (ME) where Point Estimate is xbar 
# Margin of Error formula = Critical Value * Standard Error where Critical Value is Zα/2 and Standard Error is σ / sqrt(n)
# for the CI for μ with σ known
# ex. SRS of 31 typists with xbar = 80 cm. Assume normal and σ = 2 cm. Find a 95% CI for μ
qnorm(0.95+0.025) # gives the z score (100% - 95% = 0.05 and then /2 to get each tail)
80 + c(-1, 1)*qnorm(0.95+0.025)*(2 / sqrt(31))

# 1. mean and sd are 70 GPa and 1.6 GPa
# 1a. If X is the sample mean for a random sample of n = 16 sheets, 
#     where is the sampling distribution of X centered, and what is the sd of the X distribution? 
# sample mean is the same as the population mean
# sd of sample = sd of pop / sqrt(n)
sepop <- 1.6 / sqrt(16); sepop
# 1b. a sample size of n = 256 sheets
sepop <- 1.6 / sqrt(256); sepop
# 1c. For which of the two random samples is X more likely to be within 1 GPa of 70 GPa
# X is more likely to be within 1 GPa of the mean in part (b) due to the decreased variability of X with a larger sample size.
# 2. mean and sd are 70 GPa and 1.6 GPa with normal distribution
# 2a. Calculate P(69 ≤ X ≤ 71) when n = 9.
sepop <- 1.6 / sqrt(9); sepop
pnorm(71, 70, sepop) - pnorm(69, 70, sepop) # value, mean, sd
# 2b. How likely is it that the sample mean diameter exceeds 71 when n = 16?
sepop <- 1.6 / sqrt(16); sepop
1 - pnorm(71, 70, sepop)
# 3. mean of 50 and a sd of 1.4
# 3a. If normal, probability the sample mean for a random sample of 11 is at least 51?
sepop <- 1.4 / sqrt(11); sepop
1 - pnorm(51, 50, sepop)
# 3b. (approximate) probability the sample mean for a random sample of 37 is at least 51?
sepop <- 1.4 / sqrt(37); sepop
1 - pnorm(51, 50, sepop) # the answer was 0?
# 4. normally distributed with mean 2.62 and sd 0.82
# 4a. If a random sample of 25 specimens is selected, probability that the sample average density is at most 3.00? 
#     Between 2.62 and 3.00?
sepop <- 0.82 / sqrt(25); sepop
pnorm(3.00, 2.62, sepop)
pnorm(3.00, 2.62, sepop) - pnorm(2.62, 2.62, sepop)
# 4b. How large a sample size would be required to ensure that the first probability in part (a) is at least 0.99?
sepop <- 0.82 / sqrt(26); sepop # playing with n here 
pnorm(3.00, 2.62, sepop)
# 5. mean of 9,900 psi and a sd of 502 psi
# 5a. probability that the sample mean for a random sample of 40 rivets is between 9,800 and 10,100?
sepop <- 502 / sqrt(40); sepop
pnorm(10100, 9900, sepop) - pnorm(9800, 9900, sepop)
# 5b. If the sample size had been 15, could the probability requested in part (a) be calculated?
# No, n should be greater than 30 in order to apply the Central Limit Theorem.  
# 6. 𝜇 = true average (i.e., population mean)
#    CI: (117.6, 118.4)      (117.4, 118.6) 
# 6a. What is the value of the sample mean
xbar1 <- (117.6+118.4) / 2; xbar1
xbar2 <- (117.4+118.6) / 2; xbar2
# 6b. Both intervals were calculated from the same sample data. The confidence level for one of these intervals is 90% 
#     and for the other is 99%. Which of the intervals has the 90% confidence level, and why? 
# The first interval has the 90% confidence level because it is a narrower interval. 
# The first interval is narrower (width = 0.8), and the second interval is wider (width = 1.2).
# Wider intervals correspond to higher confidence levels
# 7. random sample of 50 bottles, 𝜇 denote the average for the population, 95% CI is (7.9, 9.3)
# 7a. Would a 90% confidence interval calculated from this same sample have been narrower or wider than the given interval?
#     The 90% would be narrower since the z critical value for 90% is smaller than the z critical value for 95%.
# 7b. There is a 95% chance that 𝜇 is between 7.9 and 9.3. Is this statement correct?
#     It is not a correct statement. We are 95% confident in the general procedure for creating the interval, but the mean may or may not be enclosed in this interval.
# 7c. We can be highly confident that 95% of all bottles of this type of cough syrup have an alcohol content that is between 7.9 and 9.3. Is this statement correct?
#     It is not a correct statement. The interval is an estimate for the population mean, not a boundary for population values.
# 7d. If the process of selecting a sample of size 50 and then computing the corresponding 95% interval is repeated 100 times, 95 of the resulting intervals will include 𝜇. Is this statement correct?
#     It is not a correct statement. We expect 95 out of 100 intervals will contain the mean, but we don't know this to be true. 
# 8. 𝜇 (watts) current is held at 10 amps for a speed of 1500 rpm. 
#    Assume that stray-load loss is normally distributed with 𝜎 = 3.1
# 8a. Compute a 95% CI for 𝜇 when n = 25 and xbar = 59.2.
zAlpha <- qnorm(1 - (1 - 0.95)/2); zAlpha # confidence level
sepop <- 3.1 / sqrt(25); sepop # sigma and n
marginError <- sepop * zAlpha
lower <- 59.2 - marginError; lower # xbar
upper <- 59.2 + marginError; upper # xbar
# 8d. Compute an 82% CI for 𝜇 when n = 100 and xbar = 59.2.
zAlpha <- qnorm(1 - (1 - 0.82)/2); zAlpha
sepop <- 3.1 / sqrt(100); sepop
marginError <- sepop * zAlpha
lower <- 59.2 - marginError; lower
upper <- 59.2 + marginError; upper
# 8e. How large must n be if the width of the 99% interval for 𝜇 is to be 1.0?
zAlpha <- qnorm(1 - (1 - 0.99)/2); zAlpha
sepop <- 3.1 / sqrt(260); sepop
marginError <- sepop * zAlpha
lower <- 59.2 - marginError; lower
upper <- 59.2 + marginError; upper
total <- upper - lower; total # looking for n about 260
# 9. The following observations are lifetimes (days) subsequent to diagnosis for individuals suffering from blood cancer (43 results)
# 9a. Can a confidence interval for true average lifetime be calculated without assuming anything about the nature of the lifetime distribution?
#     Yes, the sample size is large enough for the confidence interval to be reasonable.
# 9b. Calculate and interpret a confidence interval with a 99% confidence level for true average lifetime. 
#     [Hint:xbar = 1192.0 and s = 506.5.
zAlpha <- qnorm(1 - (1 - 0.99)/2); zAlpha
sepop <- 506.5 / sqrt(43); sepop
marginError <- sepop * zAlpha
lower <- 1192.0 - marginError; lower
upper <- 1192.0 + marginError; upper
# We are 99% confident that this interval contains the true population mean.
# 10. normally distributed with true sd 0.76. 
# 10c. How large a sample size is necessary if the width of the 95% interval is to be 0.3?
zAlpha <- qnorm(1 - (1 - 0.95)/2); zAlpha
sepop <- 0.76 / sqrt(100); sepop
marginError <- sepop * zAlpha
lower <- 4.56 - marginError; lower
upper <- 4.56 + marginError; upper
total <- upper - lower; total
# 10d. What sample size is necessary to estimate true average porosity to within 0.24 with 99% confidence?
zAlpha <- qnorm(1 - (1 - 0.99)/2); zAlpha
marginError <- 0.24
sd <- 0.76
n <- (zAlpha * sd / marginError)^2; n # solving for n
# 11. normally distributed with 𝜎 = 100
# 11a. if a sample of 25 modified bars resulted in a sample average yield point of 8446 lb, compute a 90% CI for the true average yield point
zAlpha <- qnorm(1 - (1 - 0.90)/2); zAlpha
sepop <- 100 / sqrt(25); sepop
marginError <- sepop * zAlpha
lower <- 8446 - marginError; lower
upper <- 8446 + marginError; upper
# 11b. How would you modify the interval in part (a) to obtain a confidence level of 94%?
#      The value of z would have to be changed
zAlpha <- qnorm(1 - (1 - 0.94)/2); zAlpha # from 0.90 to 0.94

# Homework 10 (7.1/7.3)
# 7.3/7.4: CIs Based on Normal Population
# when we don’t have σ but have a small sample size, use t with degrees of freedom (df) = n-1, instead of z
# T = ( xbar - μ ) / ( s * sqrt(n) ) with df = n-1
# z = tall peak, t25 = df = 25 medium peak, t5 = df = 5 low peak
# z: If your population is normal AND you know the population sd (σ) 
# If the population is not normal (or you do not know): 
#    z: If the sample size is at LEAST 40, use z
#    DO NOTHING: If the sample size is less than 40, cannot do anything 
# t: If your population is normal AND σ is not known (meaning sample s is known)
# a CI for μ using t: xbar +/- tofα/2anddf * [ s / sqrt(n) ] where ME = tofα/2anddf * [ s / sqrt(n) ]
# Upper CI: xbar + tα * [ s / sqrt(n) ] (notice it is not tsubalpha/2)
# Lower CI: xbar - tα * [ s / sqrt(n) ] (notice it is not tsubalpha/2)
# CI for proportion, p: 
#   Point Estimate: pwithhat = x / n
#   x is the number of successes
#   n is the sample size
# Critical Value: zα/2
# Standard Error: sqrt ([ pwithhat (1 - pwithhat) ] / n )
# Margin or Error: zα/2 * sqrt ([ pwithhat (1 - pwithhat) ] / n )
# CI for p: pwithhat +/- zα/2 * sqrt ([ pwithhat (1 - pwithhat) ] / n )
# Requirements: SRS AND ALSO n*pwithhat ≥ 10 AND ALSO n * (1-pwithhat) ≥ 10
# If there is no estimate for pwithhat, use pwithhat = 0.5, a “worst case” estimate

# 1. Determine the values of the following quantities.
# 1a. t0.10, 10 
qt(.90, 10)
# 1e. t0.005, 60
qt(1-0.005, 60)
# 2. Determine the t critical value(s) that will capture the desired t-curve area in each of the following cases. 
#    (Assume that central areas are centered at t = 0)
# 2a. Central area = 0.95, df = 15
qt((1-0.95)/2, 15)
# 2e. Upper-tail area = 0.01, df = 20
qt(1-0.01, 20)
# 2f. Lower-tail area = 0.025, df = 5
qt(0.025, 5)
# 3a. Construct a boxplot of the data
x <- c(417, 421, 422, 423, 426, 428, 430, 435, 436, 439, 445, 446, 449, 451, 455, 463, 464)
boxplot(x, horizontal=TRUE)$stats
# there are no outliers, little to no skew, and centered around 438
# 3b. Is it plausible that the given sample observations were selected from a normal distribution? Yes
# 3c. Calculate a two-sided 95% confidence interval for true average degree of polymerization
meanX <- mean(x); meanX 
sdX <- sd(x); sdX
meanX + c(-1, 1) * qt((1+0.95)/2, 17-1) * sdX / sqrt(17) # this gives the CI
# 3d. Does the interval suggest that 439 is a plausible value for true average degree of polymerization: Yes
# 3e. Does the interval suggest that 450 is a plausible value? No
# 4. 54% of 2345 American adults surveyed
# 4a. Calculate and interpret a CI at the 99% confidence level for the proportion
n <- 2345; n
x <- 0.54 * 2345; x
CI <- 99 / 100; CI
pHat <- x / n; pHat
zAlpha <-qnorm((1 + 0.99) / 2); zAlpha
pHat + zAlpha * sqrt(pHat * (1-pHat) / n)
pHat - zAlpha * sqrt(pHat * (1-pHat) / n)
# We are 99% confident that this interval contains the true population proportion. 
# 4b. What sample size would be required for the width of a 99% CI to be at most 0.06 irrespective of the value of p̂? 
n <- 1850
pHat <- 0.5 # irrespective of pHat
zAlpha <-qnorm((1 + 0.99) / 2); zAlpha
(pHat + zAlpha * sqrt(pHat * (1-pHat) / n)) - (pHat - zAlpha * sqrt(pHat * (1-pHat) / n))
# 6. Sample of 54 resulted in a sample average of 8.14 and a sample sd of 1.45.
# 6a. 95% CI
8.14 + c(-1, 1) * qt((1+0.95)/2, 54-1) * 1.45 / sqrt(54) 
# We make no assumptions about the distribution of percentage elongation.
# 8. 10 amps for a speed of 1500 rpm. normally distributed with 𝜎 = 2.3
# 8a. Compute a 95% CI for 𝜇 when n = 25 and x = 52.0.
zAlpha <- qnorm(1 - (1 - 0.95)/2); zAlpha # confidence level
sepop <- 2.3 / sqrt(25); sepop # sigma and n
marginError <- sepop * zAlpha
lower <- 52.0 - marginError; lower # xbar
upper <- 52.0 + marginError; upper # xbar
# 11. The following observations are lifetimes (days) subsequent to diagnosis for individuals suffering from blood cancer
x <- c(116, 181, 256, 419, 442, 462, 517, 739, 744, 789, 808, 866, 924, 983, 1025, 1063, 1063, 1165, 1191, 1222, 1222, 1252,
       1278, 1290, 1358, 1369, 1409, 1455, 1479, 1519, 1578, 1578, 1599, 1604, 1606, 1697, 1736, 1799, 1815, 1852, 1899, 1926, 1966)
length(x)
boxplot(x, horizontal=TRUE)$stats
# Yes, the sample size is large enough for the confidence interval to be reasonable.
# 11b. xbar = 1192.1 and s = 506.5
mean(x) + c(-1, 1) * qt((1+0.99)/2, length(x)-1) * sd(x) / sqrt(length(x))
# We are 99% confident that this interval contains the true population mean.     

# SOME RANDOM CLASS NOTES: 
# n = (2*2.58*(0.79/0.44))^2 
# 2.575829 is the z for 99% conf
# true standard deviation 0.79
# estimate true average porosity to within 0.22 (doubled to be 0.44 which is width)

# something about mean: when you interpret, you need context
# from a SRS of 64 students, the avg test score was 24 with a sd of 5. 
# find an 85% CI for the mean where the mean: the avg of the test scores
#      FOR THIS USE T because we do NOT have the pop sd
25 + c(-1, 1) * qt((1+0.85)/2, 64-1) * 5 / sqrt(64) # this gives the CI
# we are 85% confident that the true average test score is between 24.81784 and 25.18216

# one-sided, upper
25 + qt(0.85, 64-1) * 5 / sqrt(64)
# we are 85% confident that the true average test score is below 25.65315

# # one-sided, lower
25 - qt(0.85, 64-1) * 5 / sqrt(64)
# we are 85% confident that the true average test score is above 24.34685

# proportion CI
# 75 out of 200 students prefer beef over chicken
# find 97% CI for p
p <- 75 / 200; p
p + c(-1, 1) * qnorm((1 + 0.97) / 2) * sqrt(p * (1-p) / 200)
# we are 97% confident that the true proportion of students who prefer beef over chicken is between 0.300712 and 0.449288

# finding n when given ME (for mu)
# width = 2*me so something like width = 10 means me = 5
# me = zofalpha/2 * sigma / sqrt(n)
# n = (zofalpha/2 * sigma / me)^2, rounding up to the next int
# let me = 0.1
ceiling( (qnorm(1.85/2) * 5 / 0.91) ^2 ) # remember that qnorm gives z
# if given width, me = width / 2
# qnorm(0.90, mean, sd) = raw score
# z represents the number of sd away from the mean that the raw score (x) is
# if z = 1.3 then the raw score is 1.3sd ABOVE the mean 
# if z is negative then below the mean 

# ywithhat is the predicted value of y based on the model.
# r measures the strength and direction of the relationship between x and y. lines 191-193
# r2 measures how much of the variability in y is explained by the model.

# ~~~~~~~~~~~~~~~~~~~~TEST ~~~~~~~~~~~~~~~~~~~~ #
# 1a. 
f <- function(x) {
  2/25 * x
}
integrate(f, 0, 5)
# 1b. 
integrate(f, 3, 5)
# 1c. 
integrate(f, 3.75, 5)
# 1d. 
f <- function(x){
  (2/25)*x^2
  }
integrate(f, 0, 5) 
ex <- integrate(f, 0, 5)$value; ex


# 2a. 
qt((1-0.95)/2, 195)
# 2b. 
qt((1+0.95)/2, 196-1) * 3.31 / sqrt(196)
3.31 / sqrt(196)
# 2c. 
# from a SRS of 64 students, the avg test score was 24 with a sd of 5. 
# find an 85% CI for the mean where the mean: the avg of the test scores
#      FOR THIS USE T because we do NOT have the pop sd
51.64 + c(-1, 1) * qt((1+0.95)/2, 196-1) * 3.31 / sqrt(196) # this gives the CI
# we are 85% confident that the true average test score is between 24.81784 and 25.18216

# 3a. 
# CI for proportion, p: 
#   Point Estimate: pwithhat = x / n
#   x is the number of successes
#   n is the sample size
# Critical Value: zα/2
# Standard Error: sqrt ([ pwithhat (1 - pwithhat) ] / n )
# Margin or Error: zα/2 * sqrt ([ pwithhat (1 - pwithhat) ] / n )
p <- 278 / 356; p
qnorm((1+0.90)/2) * sqrt(p * (1-p) / 356)
# 3b. 
p + qnorm(0.90) * sqrt(p * (1-p) / 356)
# 3c. 
# n = (zofalpha/2 * sigma / me)^2, rounding up to the next int
# let me = 0.1
ceiling( (qnorm(1.90/2) * 5 / 0.03) ^2 ) # remember that qnorm gives z


# 4a. 
pnorm(24+1.1*5, 24, 5) - pnorm(24-1.1*5, 24, 5) 
# 4b. 
between1 <- pnorm(24+1*5, 24, 5) - pnorm(24-1*5, 24, 5)
between2 <- pnorm(24+2*5, 24, 5) - pnorm(24-2*5, 24, 5)
between2 - between1
# 4c. 
sepop <- 5 / sqrt(16); sepop
pnorm(23.24, 24, sepop) - pnorm(22.5, 24, sepop) # value, mean, sd
# 4d. 
pnorm(1.2, 24, sepop)
