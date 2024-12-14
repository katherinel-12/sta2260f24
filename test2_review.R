# Test 2

# This gives an outline option
# Some Comment ----
## Another Comment ----

# QUESTION 1. 
f <- function(x){
  2/25*x
}
integrate(f, 3, 5)$value
sqrt(25 * 0.75)

integrate(f, 0, 4.33)$value

exVal <- function(x){
  2/25*x*x
}
integrate(exVal, 0, 5)$value

# QUESTION 2. 
xbar <- 51.64; s <- 3.31; n <- 196

t <- qt(1.95/2, n-1); t

se <- s / sqrt(n); se
me <- t * se; me

xbar + c(-1, 1) * me

# Try lowering c for your crit value: 
t <- qt(1.90/2, n-1); t
me <- t * se; me
xbar + c(-1, 1) * me

# QUESTION 3. 
phat <- 278 / 356; phat
me <- qnorm(0.9) * sqrt(phat * (1-phat)/ 356); me
phat + me

ceiling(phat * (1-phat) * (qnorm(0.9) / 0.03) ^2)
# another way: get the number and round up phat * (1-phat) / (0.03 / qnorm(0.9))^2

# QUESTION 4. 
pnorm(1.1) - pnorm(-1.1)

2 * (pnorm(2) - pnorm(1))

pnorm(23.25, 24, 5/sqrt(16)) - pnorm(22.5, 24, 5/sqrt(16))

qnorm(0.05, 24, 5/sqrt(16))

