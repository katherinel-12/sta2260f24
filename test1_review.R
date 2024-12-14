# 1. Deck of 20 cards, 12 are trap and 8 are not. Draw a hand of 5 without replacement
#1a. Distribution, parameters? Hypergeometric, 
N <- 20
M <- 12 # (or 8) 
n <- 5
# 1b. X = at least 3 trap? Pr(X >= 3) = 1 - Pr(X <= 2)
1-phyper(2, M, N-M, n) # preferable
sum(dhyper(3:5, M, N-M, n)) # alternative way
#1c. At most 2 non-trap? This questions was just showing how to phrase "success"
phyper(2, 8, 12, 5)
#1d. E[x], Var[x], SD[x]
ex <- n * M/N; ex
varx <- ( (N-n) / (N-1) ) * n * M/N * ( 1 - (M/N) ); varx
sdx <- sqrt(varx); sdx
  
# 2. Pop of 340 employees, ages range from 20-49 years, rent or own
# 2a.      Twenties Thirties Fourties Total 
#     Own   7        29       74       110
#     Rent  128      80       22       230
#           135      109      96       340
# 2b. 29 / 340
# 2c. 230 + 135 - 128 = 237 / 340
# 2d. 103 / 205
# 2e. Twenties and Renting independent? Pr(Twenties | Rent) ?= Pr(Tw)
#     128 / 230 ?= 135 / 340 Not equal, so not independent
# 2f. 2 events which are disjoint: 
#     Owning and Renting, you're not going to do both

# 3. n = 25, p(0) = 0.6, p(1) = 0.25, p(2) = 0.1
# 3a. p(3) = 0.05
# 3b. Probability at least 15 people have no toppings
#     p = 0.6, P(X >= 15) = 1-Pr(X <= 14)
1-pbinom(14, 25, 0.6)
sum(dbinom(15:25, 25, 0.6))
# 3c. Probability fewer than half have at least 1: Pr(X <= 12)
pbinom(12, 25, 0.4)
sum(dbinom(0:12, 25, 0.4))
# 3d. E[x], var[x]
ex3 <- 25*0.4; ex3
var3 <- 25*0.4*0.6; var3
sd3 <- sqrt(var3); sd3
# 3e. Take the mean and +/- 2 * sd
10+c(-1, 1)*2*sqrt(6) # Then do Pr(6 <= X <= 14)
# Pr(X <= 14) - Pr(X <= 5)
pbinom(14, 25, 0.4) - pbinom(5, 25, 0.4)
sum(dbinom(6:14, 25, 0.4))

# 4. n = 20
# 4a. Outliers if any: 125.7 and 251.7
fl <- 198.0
med <- 201.4
fu <- 216.2
fs <- fu - fl; fs
lowMildOut <- fl - 1.5*fs; lowMildOut
highMildOut <- fu + 1.5*fs; highMildOut
lowExOut <- fl - 3*fs; lowExOut
highExOut <- fu + 3*fs; highExOut
# 4b. *(125.7)  |(188.1)---s(198)q(med)uar(216.2)e-----------(230.8)|    *(251.7)
# 4c. Right skew because median on the left of box and longer tail on right 
# 4d. What number do 75% exceed: fl
# 4e. Find the largest value we can change the smallest value to without affecting the median
#     Claim the largest value we can change the smallest one to would be the median
#     Make fake dataset: 1, 3, 5, 7, 9, 11 -> median 6
#     changing 1 to median: 3, 5, 6, 7, 9, 11 -> median 6.5 so incorrect

# something about needing odd number of values?

# 5a. Ex of nomial variable is t-shirt size? False, this is ordinal
# 5b. Pr(A) = 0.6, Pr(0.2), Pr(C) = 0.9, if Pr(B or C) = 0.9, are these events independent, disjoint, neither, or both
#        Not disjoint: P(B or C) = 0.9
#                      P(B) + P(C) - P(B and C) = 0.2 + 0.9 - 0.2
#                      P(B and C) = 0.2
#        Not independent: P(B) * P(C) ?= 0.2
#                        0.2 * 0.9 =/= 0.2
# 5c. Mean changes, SD does not change as it is just the "spread" of the mean 
# 5d. If 2 events are independent, they must be disjoint?
#     False! They are one or the other, and cannot be both 
# 5e. E[y] = 0.95 * E[x] + 0.23, where E[x] = 0.89
#          = 1.0755
#     Var[y] = 0.95^2 * Var[x] where SD was given 
#            = 0.8123












