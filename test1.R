rep(0,2) # repeats the number 0 two times. 
rep(2,1) # repeats the number 2 one time. 
stair_data <- c(rep(0,2),
                rep(1,3),
                rep(2,6),
                rep(3,8),
                rep(4,5),
                rep(5,8),
                rep(6,16),
                rep(7,18),
                rep(8,9),
                rep(9,12),
                rep(10,13))
stair_data[c(1:100)] # this prints the numbers at those indexes

table(stair_data) # shows the frequency distribution of the data
prop.table(stair_dist) # relative-frequency distribution

barplot(stair_dist) # Or barplot(prop.table(stair_dist))

x <- c(5, 5, 5, 6, 7, 8)
median(x) # aka xsquiggly

xbar <- mean(x); xbar
varx <- sum((x-xbar)^2)/(length(x)-1); varx # or just var(x) for sample
sdx <- sqrt(s2); sdx # just sd(x) for sample

pop_var <- function(x){
  xbar <- mean(x)
  return(sum((x-xbar)^2)/(length(x)))
}

pop.var <- pop_var(x); pop.var
pop.sd <- sqrt(pop_var(x)); pop.sd

bp_data <- c(171,178,201,203,205,211,218,223,234,249,256,359,408)
median(bp_data)
summary(bp_data)
boxplot(bp_data, horizontal=T)
bp <- boxplot(bp_data, horizontal=T)
bp$stats # true FNS! use this on hw
bp$out # outliers!

fu <- bp$stats[4]
fl <- bp$stats[2]
fs <- fu-fl; fs
mild_outliers_range <- c(fl - 1.5*fs, fu + 1.5*fs); mild_outliers_range
ext_outliers_range <- c(fl - 3*fs, fu + 3*fs); ext_outliers_range

# getting actual outliers
bp_data[which(bp_data >= ext_outliers_range[2] | bp_data <= ext_outliers_range[1])]
bp_data[which(bp_data >= mild_outliers_range[2] | bp_data <= mild_outliers_range[1])]


# Homework 1 (1.1/1.2/1.3/1.4)
# 1.1 Population proportion: p = x / N
# 1.2 Variables: 
#   Numeric/ Quantitative Variables (first parent): only numbers as their possible values (like someone’s height)
#      Discrete Variables (first child): has values that can be counted or listed
#      Continuous Variables (second child): has values that can be any number in some interva;: like 1, 2, (3, 4, 5), 6, 7 … where it can be like 4.25
#   Categorical/ Qualitative Variables (second parent): can be placed in categories, specifically non-numeric categories 
#      Nominal Variables (first child): no natural order to them, names or labels like colors
#      Ordinal Variables (second child): have a natural order like months of the year
# Frequency: how many times it occurs
# Relative Frequency (r.f): proportion of times it occurs in a set r.f = f / n
# Frequency Distribution: put it in a table with headers item, f, and rf
# 1.3 Population Mean: 𝜇(mu) = (x1 + x2 + … + xn) / N aka 𝜇 = (1/N) Σx
# Sample Mean: x̄ = (1/ n) Σx
# Outliers impact the mean more than the median
# Graph looks like / then skewed left and xbar < xsquiggly
# \ then skewed left and xsquiggly < xbar
# n then normal, symmetric, unimodal, and xbar = xsquiggly
# - then uniform and xbar = xsquiggly
# bimodal, random
# Lower Fourth: 
#   if n is odd, take the median of the lower half (including the median)
#   if n is even, take the median of the lower half
# Median Fourth: xsquiggly
# Upper Fourth: same as lower
# Spread of Fourths: upper - lower (50% of the data)
# Mild Outliers: [fL - 1.5*fS , fU + 1.5*fS ]
# Extreme Outliers: [fL - 3*fS , fU + 3*fS ]
# 1.4 Range: the largest value - the smallest value 
# Population Variance: σ2 = [Σ (x - 𝜇)2] / N 
# Population Standard Deviation: √σ2
# Sample Variance: s2 = [Σ (x - bar x)2] / (n-1)
#   The top is the Sum of Squares
#   The bottom is the Degrees of Freedom (df)
# Sample Standard Deviation: s = √[(Σ (x - bar x)2) / (n-1)]

# 1. The summary data was read from a graph in an article
#     3.0 -< 3.5 : 5 then 3.5 -< 4.0 : 14 then 4.0 -< 4.5 : 26 then
#     4.5 -< 5.0 : 33 then 5.0 -< 5.5 : 22 then 5.5 -< 6.0 : 13 then 
#     6.0 -< 6.5 : 7 then 6.5 -< 7.0 : 1 then 
#     7.0 -< 7.5 : 3 then 7.5 -< 8.0 : 1
# 1a. What proportion of the observations are less than 5: 0.624
# 1b. What proportion of the observations are at least 6: 0.096
# 1c. Construct a histogram with relative frequency on the vertical axis: 
#     y = Percent(5, 10, 15, 20, 25), x = Particle Size(3, 4, 5, 6, 7, 8) 
#     and this is positively skewed
# 1d. Construct a histogram with density on the vertical axis: 
#     y = Density(0.1, 0.2, 0.3, 0.4, 0.5), x = Particle Size(3, 4, 5, 6, 7, 8) 
#     Comparing graph 1c and 1d: essentially identical other than label
# 2. No. Bidders: 2, 3, 4, 5, 6, 7, 8, 9, 10, 11
#    No. Contracts: 6, 20, 24, 17, 13, 8, 6, 7, 2, 1
# 2a. What proportion involve at most 5 bidders: 0.644
#     What proportion involve at least 5 bidders: 0.519
# 2b. What proportion involve 5-10 bidders inclusive: 0.510
# 2b. What proportion involve between 5-10 bidders : 0.327
# 2c. Construct a histogram: 
#     y = Frequency(0, 5, 10, 15, 20, 25, 30), 
#     x = # Bidders(2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
#     distribution of # of bidders positively skewed, ranging from low of 2 
#.    to high of 11 with typical value 4-5 bidders
# 3. Symmetric and unimodal graph is when it looks like an n
# 4. Pareto diagram is a variation of histogram where the categories are ordered
#    from largest frequency to smallest
# 5. Relative frequency distribution with large positive skew is |\ 
#    Middle value is where the biggest bar lays
# 6. Amount of snow cover on each continent to average continental temperature: 
#    October snow cover for Eurasia between 1970-1979: 
#    6.5, 12.0, 14.9, 10.0, 10.7, 7.9, 21.9, 12.5, 14.5, 9.2
#    Representative(typical) value: median because 
#    potential outlier = misleading median
# 7. Blood pressure values are reported to nearest 5, actual are: 
#    108.6, 117.4, 128.4, 120.0, 103.7, 112.0, 98.3, 121.5, 123.2
# 7a. Median of reported: 115
# 7b. Suppose the blood pressure is 117.7 rather than 117.4, 
#     new median of reported: 120
# 7c. What does this say about the sensitivity of the median to rounding or 
#     grouping: median can be highly sensitive to small change
# 8. 0.732, 0.840, 0.866, 0.902, 0.922, 0.932, 0.966, 1.008
#    1.036, 1.055, 1.080, 1.114, 1.139, 1.158, 1,239, 1.352
# 8a. Compute sample mean (x bar) and median (x squiggly): 
#     bar = 1.021 and squiggly = 1.022
# 8b. How much could the largest sample be decreased without affecting the value
#     of the median: 0.316
# 9. Sample of n = 10 cars, S = no damage, F = damage
#    F F S S S F F S F F 
# 9a. Sample proportion of success (x/n): 4/10
# 9b. xbar = 0.4
#     xbar compared to x/n: the proportion is exactly equal to the mean
# 9c. Include 15 more cars, how many S so x/n=0.72 for 25 cars: 14
# 10. Sample = 10, median = 202.3, lower = 195.0, upper = 216.5
#     Smallest 3: 125.3, 188.6, 194.1
#     Largest 3: 221.4, 230.4, 250.9
# 10a. Outliers = yes, extreme = yes
# 10b. Boxplot with outliers: *                   |--sq(median)uare--|   *
#      Positive skew, small variation in the data
# *       |--sq(median)uare--|               *
# right skew - positive
# *                     |--squa(median)re--| *
# left skew - negative 
# order matters, 8 PICK 5 = n! / (n-k)!
perm <- factorial(8) / factorial(8-5); perm
# order doesn't matter, use CHOOSE(n,k)
choose(8,5) # n! / k! (n-k)!
# 5 chairs, 8 people (6F and 2M)
# prob of less than 2M being sat
(choose(2,1)*choose(6,4) / choose(8,5)) + (choose(2,0)*choose(6,5) / choose(8,5))

# Homework 2 (1.4/2.1/2.2) 
# 1.4 Five Number Summary (boxplots): 
# First: lowest value (not an outlier) Second: fL
# Third: median Fourth: fU
# Fifth: largest value (not an outlier)
# 2.1 Sample Space: set of all possible outcomes
# Simple Event: an event with only one outcome
# Compound Event: an event with more than one outcome
# Relations: 
#   Complement of event A (A’ or Ac): A does not happen
#   Union A and B (A∪B - “A or B”): A or B or BOTH happen 
#   Intersection A and B (A∩B - “A and B”): both A and B happen
# 2.2 Probability of an Event A: 0 ≤ Pr(A) ≤ 1 
#   Pr(A) = 0 means A will not happen
#   Pr(A) = 1 means A is guaranteed to happen 
# Complement Rule: 1 - Pr(A)
# General Addition Rule: Pr(A∪B) = Pr(A) + Pr(B) - Pr(A∩B)
#   If A∩B = 0 then Pr(A∩B) = 0 
#   A and B are mutually exclusive or disjoint events and Pr(A∪B) = Pr(A) + Pr(B)

# 1. Sample: 116.8, 115.5, 114.9, 115.2, 115.7
# 1a. xbar: 115.62, deviations from mean: 1.18, -0.12, -0.72, -0.42, 0.08
# 1b. Deviations calculated to get sample variance: s^2 = 0.527
#     Deviations calculated to get sample deviation: s = 0.726
# 2. Sample n = 5, 1st four deviations: 0.2, 0.7, 1.1, 1.4
#    5th deviation: -3.4
#    Give sample of 5 deviations: 3.6, 4.1, 4.5, 4.8, 0.0
# 6. 3 components form a system, components 2 and 3 are connected in parallel and
#    the system will function if at least one of them work 
#    For the entire system to function, component 1 must function
# 6a. Which outcomes are contained in event A that exactly 2 components function: 
#     A = {SSF, SFS, FSS}
# 6b. Event B at least 2 components function: B = {SSF, SFS, FSS, SSS}
# 6c. Event C the system functions: C = {SSF, SFS, SSS}
# 6d. Outcomes in C' = {FFS, FFF, FSF, SFF, FSS}
# 6e. Outcomes in AUC = {SSF, SFS, FSS, SSS}
# 6f. Outcomes in ANC = {SSF, SFS}
# 6g. Outcomes in BUC = {SSF, SFS, FSS, SSS}
# 6h. Outcomes in BNC = {SSF, SFS, SSS}
# 7. 5 copies, copies 1 and 2 are first prints, and 3, 4, 5 are second print
#    Examine in random order, stopping when second print chosen
# 7a. Outcomes in S = {3, 4, 5, 13, 14, 15, 23, 24, 25, 123, 124, 125, 213, 214, 215}
# 7b. Exactly one book examines: A = {3, 4, 5}
# 7c. Book 5 selected: B = {5, 15, 25, 125, 215}
# 7d. Book 1 not examined: C = {3, 4, 5, 23, 24, 25}
# 8a. At least 1 plant completed by contract date: 
#     A U B U C -> all parts of circles in venn diagram colored
# 8b. All plants complete by contract date: 
#     A N B N C -> only part ABC overlapping colored (very center)
# 8c. Only A completed by contract date: 
#     A N B' N C' -> only part A colored with no overlapping 
# 8d. Exactly one plant completed by contract date: 
#     (A N B' N C') U (A' N B N C') U (A' N B' N C) ->
#     only part A, part B, part C colored with no overlapping parts
# 8e. Either A or both other completed: 
#     A U (B N C) -> part A and overlapping part AB, BC, AC colored
# 9a. (A U B)' = A' N B' -> True, they are the same
#     (A U B)' -> only outside of circles colored
#     A' N B' -> only outside of circles colored
# 9b. (A N B)' = A' U B' -> True, they are the same
#     (A N B)' -> the outside of circles, part A, part B colored, just not overlapping part AB
#     A' U B' -> same as above
# 10. Money-market: 21%, Short bond: 14%, Inter bond: 7%, long bond: 5%, 
#     high-risk stock: 19%, mod-risk stock: 25%, and balanced: 9%
#     A customer who owns just one fund randomly selected: 
# 10a. Probability selected individual owns balanced: 0.09
# 10b. Probability individual owns bond fund: 0.26
# 10c. Probability does now own stock fund: 0.56
# 11. A = Visa = 0.7, B = Mastercard = 0.3
# 11a. Could P(A N B) = 0.5: No, needs to be P(A N B) <= P(B) but 0.5 > 0.3
# 11b. Supposed P(A N B) = 0.2, probability at least one card: 0.8
# 11c. Probability neither: 0.2
# 11d. Event student has Visa but not Master: A N B' = 0.5
# 11e. Probability exactly one: 0.6
# b - P(A or B or Both) = P(A) + P(B) - P(A and B) where 
#     P(A and B) is given as 0.2
# c - P(Not A and Not B) = P(A or B) ^Not which becomes 1 - P(A or B) and 
#     P(A or B) equalled 0.8 in section b
# d - P(A and Not B) = P(A) - P(A and B) where P(A) was given as 0.7 and 
#     P(A and B) was given at 0.2
# e - P(only A) + P(only B) = P(A and Not B) + P(B and Not A) which becomes 
#     [P(A) - P(A and B)] + [P(B) - P(A and B)] where all 3 values are given
# 12. P(A) = 0.22, P(B) = 0.25, P(C) = 0.28, P(A N B) = 0.14, 
#     P(A N C) = 0.03, P(B N C) = 0.05, and P(A N B N C) = 0.01
# 12a. A U B = award either 1 or 2 or both = 0.33
#      A or B = A + B - (A and B) 
# 12b. A' N B' = award neither 1 nor 2 = 0.67
#      Not A and Not B = 1 - (A or B)
# 12c. A U B U C = award at least 1 of the 3 = 0.54
#      (A or B or C) = A + B + C + (A and B and C) - (A and B) - (A and C) - (B and C) 
# 12d. A' N B' N C' = award none of the 3 = 0.46
#      (Not A and Not B and Not C) = 1 - (A or B or C)
# 12e. A' N B' N C = award 3 but not 1 nor 2 = 1-0.54(from 12c) = 0.46
# 12f. (A' N B') U C = award neither 1 and 2, or award 3 = 1−P(A∪B)=1−0.33=0.67 
#       and then 0.67+0.28=0.95
# 14. Soda = C, D and P -> Permutation where order matters
# 14a. Simple events in ranking experiment: {CDP, CPD, DCP, DPC, PCD, PDC}
#     All have same probability of 1/6
# 14b. Probability C ranked 1st: 0.33
# 14c. Probability C ranked 1st and D ranked 3rd: 0.165
# 15. Homeowners: N, L, M, H
#     Auto(L): 0.04  0.07  0.05  0.02
#     Auto(M): 0.07  0.08  0.20  0.12
#     Auto(H): 0.02  0.03  0.15  0.15
# 15a. P(Auto(M) N Home(H)) = 0.12
# 15b. P(Auto(L)) = 0.18 and P(Home(L)) = 0.18
# 15c. Same category for both Auto and Home = 0.42
# 15e. At least 1 low deductible level = 0.117
#     P(L′ low)=1−P(L low)=0.96
#     P(M′ low)=1−P(M low)=0.93
#     P(H′ low)=1−P(H low)=0.98
#     =0.96×0.93×0.98≈0.883
#     =1−P(None low)=1−0.883≈0.117
# 16. Intersection1 = 0.45, Intersection2 = 0.55, At least 1 = 0.9
# 16a. Stop at both = 0.1 where P(A or B) = P(A) + P(B) - P(A and B) 
# 16b. Stop at 1 but not at 2 = 0.35 where P(A and Not B) = P(A) - P(A and B) 
# 16c. Stop at exactly 1 = 0.8 where P(only A) + P(only B) = P(A and Not B) + P(B and Not A) 
#      which becomes [P(A) - P(A and B)] + [P(B) - P(A and B)] 

# Homework 3 (2.3/2.4/3.1/3.2) 
# 2.3 Permutation: order matters, Combination: order does not matter
# 2.4 For any 2 events A and B, with P(B) > 0, 
# the probability of A happening given that B happens is: P(A|B) = P(A∩B) / P(B)
# 3.1 Random Variable (RV) is any rule that assigns a number to an outcome in Sample Space S
# Bernoulli RV has only 2 outcomes, S or F, mapped to 1 and 0 respectively
# 3.2 Probability Distribution: of X, says how the total probability of 1 is assigned to the possible outcomes
# Probability Mass Function (pmf): for every number x by P(X = x) = p(x) = P(all ω in S : X(ω) = x)
# Any pmf requires: 0 ≤ P(X = x) ≤ 1 AND Σp(x) = 1
# Cumulative Distribution Function (CDF = F(x)): F(x) = P(X ≤ x) = Σ from (y: y ≤ x) of p(y)
# ex. F(8) = P(X ≤ 8) = p(1) + p(2) + p(4) + p(8) = 0.90
# 1 -> p(0.05), 2 -> p(0.10), 4 -> p(0.35), 8 -> p(0.40)
# RULE FOR CDF: P(a ≤ X ≤ b) = F(b) - F(a-)
# ex. P(2 ≤ X ≤ 8) = P(X ≤ 8) - P(X ≤ 1)

# 2. P(A) = 0.10, P(B) = 0.07, P(C) = 0.06
#   P(A U B) = 0.11, P(A U C) = 0.13, P(B U C) = 0.11
#   P(A N B N C) = 0.01
# 2a. System has A, probability is has B: 0.6
#     P(A | B) = P(A and B) / P(A) = 0.06 / 0.1
# 2b. System has A, probability is has ABC: 0.1
#     P(A | A, B, and C) = P(A and B and C) / P(A) = 0.01 / 0.1
# 2c. System has at least 1 type, probability exactly 1: 0.3077
#     Have to solve the venn diagram starting from the middle and making sure to subtract from each piece. 
#     Ex. A1 total = 0.1 but A1 only = 0.02 (answer: (A1 only + A2 only + A3 only) / total of 0.15
# 2d. System has A and B, probability not C: 0.8333
#     1 - [(A1 and A2 and A3) / P(A1 and A2)] = 1 - (0.01/0.06)
# 4. P(A) = 0.22, P(B) = 0.25, P(C) = 0.28
#    P(A N B) = 0.05, P(A N C) = 0.08, P(B N C) = 0.11
#    P(A N B N C) = 0.02
# 4a. P(B|A) = 0.2273 = if awarded Proj1, the chance also awarded P2
#     P(B | A) = P(A and B) / P(B) 
# 4b. P(B N C | A) = 0.0910 = if awarded Proj1, chance also awarded P2 and P3
#     (A1 n A2 n A3) / A1 = 0.02 / 0.22
# 4c. P(B U C | A) = 0.5 = if awarded Proj1, chance also awarded 1 of other 2
#     (A1 n A2 +  A1 n A3 - A1 n A2 n A3) / A1 = (0.05 + 0.08 - 0.02) / 0.22
# 4d. P(A N B N C | A U B U C) = 0.0377 = if awarded at least 1, chance awarded all 3
#     A1 U A2 U A3 = A1 + A2 + A3 + All - A1 N A2 - A1 N A3 - A2 N A3 = 0.53
#     Then All / 0.53
# 5. If A and B are independent, show A' and B are also independent
#     P(B) = P(A' N B) + P(A N B)
#     P(A' N B) = P(B) - P(A N B)
#     Since A and B are independent then the following: 
#     P(A' N B) = P(B) - P(A)*P(B)
#     P(A' N B) = [1-P(A)] - P(B)
#     P(A' N B) = P(A')*P(B) showing A' and B are independent
# 6. Blood Types A = 0.45, B = 0.08, AB = 0.05, and O = 0.42
# 6a. 2 random are independent of another, probability both O: 0.1764
#     P(Both O) = P(O) * P(O)
# 6b. Match: 0.3878
#     P(Match) = P(A)* P(A) + P(B) * P(B) + P(AB) * P(AB) + P(O) * P(O)

# Homework 4 (3.1/3.2/3.3) 
# 3.3 Given a PMF table, E[X] = μ = 1(0.01) + 2(0.03) + … + 7(0.02) = 4.57
# Expected Value: E[X] = Σ (x * p(x))
# Variance of a Discrete Random Variable: Var[X] = E[x^2] - E[X]^2 
# Standard deviation (σx)= sqrt(var[X])
# Expected Value of a Linear Function: where a and b are constants
# E[aX + b] = a * E[X] + b * 1
# Variance of a Linear Function: Var[aX + b] = a2 * var[x]
# 1. Failure by Shear(s) or Flexure(F), 3 beams randomly selected, X = # of beams
#    among 3 selected that failed by S
# S: FFF, SFF, FSF, FFS, FSS, SFS, SSF, SSS
# X: 0    1    1    1    2    2    2    3
# 2. Give 3 ex of Bernoulli's RV
#    a. X = 1 if a randomly selected day has high temp over 100 and X = 0 otherwise
#    b. X = 1 if a randomly selected shopper purchases food item at department store and X = 0 otherwise
#    c. X = 1 if a randomly selected lightbulb needs to be replaced and X = 0 otherwise
#    ex of not: X = num of days in year where high temp exceeds 100 and X = 0 if none
# 3. X = num nonzero digits in randomly selected 4-digit pin with no restrictions on digits
# 3a. What are possible values of X: 0, 1, 2, 3, 4
# 3b. Pin: 1300 & associated X Value: 2
#     Pin: 2530 & X: 3
#     Pin: 7486 & X: 4
# 4. Car turns Left(L), Right(R), or Ahead(A). Experiment ends when car turns left. 
# 4a. Possible X values: 1, 2, 3, 4, ...
# 4b. Outcome: RL, X Value: 2
#     Outcome: RRL, X Value: 3
#     Outcome: RRAL, X Value: 4
#     Outcome: AAL, X Value: 3
# 5. PMF of X: p(0)=0.15, p(1)=0.10, p(2)=0.25, p(3)=0.30, p(4)=0.20
# 5a. Probability Histogram: y=p(x)=(0.0, 0.05, 0.1 ... 0.3) and x=x-(0, 1, 2, 3, 4)
#     with the rectangles
# 5b. Probability at least 2: 0.75, More than 2: 0.5
# 5c. Probability 1 to 3 inclusive: 0.65
# 6. Plane with 50 seat, 55 tickets and pmf: 
# y    45   46   47   48   49   50   51   52   53   54   55
# p(y) 0.05 0.10 0.11 0.14 0.26 0.17 0.06 0.05 0.03 0.02 0.01
# 6a. Probability accomodate all ticketed who show: 0.83
# 6b. Probability not all ticketed who show can be accomodated: 0.17
# 6c. 1st on standby, probability take the flight: 0.66
# 6d. Probability if 3rd on standby: 0.26
# 7. Submit 1-7 forms depending, Y = num forms required of next applicant and p(y) = ky
# 7a. Value of k if 7Σy=1 of p(y) = 1 : k = 1/28
# 7b. Probability at most 5 forms: 15/28
# 7c. Probability 2-6 forms inclusive: 20/28
# 7d. Could p(y) = y^2 / 135 for y = 1 ... 7 be pmf Y: no because 7Σy=1 of p(y) = 140/135
#     Any pmf requires: 0 ≤ P(X = x) ≤ 1 AND Σp(x) = 1 and 140/135 is about 1.037
# 8. Equations for E(x), V(x) and SD(x)
# 9. pmf of y: p(y) is 0:0.5, 1:0.2, 2:0.25, 3:0.05
# 9a. E(X)=(0*0.5)+(1⋅0.20)+(2⋅0.25)+(3⋅0.05) = 0.85
# 9b. Surcharge = 100y^2 and then (Surcharge 0 * 0.5) + (Surcharge 100 * 0.2) + (Surcharge 400 * 0.25) + (Surcharge 900 * 0.05) = 165
# 10. Determine the probability that Y is within 1 standard deviation of the mean value
# expval<-(45*0.02)+(46*0.1)+(47*0.12)+(48*0.14)+(49*0.25)+(50*0.17)+(51*0.05)+(52*0.05)+(53*0.01)+(54*0.03)+(55*0.06); expval
# expval2<-(45^2*0.02)+(46^2*0.1)+(47^2*0.12)+(48^2*0.14)+(49^2*0.25)+(50^2*0.17)+(51^2*0.05)+(52^2*0.05)+(53^2*0.01)+(54^2*0.03)+(55^2*0.06); expval2
# varval <- expval2 - expval^2; varval
# sdval <- sqrt(varval); sdval
# e[x] = 49.21 and sd = 2.384513 so find the values between (46.82549, 51.59451) -> (47 - 51)
# 0.12 + 0.14 + 0.25 + 0.17+ 0.05 = 0.73
# 11. X = rated capacity of freezer of brand sold at this store
#     PMF: x: p(x) : 16:0.1, 18:0.2, 20:0.7
# 11a. If the price having capacity X is 63X-650, expected price of next customer: 
#      E[X] = 19.2 so 63(19.2) - 650 = 559.6
# 11b. Variance of price paid by next customer: Variance of a Linear Function: 
#      a^2 * var[x] and a = 63 and 63^2 * 1.76 = 6985.44
# 11c. X = rated capacity, actual capacity = h(x) = X - 0.009 * X^2 and X = 19.2
# 12. 1:0.35, 2:0.35, 3:0.15, 4:0.15
# 12a. E[x] = 2.1 and E[x-5] = 2.9
# 12b. Better charging $65 or $[150 / (5-X)]: $65 > $64.375
#      E[150 / 5−X] = 150/5-1 + 150/5-2 + 150/5-3 + 150/5-4
#            = 375, 50, 72, and 150
#      E[150 / 5−X] = (37.5⋅0.35)+(50⋅0.35)+(75⋅0.15)+(150⋅0.15)

# Homework 5 (3.4/3.5) 
# 3.4 Binomial Probabilities: b(x; n, p)
#    Fixed number of trials n, Each trial has only 2 outcomes: success or failure
#    Each trial is independent of each other, The probability of success for each trial, p, is the same 
#    Probability of x successes in a binomial distribution: P(X = x) = (n choose x)p^x(1-p)^(n-x)
# Hypergeometric Distribution - Assumptions: 
#    Finite population size N, Finite number of successes in the population M
#    Two possible outcomes: success or failure, Take a sample size of n without replacement
#    P(X = x) = [(M choose x)(N-M choose n-x)] / (N choose n)
# 1a and 1b: b(2; 8, 0.25)
dbinom(2, 8, 0.25)
dbinom(6, 8, 0.65)
# 1c: P(3 ≤ X ≤ 5) when n = 7 and p = 0.65 
dbinom(3, 7, 0.65)+dbinom(4, 7, 0.65)+dbinom(5, 7, 0.65)
# 1d: P(1 ≤ X) when n = 9 and p = 0.15 
1 - dbinom(0, 9, 0.15)
# 2. An article reported that 6 in 10 auto accidents involve a single vehicle 
# (the article recommended always reporting to the insurance company an accident 
# involving multiple vehicles). Suppose 20 accidents are randomly selected.
# 2a. Probability that at most 8 involve a single vehicle? 
pbinom(8, 20, 0.6)
# 2b. Probability that exactly 8 involve a single vehicle? 
dbinom(8, 20, 0.6)
# 2c. Probability that exactly 10 involve multiple vehicles? 
dbinom(10, 20, 0.4)
# 2d. Probability that between 5 and 8, inclusive, involve a single vehicle? 
dbinom(5, 20, 0.6)+dbinom(6, 20, 0.6)+dbinom(7, 20, 0.6)+dbinom(8, 20, 0.6)
# 2e. Probability that at least 5 involve a single vehicle? 
1 - pbinom(5, 20, 0.6)
# 2f. Probability that exactly 8 involve a single vehicle and 
# the other 12 involve multiple vehicles? 
dbinom(8, 20, 0.6)+dbinom(12, 12, 0.4)
# 3. NBC News reported on May 2, 2013, that 1 in 20 have a food allergy. 
# Consider selecting a random sample of 25 children then X ~ Bin(25, 0.05)
# 3a. Determine both P(X ≤ 4) and P(X < 4). 
pbinom(4, 25, 0.05)
pbinom(3, 25, 0.05)
# 3b. Determine P(X ≥ 5). 
1 - pbinom(4, 25, 0.05)
# 3c. Determine P(1 ≤ X ≤ 4). 
dbinom(4, 25, 0.05)+dbinom(3, 25, 0.05)+dbinom(2, 25, 0.05)+dbinom(1, 25, 0.05)
# 3d. What are E(X) and 𝜎X? 
#     E[x] = n*p where 25*0.05
#     25*0.05*(1-0.05) where n*p(1-p) is var and sqrt(var) is sd
# 3e. In a sample of 80, probability that none has a food allergy? 
dbinom(0, 80, 0.05)
# 4a. Probability that at most 7 of the calls involve a fax message?
pbinom(7, 25, 0.25)
# 4b. Probability that exactly 7 of the calls involve a fax message?
dbinom(7, 25, 0.25)
# 4c. Probability that at least 7 of the calls involve a fax message?
1 - pbinom(6, 25, 0.25)
# 4d. Probability that more than 7 of the calls involve a fax message?
1 - pbinom(7, 25, 0.25)
# 5. A particular telephone number is used to receive voice calls and faxes.
# Suppose 40% incoming calls involve fax, and a sample of 25 incoming calls. 
# 5a. Expected number of calls among the 25 that involve a fax? 
x <- 25
px <- 0.4
ex <- x*px; ex
# 5b. Standard deviation of the number among the 25 calls that involve a fax?
varx <- ex*(1-px); varx
sdx <- sqrt(varx); sdx
# 5c. Probability that the number of calls among the 25 that involve a fax
# exceeds the expected number by more than 2 sd? E[X] = 10, sd = 2.5, num = 15
1 - pbinom(14, 25, 0.40)
# 6. The pmf of Y is given as: 
# y 	0 	1 	2 	3
# p(y)0.60 	0.20 	0.15 	0.05
# 6a. Probability that among 25 randomly chosen, at least 15 have no citations?
pbinom(15, 25, 0.60)
# 6b. Probability that among 25 randomly chosen, 
#     fewer than half have at least one citation? 1-P(0) = 1-0.6 = 0.4
pbinom(12, 25, 0.40)
# 6c. Probability that among 25 randomly chosen, at least one citation is between 10 and 15, inclusive?
dbinom(10, 25, 0.4)+dbinom(11, 25, 0.4)+dbinom(12, 25, 0.4)+dbinom(13, 25, 0.4)+dbinom(14, 25, 0.4)+dbinom(15, 25, 0.4)
# 7. (A)credit card, (B)debit card, or (C)cash where P(A) = 0.6, P(B) = 0.3, and P(C) = 0.1
# 7a. Among the next 100 customers, what are the mean and variance of the number who pay with a debit card? 
#     Mean = 100*0.3 and Var = n⋅px⋅(1−px)
# Because we are interested in whether or not a debit card was used, we can use the binomial distribution. 
# X = the number of customers who use a debit card.

# 8. 16 are scheduled to take a driving test at a particular DMV on a certain day, 
#    7 of whom will be taking the test for the 1st time. 
#    Suppose 6 are randomly assigned to a particular examiner, 
#    and let X be the number among the 6 who are taking the test for the 1st time. 
# 8a. What kind of a distribution does X have: h(x; 6, 7, 16)
# 8b. Compute P(X = 2), P(X ≤ 2), and P(X ≥ 2).
# dhyper(x, M, N-M, n)
dhyper(2, 7, 9, 6)
phyper(2, 7, 9, 6)
1-phyper(1, 7, 9, 6)
# 8c. Calculate the mean value and standard deviation of X 
exHyp <- 6 * (7/16); exHyp # mean
varHyp <- ((16-6) / (16-1)) * 6 * (7/16) * (1 - (7/16)); varHyp
sdHyp <- sqrt(varHyp); sdHyp
# 9. 2 sections of class, the first with 25 and second with 40. 
#    All projects turned in, instructor randomly ordered them before grading. 
#    Consider the first 15 graded projects. 
# 9a. Probability that exactly 10 from the second section?
dhyper(10, 40, 25, 15)
# 9b. Probability that at least 10 of these are from the second section?
1 -phyper(9, 40, 25, 15)
# 9c. Probability that at least 10 of these are from the same section?
sum(dhyper(10:15, 40, 25, 15)) + sum(dhyper(10:15, 25, 40, 15))
# 9d. Mean and sd of the number among these 15 that are from the second section? 
exHyp <- 15 * (40/65); exHyp # mean
varHyp <- ((65-15) / (65-1)) * 15 * (40/65) * (1 - (40/65)); varHyp
sdHyp <- sqrt(varHyp); sdHyp
# 9e. Mean and sd of projects not among first 15 that are from the second section?
exHyp <- 50 * (40/65); exHyp # mean
varHyp <- ((65-50) / (65-1)) * 50 * (40/65) * (1 - (40/65)); varHyp
sdHyp <- sqrt(varHyp); sdHyp

# E[X] = n * (M/N)
# Var[X] = ((N-n) / (N-1)) * n * (M/N) * (1 - (M/N))

# For R: 
# D = exactly equal
# P = less than or equal

#~~~~~~~~~~~~~~~~~~~~~~~~~~~TEST HERE~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 3b. 
1 - pbinom(14, 25, 0.6)

# 1b. 
1 -phyper(2, 12, 12, 5)

# 1c. 
phyper(2, 8, 12, 5)

# 1d. # E[X] = n * (M/N)
# Var[X] = ((N-n) / (N-1)) * n * (M/N) * (1 - (M/N))
exHyp <- 5 * (12/20); exHyp # mean
varHyp <- ((20-5) / (20-1)) * 5 * (12/20) * (1 - (12/20)); varHyp
sdHyp <- sqrt(varHyp); sdHyp



