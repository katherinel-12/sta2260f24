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
sdx <- sqrt(varx); sdx # just sd(x) for sample

# 7. Janista said that if we subtracted some constant from every number in our data set, then the mean would decrease by that number divided the total number of observations. Is this correct? 
# No, the mean would decrease by the number we subtracted from the entire data set. 
# 9. Dimitri was wondering what would happen to the variance of our data set if we added some constant to every number in the data set. What would happen in this case? 
# Nothing, the variance would remain the same.   

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

# Homework 1 (1.1/1.2/1.3/1.4) ----
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

# Homework 2 (1.4/2.1/2.2) ----
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
# INDEPENDENT VS DISJOINT
# 6. Let A be the event that Conner is cooking, B be the event that Amber is playing LoL, and C be the event that Cyrena is playing WoW. 
#    The probability of these events are Pr(A) = 0.6, Pr(B) = 0.2, and Pr(C) = 0.9. 
#    If the probability of A or B happening is 0.8, are events A and B independent? What about disjoint (mutually exclusive)? 
# Two events A and B are INDEPENDENT if: Pr⁡(A∩B)=Pr⁡(A)×Pr⁡(B)
# We are given: Pr(A)=0.6, Pr(B)=0.2, Pr⁡(A∪B)=0.8
# Using the inclusion-exclusion principle, we can express Pr(A∪B) as: Pr(A∪B)=Pr(A)+Pr(B)−Pr(A∩B)
# Substitute the known values: 0.8 = 0.6 + 0.2 − Pr(A∩B)
# Solving for Pr(A∩B): Pr(A∩B)=0.8−0.8=0
# Check if the events are independent: Pr(A∩B) = 0 and Pr(A)×Pr(B) = 0.6×0.2 = 0.12
# Since Pr(A∩B)=0 is not equal to Pr⁡(A)×Pr⁡(B)=0.12, events A and B are not independent.
# Two events are DISJOINT (or mutually exclusive) if: Pr⁡(A∩B)=0
# And we found Pr(A∩B)=0
# ANSWER: Disjoint, but not independent. 

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

# Homework 3 (2.3/2.4/3.1/3.2) ----
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
# RULE FOR CDF: P(a ≤ X ≤ b) = F(b) - F(a-) where a- is the next largest value below a 
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

# Homework 4 (3.1/3.2/3.3) ----
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

# Homework 5 (3.4/3.5) ----
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
dhyper(2, 7, 9, 6) # P(X = 2)
phyper(2, 7, 9, 6) # P(X ≤ 2)
1-phyper(1, 7, 9, 6) # P(X ≥ 2)
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

# FROM EXTRA 11. A box in a certain supply room contains 6 40-W lights, 4 60-W bulbs, and 3 75-W bulbs. Suppose that three bulbs are randomly selected. 
# 11a. What is the probability that exactly two of the selected bulbs are rated 75-W?
dhyper(2, 3, 10, 3) 
# 11b. What is the probability that all three of the selected bulbs have the same rating? 
totalCombs <- choose(13, 3)
(choose(6, 3) / totalCombs) + (choose(3, 3) / totalCombs) + (choose(4, 3) / totalCombs)
# 11c. What is the probability that one bulb of each type is selected? 
(choose(6, 1) * choose(3, 1) * choose(4, 1)) / totalCombs
# 11d. 
choose(10,5)/choose(13,5)

# For R: 
# D = exactly equal
# P = less than or equal

# TEST #1 ----
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

# Homework 6 (3.6/4.1) ----
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
# FROM EXTRA 12. An article considered the use of a uniform distribution with A = 0.20 and B = 4.25 for the diameter X of a certain type of weld
# 12a. Determine the pdf of X
# f(x) = { 1/(b-a) where a < x < b and 0 otherwise } since x is a uniform RV then E[x] = (1/2)(a+b) and Var[x] = (1/12)(b-a)^2 
# graph is a straight, horizontal line at 1/(b-a) from a to b and then 0 on all other parts 
# 12b. What is the probability that diameter exceeds 2 mm? 
f <- function(x) {1/4.05}
integrate(Vectorize(f), 2, 4.25)
# 12c. What is the probability that diameter is within 2 mm of the mean diameter? 
expected <- (1/2)*(4.25+0.20); expected
integrate(Vectorize(f), 0.225, 4.25)
# 12d. For any value a satisfying 0.20 < a < a + 3 < 4.25, what is P(a < X < a + 3)?
# P(a < X < a + 3) = ((a+3) - a) / (4.25 - 0.2) = 3 / (4.25 - 0.2) which is 0.7407407
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
1 - ppois(6, 4) # more than 6
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

# Homework 7 (4.2 and Ch 12) ----
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
m1 <- lm(y ~ x)
summary(m1)
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
# FROM EXTRA 14a. Does a scatter plot of the data support the use of the simple linear regression model? 
x <- c(5, 12, 14, 19, 23, 30, 40, 49, 55, 67, 72, 83, 96, 112, 127)
y <- c(4, 10, 13, 14, 15, 25, 27, 44, 38, 46, 53, 74, 82, 99, 105)
plot(x, y)
# Yes, the scatterplot shows a reasonable linear relationship. 
# 14b. Calculate point estimates of the slope and intercept of the population regression line. 
model <- lm(y ~ x)
summary(model)
# 14c. Calculate a point estimate of the true average runoff volume when rainfall volume is 50. 
-2.55599 + 0.85490*50 
# 14d. Calculate a point estimate of the standard deviation 𝜎. 
# Take the full Residual standard error: 4.811 
# 14e. What proportion of the observed variation in runoff volume can be attributed to the simple linear regression relationship between runoff and rainfall? 
# Adjusted R-squared:  0.9788 
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

# Homework 8 (4.3/4.4) ----
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
medianDist <- log(2) / 0.01437; medianDist # formula for median of exp dist log2 / 𝜆
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

# Homework 9 (5.3/5.4/7.1) ----
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
sepop <- 1.6 / sqrt(16); sepop # this is the sd, it is centered around the mean which is given
# 1b. a sample size of n = 256 sheets
sepop <- 1.6 / sqrt(256); sepop # sd goes down to 0.1 and mean stays at 70 
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

# Homework 10 (7.1/7.3) ----
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

# proportion Confidence Interval (CI)
# 75 out of 200 students prefer beef over chicken
# find 97% CI for p
p <- 75 / 200; p
p + c(-1, 1) * qnorm((1 + 0.97) / 2) * sqrt(p * (1-p) / 200)
# we are 97% confident that the true proportion of students who prefer beef over chicken is between 0.300712 and 0.449288

# 4b. If we used 𝛼= 0.01 instead of 0.05 then the CI would get more wide. 

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

# TEST #2 ----
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

# Homework 11 (8.1 - 8.3) ----
# Stat Hypothesis: a claim about the value of a parameter(s) or form of an entire prob dist
# Null: initial claim assumed to be true (statement of no change, always written with = )
# Alternative: claim that is contradictory to H0 (written with either <, >, or ≠ )

# Something about C = (1-𝛼) / 2 for ≠ or C = 1-𝛼 for < or >
# From the prof: you cut the tail area in half for twottailed test (≠) , you dont do that for one-tailed (< or >)
# c = confidence level where you can get c from alpha or alpha from c

#                         Truth
# Decision: H0 True               H0 False
#      RH0: Type 1 Error (alpha)  Correct Rejection
#    FTRH0: Correct Decision      Type 2 Error (Beta)
# H0 = Innocent and Ha = Guilty
# Type 1: saying they are guilty when they are innocent
# Rejection: not innocent when they are not innocent
# Decision: innocent and we do not have enough evidence to say they are not
# Type 2: not innocent and we fail to show that, so they walked free when they shouldn't have

# 8.2 Z-Tests for hypothese about a pop mean 𝜇: assume norm pop with 𝜎 known
# Null: H0: 𝜇 = 𝜇0
# Test stat: z = (xbar - 𝜇0) / (𝜎 / sqrt(n))
# Type of Test     Alt Hypothesis    p-val
# Right(Upper)     Ha: 𝜇 > 𝜇0      area to the right of z: pval = p(z >= z)
# Left(Lower)      Ha: 𝜇 < 𝜇0      area to the left of z: pval = p(z <= z) NOTE: Z-SCORE WILL BE NEG
# Two Tailed       Ha: 𝜇 ≠ 𝜇0      tail areas after z: pval = 2p(z <= z) NOTE: test stat could be +/-
# If p-val ≤ 𝛼 (sig level) then RH0 else FTRH0
# Something about still being able to do if n>40 because of CLT and its the same process

# 1. For each assertion, state whether it is a legitimate statistical hypothesis & why: 
# 1a. H: 𝜎 > 125: Yes, this statement is an assertion about the value of a parameter 
# 1b. H: x tilde = 40: No, x tilde is not a parameter 
# 1c. H: s ≤ 0.10: No, s is not a parameter 
# 1d. H: (𝜎1 / 𝜎2) < 1: Yes, this statement is an assertion about the value of parameters from two populations 
# 1e. H: Xbar − Ybar = 5: No, Xbar and Ybar are not parameters 
# 1e. H: 𝜆 ≤ 0.01, where 𝜆 is the parameter of an exponential distribution used to model component lifetime 
#       Yes, this statement is an assertion about the value of a parameter 

# 2. For the following pairs of assertions, indicate which do not comply with our rules for setting up hypotheses 
#    and why (the subscripts 1 and 2 differentiate between quantities for two different populations or samples): 
# 2a. H0: 𝜇 = 100, Ha: 𝜇 > 100: these hypotheses comply with our rules 
# 2b. H0: 𝜎 = 20, Ha: 𝜎 ≤ 20: Ha cannot include equality, so these hypotheses are not in compliance 
# 2c. H0: p ≠ 0.25, Ha: p = 0.25: Ha cannot include equality, so these hypotheses are not in compliance 
# 2d. H0: 𝜇1 − 𝜇2 = 25, Ha: 𝜇1 − 𝜇2 > 100: The asserted value in H0 should also appear in Ha, so these hypotheses are not in compliance 
# 2e. H0: S1^2 = S2^2, Ha: S1^2 ≠ S2^2: Each S is a statistic, so these hypotheses do not comply with our rules 
# 2f. H0: 𝜇 = 120, Ha: 𝜇 = 150: Ha cannot include equality, so these hypotheses are not in compliance 
# 2g. H0: 𝜎1/𝜎2 = 1, Ha: 𝜎1/𝜎2 ≠ 1: these hypotheses comply with our rules 
# 2h. H0: p1 − p2 = −0.1, Ha: p1 − p2 < −0.1: these hypotheses comply with our rules 

# 3. For which of the given P-values would the null hypothesis be rejected when performing a level 0.05 test? 
# 3b. 0.023: RH0
# 3c. 0.074: FTRH0

# 4. P-values and sig levels, 𝛼. For each pair, state whether the observed P-value would lead to rejection of H0
# 4a. P-value = 0.092, 𝛼 = 0.05: FTRH0
# 4d. P-value = 0.092, 𝛼 = 0.10: RH0 
# 4f. P-value = 0.256, 𝛼 = 0.10: FTRH0

# 5. A random sample is selected, specifications state mean strength should exceed 100 lb/in^2; the inspection decides to test 
#    H0: 𝜇 = 100 versus Ha: 𝜇 > 100. Explain why it might be preferable to use this Ha rather than 𝜇 < 100.
# We want to determine if there is significant evidence that the mean strength of welds exceeds 100 lb/in^2. 
# The current hypotheses correctly place the burden of proof on those who wish to assert that the specification is satisfied. 

# 6a. True sd is less than 0.05 mm. What hypotheses should be tested, and why? 
#    The appropriate hypotheses are H0: 𝜎= 0.05 mm versus Ha: 𝜎< 0.05 mm.
#    With this formulation, the burden of proof is on the data to show that the requirement has been met. 
# 6b. In this context, what are the type I and type II errors? 
#    In this context, the type I error occurs if we accept a shipment that should have been rejected. 
#    A type II error occurs if we reject a shipment that should have been accepted. 

# 7. Let 𝜇 denote the true average reaction time to a certain stimulus. For a z test of
#    H0: 𝜇 = 5 versus Ha: 𝜇 > 5, determine the P-value for each of the following values of the z test statistic.
# 7a. 1.45
1- pnorm(1.45)
# 7b. 0.99
1- pnorm(0.99)
# 7d. 2.47
1- pnorm(2.47)
# 7e. -0.10
1- pnorm(-0.10)

# 8. A pressure of 30 psi. Let 𝜇 denote the true average pressure. Determine the P-value for each of the following z test statistic values
# H0: 𝜇 = 30 versus Ha: 𝜇 ≠ 30 so 2 sided and p-val = 2*p(z >= |z|)
# 8a. 2.19
2*(1 - pnorm(2.19))
# 8c. -0.51
2*(1 - pnorm(0.51))
# 8d. 1.45
2*(1 - pnorm(1.45))
# 8e. −4.7 (0.000 is the answer)
2*(1 - pnorm(4.7))

# 9. sample size = 45, xbar = 52.6, 𝜎 = 4.7, true average at most 50 mils, 𝛼 = 0.05
# 9a. State the appropriate null and alternative hypotheses 
#     H0: 𝜇 = 50 versus Ha: 𝜇 > 50
# 9b. Calculate the test statistic and determine the P-value
z <- (52.6 - 50) / (4.7 / sqrt(45)); z
pval <- pnorm(-z); pval # 1-qnorm(z) can also be used because the Ha: 𝜇 > 50 so the greater than sign tells us which side
# 9c. State the conclusion in the problem context. 
# Reject the null hypothesis. There is sufficient evidence to conclude that the true average penetration is more than 50 mils.

# 10. 56 individuals in a sample, known 153 calories, sample mean estimated 193 and the sample sd 89. 
#     Does this data suggest that the true average estimated calorie content in the population sampled exceeds the actual content? sig level 0.001. 
# 10a. State the appropriate null and alternative hypotheses: H0: 𝜇 = 153 and Ha: 𝜇 > 153
# 10b. Calculate the test statistic and determine the P-value
z <- (193 - 153) / (89 / sqrt(56)); z
pval <- 1- pnorm(z); pval
# 10c. State the conclusion in the problem context 
# Reject the null hypothesis. There is sufficient evidence that the true average estimated calorie content of this beer exceeds the actual content.

# 11. n = 115, xbar = 11.4, 𝜎 = 6.64, 𝛼 = 0.05
# H0: 𝜇 = 15 and Ha: 𝜇 < 15
z <- (11.4 - 15) / (6.64 / sqrt(115)); z
pval <- 1-pnorm(-z); pval # (0.000 is the answer)
# Reject the null hypothesis. There is sufficient evidence that average daily zinc intake falls below 15 mg/day.

# 8.3 One sample T-Test: assume norm pop with 𝜎 UNKNOWN 
# Test stat: t = (xbar - 𝜇0) / (s / sqrt(n)) NOTE: df=n-1 and standard error = s / sqrt(n)
# Left tail, t should be neg, pval = pt(t, n-1)
# Right tail, t should be pos, pval = 1 - pt(t, n-1)
# 2 tailed, if using -t then pval = 2*pt(t, n-1) and if using +t then pval = 2*(1-pt(t, n-1))
# pval < 𝛼 then RH0 (with 2 tailed ex): There is enough evidence at the 𝛼 = 0.10 sig level to say the cost of a coffee is not $5. 
# if n >= 40 then use pnorm instead of pt
# Type 1: saying the avg coffee cost is not $5 when it actually is
# Type 2: FTRH0 and saying the avg cost is not $5 when it might actually be

# 12. true average pH is less than 7.0
# 12a. n = 7, t = −2.8, 𝛼 = 0.05 
pval <- pt(-2.8, 6); pval # pt( t , df)
# Reject the null hypothesis. There is sufficient evidence that the true average pH is less than 7.0.
# 12b. n = 11, t = −3.9, 𝛼 = 0.01 : Reject 
pval <- pt(-3.9, 10); pval
# 12c. n = 12, t = −1.1, 𝛼 = 0.05 
pval <- pt(-1.1, 11); pval
# Do not reject the null hypothesis. There is not sufficient evidence that the true average pH is less than 7.0.
# 12d. n = 8, t = 0.5, 𝛼 = 0.05 : Do not reject
pval <- pt(0.5, 7); pval
# 12e. n = 9, xbar = 6.78, s / sqrt(n) = 0.0820 
t <- (6.78 - 7.0) / 0.0820; t
pval <- pt(t, 8); pval
# We would reject the null hypothesis for any significance level at or above 0.014.

# 13. H0: 𝜇 = 20 versus Ha: 𝜇 > 20 
# 13a. n = 17, t = 3.3, 𝛼 = 0.05 : Reject
1 - pt(3.3, 16) # 1 - pt(3.3, 16) because of the Ha
# 13b. n = 9, t = 1.6, 𝛼 = 0.01 : Do not reject
1 - pt(1.6, 8)
# 13c. n = 30, t = −0.4
1 - pt(-0.4, 29)
# Do not reject the null hypothesis. There is not sufficient evidence to conclude that the new paint has a reflectometer reading higher than 20.

# 14. xbar = 2.481, 𝜎 = 1.611, estimated standard error = 0.294 and sig level 0.10
# 14a. H0: 𝜇 = 3 and Ha: 𝜇 ≠ 3
t <- (2.481 - 3) / 0.294; t
pval <- 2*(pt(t, 29)); pval # assume a sample size of 30 (something about CLT large enough sample)
# 14c. What can you conclude? Reject
# 14d. Would your conclusion be different if 𝛼 = 0.05 had been used? Do not reject

# From the prof: if you reject, there is enough evidence
# if you fail to reject, there is not enough evidence

# Ch. 8 Quiz ----
# 1. 55 individuals in a sample , 153 calories, sample mean 191, standard deviation 86, sampled exceeds the actual content? sig level 0.001
z <- (191 - 153) / (86 / sqrt(55)); z
pval <- 1- pnorm(z); pval

# 2. sample of 10 specimens with 2% fiber content, sample mean tensile strength (MPa) was 51.6, sample standard deviation was 1.1
t <- (51.6 - 48) / (1.1 / sqrt(10)); t
pval <- pt(t, 9); pval

z <- (51.6 - 48) / (1.1 / sqrt(10)); z
pval <- 1- pnorm(z); pval

x <- c(0.51, 0.61, 0.42, 0.5, 0.36)
xbar <- mean(x); xbar
varx <- sum((x-xbar)^2)/(length(x)-1); varx # or just var(x) for sample
sdx <- sqrt(varx); sdx # just sd(x) for sample
t <- (xbar - 0.6) / (sdx / sqrt(5)); t

# 3. if p < alpha, reject ( alpha - p = positive then reject )
0.05 # alpha, do not 
0.096

0.10 # alpha, REJECT 
0.096

# FINAL HERE ----
# 1b. 
f <- function(x) {
(5/8) * x^4 * (1/4)
}
integrate(f, 0, 1.25)

# 1d. 
f <- function(x){
  (5/8) * x^5 * (1/4)
}
integrate(f, 0, 2) 
ex <- integrate(f, 0, 2)$value; ex

# 2b. 
x <- c(0.53, 0.63, 0.42, 0.5, 0.35)
xbar <- mean(x); xbar
varx <- sum((x-xbar)^2)/(length(x)-1); varx # or just var(x) for sample
sdx <- sqrt(varx); sdx # just sd(x) for sample
t <- (xbar - 0.6) / (sdx / sqrt(5)); t

# 2c. 
t <- (0.486 - 0.6) / (0.1069112 / sqrt(5)); t
pval <- pt(t, 4); pval

# 5b. 
expval<-(16*0.15)+(18*0.2)+(20*0.35)+(24*0.3); expval
expval2<-(16^2*0.15)+(18^2*0.2)+(20^2*0.35)+(24^2*0.3); expval2
varval <- expval2 - expval^2; varval

# 6e. dhyper(x, M, N-M, n)
dhyper(5, 14, 42-14, 10) 

# 3a. 
# 75 out of 200 students prefer beef over chicken
# find 97% CI for p
p <- 100 / 275; p
p + qnorm(0.95) * sqrt(p * (1-p) / 275)
qnorm(0.95) * sqrt(p * (1-p) / 275)

qnorm(0.95) * sqrt(p * (1-p) / 77)


x <- c(5, 12, 14, 18, 23, 30, 40, 45, 55, 67, 72, 82, 96, 112, 127)
y <- c(4, 10, 13, 14, 15, 25, 27, 45, 38, 46, 53, 74, 82, 99, 104)
model1 <- lm(y ~ x)
summary(model1)

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
