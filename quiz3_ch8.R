# Homework 11 (8.1 - 8.3)
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

# Chapter 8 Quiz
# 1. 55 individuals in a sample , 153 calories, sample mean 191, standard deviation 86, sampled exceeds the actual content? sig level 0.001
z <- (191 - 153) / (86 / sqrt(55)); z
pval <- 1- pnorm(z); pval

# 2. sample of 10 specimens with 2% fiber content, sample mean tensile strength (MPa) was 51.6, sample standard deviation was 1.1
t <- (51.6 - 48) / (1.1 / sqrt(10)); t
pval <- pt(t, 9); pval

z <- (51.6 - 48) / (1.1 / sqrt(10)); z
pval <- 1- pnorm(z); pval

# 3. if p < alpha, reject ( alpha - p = positive then reject )
0.05 # alpha, do not 
0.096

0.10 # alpha, REJECT 
0.096

