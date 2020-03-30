---
title: "methods"
output: html_document
---

Patient details were submitted electronically by participating sites to the ISARIC database.  Relevant background and presenting symptoms were recorded on the day of study recruitment.  Daily follow-up was then completed until recovery or death.  A final form was completed with details of treatments received and outcomes. All categories that represent fewer than five individuals have been suppressed to avoid the potential for identification of participants. 

Graphs have been used to represent the age distribution of patience by sex and status (dead, recovered & still in hospital), the prevalence of individual symptoms  - and combinations of them - on admission, the prevalence of individual comorbidities  - and combinations of them - on admission, the length of hospital stay by sex and age group and the distribution of patient statuses by time since admission. In addition, the number of cases recruited by country and site, as well as the case count by status, has been represented.

Using a non-parametric Kaplan-Meier-based method (Ghani *et al.*, 2005), the case- fatality ratio (CFR) was estimated, as well as probabilities for death and recovery. This method estimates the CFR with the formula $a/(a+b)$, where $a$ and $b$ are the values of the cumulative incidence function for deaths and recoveries respectively, estimated at the last observed time point. In a competing risk context (i.e. where there are multiple endpoints),  the cumulative incidence function for an endpoint is equal to the product of the hazard function for that endpoint and the survival function assuming a composite endpoint. It is worth noting that this method assumes that future deaths and recoveries will occur with the same relative probabilities as have been observed so far. Binomial confidence intervals for the CFR were obtained by a normal approximation (See Ghani *et al.*, (2005)).

A survival analysis was performed to test whether significant differences exist in the length of hospital stay by sex.

<!-- Another CFR estimation method, which uses observed outcomes only, was employed in this analysis. For this method, the CFR is calculated as the ratio of deaths to the sum of deaths and recoveries until the reference point (Wu *et al.*, 2020). Over time, the uncertainty around the estimate decreases due to an increase in completed outcomes. Exact binomial confidence intervals are plotted along with the risk estimate at each time point.  -->

To obtain estimates for the distributions of time from symptom onset to hospital admission and the time from admission to outcome (death or recovery), Gamma distributions were fitted to the observed data, accounting for unobserved outcomes. Parameters were estimated by a maximum likelihood procedure and confidence intervals for the means and variances were obtained by bootstrap. 

<!-- For cases still in hospital, i.e. cases which do not yet have an outcome by the date of this report, the  likelihood function for the estimation of the time from admission to any outcome was constructed to allow for right censoring. -->

All analysis were performed using the R statistical software (R Core Team, 2019).
