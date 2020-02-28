# Minimum Detectable Effect Size (MDES) in a 2-level cluster randomised controlled trial

"**mdesapp**" is an online calculator that provides the Minimum Detectable Effect Size (MDES) in a 2-level Clustered Randomised Control Trial setting.
This is when whole clusters (instead of individuals) are randomised.
A typical situation is when complete classes or schools are allocated to either the intervention or the control/comparison group.

This tool was created according to the guidelines provided by the Education Endowment Foundation (EEF, 2013), which are available [**here**](https://educationendowmentfoundation.org.uk/public/files/Evaluation/Writing_a_Protocol_or_SAP/Pre-testing_paper.pdf). [^1]

The specific guideline about MDES is on page 4 and takes the formulae described in Bloom et al. (2007)[^2]

The online tool is available [**here**](https://patricio-troncoso.shinyapps.io/mdesapp/)

***

## Calculation of the MDES

The underlying formula is the following:

$$MDES = M_{J-k} \sqrt{\frac{\rho(1-R^2_2)}{P(1-P)J} + \frac{(1-\rho)(1-R^2_1)}{P(1-P)nJ}}$$

$$where:$$

$MDES$ is the minimum detectable effect size

$M_{J-k}$ is the degrees-of-freedom multiplier

$J$ is the total number of randomised clusters (schools or classes)

$n$ is the number of individuals (pupils) per cluster (schools or classes)

$P$ is the proportion of cluster (schools or classes) randomised to treatment

$\rho$ is the intraclass correlation (ICC, also known as Variance Partition Coefficient, VPC) of the empty multilevel model (without covariates)

$R^2_1$ is the proportion of the variance accounted for by the baseline covariate at the individual (pupils) level

$R^2_2$ is the proportion of the variance accounted for by the baseline covariate at the cluster (schools or classes) level


*Note:* As mentioned in the EEF guidelines, when 20 or more clusters (schools or classes) are randomised, the multiplier $M_{J-k}$ takes the value of 2.8 for a two-tailed MDES and 2.5 for a one-tailed MDES. 

***

## Variance accounted for by a baseline covariate


$R^2_1$ and $R^2_2$ are not to be confused with the coefficient of determination $R^2$ (R-squared) that is used in single-level regression. There is no exact equivalent for $R^2$ in multilevel regression.

When an individual level baseline covariate is added to a multilevel model, it has the potential to affect the variance at the individual and cluster level. Hence, $R^2_2$ should very rarely be zero (or close to zero) when you have an individual baseline covariate.
If $R^2_2$ is ignored in this case, the MDES is overestimated.

In multilevel regression, the proportion of variance accounted for (or predicted) by the covariate (also known as variance explained) is obtained as follows: 

At level 1: 

$$R^2_1 = \frac{\sigma^2_{e, m0} - \sigma^2_{e, m1}}{\sigma^2_{e, m0}}$$

$$where:$$

$\sigma^2_{e, m0}$ is the variance at level 1 (individuals) in the empty multilevel model (without covariates)

$\sigma^2_{e, m1}$ is the variance at level 1 (individuals) in the multilevel model with the baseline covariate

At level 2: 

$$R^2_2 = \frac{\sigma^2_{j, m0} - \sigma^2_{j, m1}}{\sigma^2_{j, m0}}$$

$$where:$$

$\sigma^2_{j, m0}$ is the variance at level 2 (clusters) in the empty multilevel model (without covariates)

$\sigma^2_{j, m1}$ is the variance at level 1 (clusters) in the multilevel model with the baseline covariate


In words, the procedure would be as such :

a) calculate the difference between the estimated variance in a multilevel model with the baseline covariate and another one without it (empty model); 

b) divide the difference by the estimated variance in the empty multilevel model.

This needs to be separately for the individual and the cluster level variance. 

For further details, see for example: Hox, J. (2010, p71)[^3]

***

## Disclaimer: 

This online tool was created using [**RStudio**](https://rstudio.com/) and [**shiny**](https://shiny.rstudio.com/) and it comes with no warranty. Results should be used at the user's discretion.

Copyright (c) 2020. Created by Patricio Troncoso, Manchester Institute of Education, The University of Manchester.

This is distributed with Creative Commons Licence CC BY-NC-SA (Attribution-NonCommercial-ShareAlike)

***

**References:**

[^1]: Bloom, H., Richburg-Hayes, L. and Black, A.R. (2007) Using Covariates to Improve Precision for Studies that Randomise Schools to Evaluate Educational Interventions. Educational Evaluation and Policy Analysis, 29, No.1, pp.30-59.

[^2]: Hox, J. (2010). Multilevel Analysis.Techniques and Applications. Routledge. New York and Hove. 

[^3]: Education Endowment Foundation. (2013). Pre-testing in EEF evaluations. London. Available at: https://educationendowmentfoundation.org.uk/public/files/Social_and_Emotional_Learning_Evidence_Review.pdf