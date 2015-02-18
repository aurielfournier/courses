# Biometry Final 2014 - Question 1

```r
# Biometry Final 2014 - Question 1

# required libraries
library(ggplot2)
library(devtools)
```

```
## 
## Attaching package: 'devtools'
## 
## The following objects are masked from 'package:utils':
## 
##     ?, help
## 
## The following object is masked from 'package:base':
## 
##     system.file
```

```r
library(digest)
library(gridExtra)
```

```
## Loading required package: grid
```

```r

# set working directory
setwd("~/Downloads")
data1 = read.csv("final_q1.csv", header = T)

plot(data1)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-11.png) 

```r

# box plots to look at distribution of variances
ggplot() + geom_boxplot(data = data1, aes(x = state, y = cholesterol, fill = state))
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-12.png) 

```r
# these are similar

par(mfrow = c(1, 2))
hist(data1$age)  #nicely normally distributed
hist(data1$cholesterol)  #slightly skewed to the left
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-13.png) 

```r
data1$chol2 = data1$cholesterol^(1/2)
hist(data1$chol2)  #this is a better distribtuion
data1 = data1[, c("chol2", "state", "age")]

# this is just a one way anova to look at the difference between longevity
# and mating partner treatment
lm1 = lm(data = data1, chol2 ~ state)
summary(lm1)  #there is not a difference between state and cholesterol level
```

```
## 
## Call:
## lm(formula = chol2 ~ state, data = data1)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -3.672 -0.858 -0.292  1.290  4.253 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)     14.255      0.615   23.16   <2e-16 ***
## stateNebraska    0.360      0.773    0.46     0.65    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.04 on 28 degrees of freedom
## Multiple R-squared:  0.00766,	Adjusted R-squared:  -0.0278 
## F-statistic: 0.216 on 1 and 28 DF,  p-value: 0.646
```

```r
# look at residuals
par(mfrow = c(2, 2))
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-14.png) 

```r
plot(lm1)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-15.png) 

```r
# the residuals dont' look bad, there is slight deivation from the normal QQ
# plot


lm2 = lm(data = data1, age ~ state)
summary(lm2)  # this is not significant, so state of origin do not explain age, shocking stuff
```

```
## 
## Call:
## lm(formula = age ~ state, data = data1)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -27.95 -12.73  -1.52  12.02  32.05 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)      53.09       4.90   10.84  1.6e-11 ***
## stateNebraska    -7.14       6.16   -1.16     0.26    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 16.3 on 28 degrees of freedom
## Multiple R-squared:  0.0459,	Adjusted R-squared:  0.0118 
## F-statistic: 1.35 on 1 and 28 DF,  p-value: 0.256
```

```r


# linearity assumptions
ggplot() + geom_point(data = data1, aes(x = chol2, y = age, group = state, colour = state))
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-16.png) 

```r

# ancova looking at the impact of mating partner treatment and thorax lenght
# on longetivy
lm3 = lm(data = data1, chol2 ~ age + state)
summary(lm3)  # there is a significant difference by age, but not by state
```

```
## 
## Call:
## lm(formula = chol2 ~ age + state, data = data1)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -2.884 -0.862 -0.104  0.854  2.344 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)     9.4406     0.9884    9.55  3.8e-10 ***
## age             0.0907     0.0167    5.42  9.9e-06 ***
## stateNebraska   1.0074     0.5580    1.81    0.082 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.44 on 27 degrees of freedom
## Multiple R-squared:  0.525,	Adjusted R-squared:  0.49 
## F-statistic: 14.9 on 2 and 27 DF,  p-value: 4.34e-05
```

```r


# check for paralell slopes
lm4 = lm(data = data1, chol2 ~ age * state)
summary(lm4)  #age is significant, but there is not a signficant interaction
```

```
## 
## Call:
## lm(formula = chol2 ~ age * state, data = data1)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -2.8073 -0.9686 -0.0335  0.9175  2.4740 
## 
## Coefficients:
##                   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         8.4457     1.8584    4.54  0.00011 ***
## age                 0.1094     0.0340    3.22  0.00346 ** 
## stateNebraska       2.2852     2.0899    1.09  0.28424    
## age:stateNebraska  -0.0249     0.0392   -0.63  0.53100    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.45 on 26 degrees of freedom
## Multiple R-squared:  0.532,	Adjusted R-squared:  0.478 
## F-statistic: 9.85 on 3 and 26 DF,  p-value: 0.000162
```

```r

# so we can go back to lm1, which was a regular ANOVA, but there was also
# not a difference there this suggests that there is no relationship between
# state and cholesterol level, but there is one with age, which is
# increasing in both cases.

```


