
# Biometry Final Question 3

```r


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

data3 = read.csv("final_q3.csv", header = T)

# subset out the boston mountains
boston = subset(data3, data3$streamtype == 1)

# subset just variables of interest
boston = subset(boston, select = c("season", "all", "richness", "stream"))


# determine number of levels in each factor
unique(boston$stream)  # Three Levels (Bear, Cave, Falling)
```

```
## [1] Bear    Cave    Falling
## Levels: Bear Cave Falling Sylamore Tomahawk Water
```

```r
unique(boston$season)  # Four levels (April, June, August and October)
```

```
## [1] April   June    August  October
## Levels: April August June October
```

```r

plot(boston$richness, boston$all)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-11.png) 

```r

par(mfrow = c(1, 2))
hist(boston$richness)  #mostly normal distribution
hist(boston$all)  #heavily skewed
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-12.png) 

```r


boston$all2 = boston$all^(1/2)

par(mfrow = c(1, 2))
hist(boston$richness)  #mostly normal distribution
hist(boston$all2)  #skewed
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-13.png) 

```r

boston$all4 = boston$all^(1/4)

par(mfrow = c(1, 2))
hist(boston$richness)  #mostly normal distribution
hist(boston$all4)  #mostly normal distribution
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-14.png) 

```r

# we'll use these two gariables, richness and all4

# histograms looking at stream

# histograms
p1 = ggplot() + geom_histogram(data = boston, aes(x = all4, fill = stream), 
    position = position_dodge())
# this is fine except for those 0's so maybe we'd need to consider a GLM and
# using a different distribution, but we're just going to go with it
p2 = ggplot() + geom_histogram(data = boston, aes(x = richness, fill = stream), 
    position = position_dodge())
# this looks much improved
grid.arrange(p1, p2, ncol = 1)
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-15.png) 

```r
# these don't totally overlap but are probably ok

# histograms looking at season
p3 = ggplot() + geom_histogram(data = boston, aes(x = all4, fill = season), 
    position = position_dodge())
# this is fine except for those 0's so maybe we'd need to consider a GLM and
# using a different distribution, but we're just going to go with it
p4 = ggplot() + geom_histogram(data = boston, aes(x = richness, fill = season), 
    position = position_dodge())
# this looks much improved
grid.arrange(p3, p4, ncol = 1)
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-16.png) 

```r

# these seem pretty well distributed across the ranges of all and richness
# values


# box plots to look at distribution of variances for stream
p5 = ggplot() + geom_boxplot(data = boston, aes(x = stream, y = all4, fill = stream))
# alrightly
p6 = ggplot() + geom_boxplot(data = boston, aes(x = stream, y = richness, fill = stream))
# looks good
grid.arrange(p5, p6, ncol = 1)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-17.png) 

```r
# there are two outliers in Bear Creek and two in Falling Creek


# box plots to look at distribution of variances for season
p7 = ggplot() + geom_boxplot(data = boston, aes(x = season, y = all4, fill = season))
# alrightly
p8 = ggplot() + geom_boxplot(data = boston, aes(x = season, y = richness, fill = season))
# looks good
grid.arrange(p7, p8, ncol = 1)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-18.png) 

```r
# there is an outlier in April


lm1 = lm(all4 ~ stream, data = boston)
lm2 = lm(richness ~ stream, data = boston)
lm3 = lm(all4 ~ season, data = boston)
lm4 = lm(richness ~ season, data = boston)

summary(lm1)  #Falling is significant, R2 is .4
```

```
## 
## Call:
## lm(formula = all4 ~ stream, data = boston)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -0.3329 -0.1088 -0.0161  0.0790  0.5836 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)     1.0875     0.0329   33.09   <2e-16 ***
## streamCave     -0.0538     0.0520   -1.04      0.3    
## streamFalling  -0.3019     0.0461   -6.55    8e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.171 on 70 degrees of freedom
## Multiple R-squared:  0.405,	Adjusted R-squared:  0.388 
## F-statistic: 23.8 on 2 and 70 DF,  p-value: 1.3e-08
```

```r
summary(lm2)  #Falling is significant R2 is .2
```

```
## 
## Call:
## lm(formula = richness ~ stream, data = boston)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
##  -4.78  -1.25  -0.25   1.39   4.22 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)      9.778      0.423   23.11  < 2e-16 ***
## streamCave      -1.167      0.669   -1.74    0.085 .  
## streamFalling   -2.528      0.593   -4.26  6.2e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.2 on 70 degrees of freedom
## Multiple R-squared:  0.207,	Adjusted R-squared:  0.184 
## F-statistic: 9.11 on 2 and 70 DF,  p-value: 0.000305
```

```r
summary(lm3)  #August and October are signfiicant R2 is .1
```

```
## 
## Call:
## lm(formula = all4 ~ season, data = boston)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -0.5544 -0.1268  0.0321  0.1174  0.6200 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)     0.8615     0.0492   17.51   <2e-16 ***
## seasonAugust    0.1542     0.0687    2.25   0.0279 *  
## seasonJune      0.0483     0.0687    0.70   0.4840    
## seasonOctober   0.1896     0.0706    2.69   0.0091 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.209 on 69 degrees of freedom
## Multiple R-squared:  0.123,	Adjusted R-squared:  0.085 
## F-statistic: 3.23 on 3 and 69 DF,  p-value: 0.0276
```

```r
summary(lm4)  #August is significant R2 is .06
```

```
## 
## Call:
## lm(formula = richness ~ season, data = boston)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -5.588 -1.474 -0.474  1.412  6.111 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)      7.889      0.566   13.93   <2e-16 ***
## seasonAugust     1.585      0.790    2.01    0.049 *  
## seasonJune       0.216      0.790    0.27    0.785    
## seasonOctober    0.699      0.813    0.86    0.392    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.4 on 69 degrees of freedom
## Multiple R-squared:  0.0652,	Adjusted R-squared:  0.0246 
## F-statistic:  1.6 on 3 and 69 DF,  p-value: 0.196
```

```r
# so none of them are actually good fits, but there are significant
# differences in each case

# look at residual plots
par(mfrow = c(2, 2))
plot(lm1)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-19.png) 

```r
# normal QQ plot deviates there is clumping in the residual plot
par(mfrow = c(2, 2))
plot(lm2)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-110.png) 

```r
# normal QQ plot deviates
par(mfrow = c(2, 2))
plot(lm3)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-111.png) 

```r
# normal QQ plot deviates there is clumping in the residual plot
par(mfrow = c(2, 2))
plot(lm4)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-112.png) 

```r
# normal QQ plot deviates there is clumping in the residual plot


# RUN THE MANOVA
manova = manova(cbind(all4, richness) ~ stream + season, data = boston)

summary(manova)  #both season and stream are significant, cool
```

```
##           Df Pillai approx F num Df den Df  Pr(>F)    
## stream     2  0.506    11.36      4    134 5.8e-08 ***
## season     3  0.254     3.25      6    134  0.0051 ** 
## Residuals 67                                          
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r

# pull out the residuals, check them out
res = manova$residuals
plot(res)  #that actually looks pretty good, no wedges
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-113.png) 

