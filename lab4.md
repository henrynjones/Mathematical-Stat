---
title: "Lab 4"
author: "Daniel Lewinsohn and Henry Jones"
date: '2023-05-11'
output: 
  html_document: 
    keep_md: yes
---


```r
results <- read.csv("/Users/henryjones/Desktop/Math_CC/MA_417/results_2019.csv")
results2 <- subset(results, neutral == FALSE)
results2$home_win <- 1*(results2$home_score - results2$away_score > 0)
mean(results2$home_win)
```

```
## [1] 0.5286236
```

## Exercise 1

Use an exact sampling distribution for p̂=  X to construct a 95% confidence interval for p. Hint: Use the binomial distribution in your computations.


```r
library(rootSolve)
y <- sum(results2$home_win)
n <- length(results2$home_win)
a <- .05
f1 <- function(x){pbinom(y-1, n, x) - 1+a/2}
f2 <- function(x){pbinom(y, n, x) - (a/2)}
uniroot(f1, interval=c(0,1))
```

```
## $root
## [1] 0.4938007
## 
## $f.root
## [1] 5.920902e-05
## 
## $iter
## [1] 13
## 
## $init.it
## [1] NA
## 
## $estim.prec
## [1] 6.103516e-05
```

```r
uniroot(f2, interval=c(0,1))
```

```
## $root
## [1] 0.5632077
## 
## $f.root
## [1] 5.106583e-05
## 
## $iter
## [1] 13
## 
## $init.it
## [1] NA
## 
## $estim.prec
## [1] 6.103516e-05
```



## Exercise 2

Use a large-sample approximation for the sampling distribution of p̂=X to construct a 95% confidence interval for p.

X_bar follow N(p, sigma2 / n).


```r
c <- qnorm(.975, 0, 1)

x_bar <- mean(results2$home_win)
var_est <- var(results2$home_win)
n <- length(results2$home_win)
lower_interval <- x_bar - ((c * sqrt(var_est)) / sqrt(n))
higher_interval <- x_bar + ((c * sqrt(var_est)) / sqrt(n))
print(lower_interval)
```

```
## [1] 0.4944573
```

```r
print(higher_interval)
```

```
## [1] 0.56279
```

Our low and high bounds for our 95% confidence interval.

## Exercise 3


```r
set.seed(8)
X_bars <- numeric(20)

n <- length(results2$home_win)

for(i in 1:1000){
  samp <- sample(results2$home_win, n, replace=T)
  X_bars[i] <- mean(samp)
}

q_one <- quantile(X_bars, .025)
q_two <- quantile(X_bars, .975)

print(q_one)
```

```
##      2.5% 
## 0.4957369
```

```r
print(q_two)
```

```
##     97.5% 
## 0.5651949
```

## Exercise 4

We use Bernoulli with sample mean as our distribution to generate samples.


```r
set.seed(8)
p <- mean(results2$home_win) # use sample mean
X_bars <- numeric(20)

n <- length(results2$home_win)

for(i in 1:1000){
  samp <- rbinom(n, 1, p)
  X_bars[i] <- mean(samp)
}

q_one <- quantile(X_bars, .025)
q_two <- quantile(X_bars, .975)

print(q_one)
```

```
##      2.5% 
## 0.4957065
```

```r
print(q_two)
```

```
##     97.5% 
## 0.5615104
```


## Exercise 5


```r
# Beta(1,1) is uniform
a <- 1
b <- 1

post_a <- a + sum(results2$home_win)
post_b <- b + length(results2$home_win) - sum(results2$home_win)

c_one <- qbeta(.025, post_a, post_b)
c_two <- qbeta(.975, post_a, post_b)

print(c_one)
```

```
## [1] 0.4944136
```

```r
print(c_two)
```

```
## [1] 0.5625631
```


Use one with informative prior that has about expected value equal to sample mean. Beta(10, 10)

```r
a <- 10
b <- 10

post_a <- a + sum(results2$home_win)
post_b <- b + length(results2$home_win) - sum(results2$home_win)

c_one <- qbeta(.025, post_a, post_b)
c_two <- qbeta(.975, post_a, post_b)

print(c_one)
```

```
## [1] 0.4941689
```

```r
print(c_two)
```

```
## [1] 0.5615911
```


## Exercise 6

Large sample approximation CI: [0.4944573, 0.56279]

Non-parametric bootstrap CI: [0.4957369, 0.5651949]

Parametric bootstrap CI: [0.4957065, 0.5615104]

Bayesian with uninformative prior CI: [0.4944136, 0.5625631]

Bayesian with informative prior CI: [0.4941689, 0.5615911]

There are not any huge differences between the confidence/credible intervals from all of the methods. This likely due to the large sample size. There is not a way to know which procedure is best for this problem, as we do not know the true value of p, because this is real data. With small n, the large sample and bootstraps would not work very well. However, the Bayesian approaches could still do well, especially with an informative prior. The exact solution would still work well with small n.
