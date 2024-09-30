#### Environment ####
library(tidyverse)
library(dslabs)

data("outlier_example")
#####

#### How to detect outliers? ####

# 1. Boxplot/Median
boxplot(outlier_example)
# 可以直接看出outlier
# 本质上是通过median和极值的差距看出的



# 2. Inter Quartile Range IQR
# refer to the difference between the 3rd and 1st quantile 
# like median, robust against outilers
#
# IQR/1.349 approximates the sd 
# should the data follow a normally distribution without outliers
IQR(outlier_example) / 1.349
# still get sd despite outlier
sd(outlier_example)
# the difference reveals the presence of an outlier



# 3. MAD median absolute deviation 
# MAD = median(distance between values and the median of data)
# sd = MAD * 1.4826 (approximation)
mad(outlier_example)
# mad already includes above correction, and we get sd again 



# 4. Tukey's definition of an outlier
# an outlier is anything outside the range:
# [Q1 − 1.5×(Q3 −Q1), Q3 + 1.5×(Q3 −Q1)]
# where IQR = Q3-Q1
#
# for normal distribution:
q3 <- qnorm(0.75)
q1 <- qnorm(0.25)
iqr <- q3 - q1
r <- c(q1 - 1.5*iqr, q3 + 1.5*iqr) 
1- (pnorm(r[2]) - pnorm(r[1]))
# we actually expect 0.7% of the data to become the outlier

# can also make outliers rarer, such as 3*IQR
# called "far out outliers"
q3 <- qnorm(0.75)
q1 <- qnorm(0.25)
iqr <- q3 - q1
r <- c(q1 - 3*iqr, q3 + 3*iqr) 
1- (pnorm(r[2]) - pnorm(r[1]))
# for normal distribution, 0% of the data would become the outlier
max_height <- quantile(outlier_example, 0.75) + 
  3*IQR(outlier_example)
# but for the example above, we see it still surpass the max-height



# 5. qqplot 
qqnorm(outlier_example)
# 同样直接看出outlier
qqnorm(outlier_example[outlier_example < max_height])
# 去除该outlier后，的确为normal distribution

