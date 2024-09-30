##### Environment ####
library(tidyverse)
library(ggplot2)

library(dslabs)
data("heights")
head(heights)

#####



#### Variable Types #####
# Categorical 
# When each entry in a vector comes from one of a small number of groups, 
# we refer to the data as categorical data.
# ordered categorical data are referred to as ordinal data.
# e.g. 
# sex, regions

# Numerical 
# can be discrete or continuous
# discrete numeric data can be considered ordinal, but not always applicable

#####



#### Some Elements ####
# mean(), sd() 求均值和方差

# mad() median absolute deviation 
# (median of the absolute deviations from the median)

# scale(), 化标

# pnorm() 由值得quantile qnorm() 由quantile得值
# 默认是standard normal，可由变量调节为一般normal

# quantile() obtain quantiles from data

# Boxplots
# box defined by the 25% and 75% percentile 
# the whiskers showing the range. 
# The distance between these two is called the interquartile range. 
# The points are outliers 
# The median is shown with a horizontal line. 
# Today, we call these boxplots.

# Stratification 
# divide the data into groups based on some (categorical) varialbes
# call this procedure stratification and  the resulting groups strata.

#####



#### Computation ####

# empirical CDF drawing
a <- seq(min(heights$height),
         max(heights$height),
         length = 100)
# 从一到一百的probability 
cdf_function <- function(x){
  mean(heights$height <= x)
  # 对于每个x,求小于这个x的值的平均数
  # 每个x都是上面定义的区间的端点
  # 相当于是取每个区间的平均值
}
cdf_values <- sapply(a, cdf_function)
plot(a, cdf_values)

# use mean to get proportion 
mean(x > 69 & x <= 72) #直接用logical function

#####

#### ggplot2 geometries ####

# Barplots 
tab <- murders %>%
  count(region) %>%
  mutate(proportion = n/sum(n))
tab
# we can use pre-made table directly 
tab %>%
  ggplot(aes(x = region, y = proportion)) +
  geom_bar(stat = "identity") # cancel count function of geom_bar
# can also use data directly 
murders %>%
  ggplot(aes(region)) +
  geom_bar() # auto counting is enabled here



# Histograms
# only required argument is x (which is the first argu)
# the variable for which we will construct a histogram
heights %>%
  filter(sex == "Female") %>%
  ggplot(aes(height)) +
  geom_histogram(binwidth = 1, # without binwidth the pic can be ugly
                 fill = "blue", # 内填充颜色
                 col = "black" # 边框线颜色
                 ) +
  xlab("Male Heights in Inches") +
  ggtitle("Histogram")



# Density Plots
heights %>%
  filter(sex == "Female") %>%
  ggplot(aes(height)) +
  geom_density(fill = "blue", # 内填充颜色
               adjust = 0.5 # smoothness of lines, default is 1
               )



# Boxplots 
help("geom_boxplot")
heights %>%
  ggplot(aes(x = sex, y = height)) +
  geom_boxplot(outlier.colour = "darkblue", # outlier colour
               notchwidth = 1, # box line width
               colour = "darkred" # box colour
               )



# qqplots
# QQ plot也就是Quantile-Quantile Plots。
# 是通过比较两个概率分布的分位数对这两个概率分布进行比较的概率图方法
# 其想法就是，如果现在有从某个类型的概率分布中抽取的N个数据，
# 那么如果想确定这个概率分布是否接近normal distribution该怎么办呢？

# 如果这些样本点并不符合normal distribution,
# 那么画出来的QQplot中的点就不呈一条直线分布
# 另外，我们会发现如果样本点确实取自一个normal distribution, 
# 那么样本点数量越大，QQplot就越接近一条直线
heights %>%
  filter(sex == "Male") %>%
  ggplot(aes(sample = height)) +
  geom_qq() # default is to compare standard normal

# 推荐先化标再qq
heights %>%
  filter(sex == "Male") %>%
  ggplot(aes(sample = scale(height))) +
  geom_qq() +
  geom_abline() # 直线用于比较



# Images 
help("geom_tile")
# geom_raster() is a high performance special case for 
# when all the tiles are the same size.
# geom_rect() and geom_tile() are the same, but parameterised differently
# geom_rect() uses the locations of the four corners 
# (xmin, xmax, ymin and ymax)
# geom_tile() uses the center of the tile and its size 
# (x, y, width, height)

expand.grid(x = 1:12, y = 1:10) %>% # create a data frame
  mutate(z = 1: 120) %>%
  ggplot(aes(x, y, fill = z)) +
  geom_raster() +
  scale_fill_gradientn(colours = terrain.colors(10))

#####



#### Exercise ####
# 1 - 4
heights %>%
  ggplot(aes(x = height)) +
  geom_histogram(binwidth = 1)

# 8 
heights %>%
  ggplot(aes(x = height, fill = sex)) +
  geom_density(alpha = 0.2 
               # transparency parameter to distinguish groups
               )

#####
  