#### Environment ####
library(tidyverse)
library(dslabs)
library(gridExtra)
library(ggrepel)
library(RColorBrewer)

data(murders)
head(murders)

data("heights")
head(heights)

data("gapminder")
head(gapminder)

data("us_contagious_diseases")
head(us_contagious_diseases)
#####

#### Data Visualisation Principles ####

# 1. position and length are preferred over angles and/or area
# =>
# (1) Pie charts are a very bad way of displaying information.
# bar chart or dot chart is a preferable way
# (2) If for some reason you need to make a pie chart, 
# label each pie slice with its respective percentage



# 2. include 0
# (1) When using barplots, it is misinformative to exclude 0.
# bars imply that the length is proportional to the quantities 
# By avoiding 0, 
# relatively small differences can be made to look much bigger
# (2) When using position rather than length, 
# it is then not necessary to include 0. 
# especially when comparing differences between groups 
# relative to the within group variability



# 3. Order categories by a meaningful value
# default: 
# order character string categories alphabetically 
# order factor categories by the factor level
# reorder function helps 
# example:
murders %>%
  mutate(murder_rate = total / population * 100000) %>%
  mutate(state = reorder(state, murder_rate)) %>%
  # order by murder_rate
  ggplot(aes(x = state,
             y = murder_rate)) +
  geom_bar(stat = "identity") +  
  # cancel count function of geom_bar
  coord_flip() +
  # 调转轴向（默认的x轴效果不好）
  theme(axis.text.y = element_text(size = 6)) +
  xlab("")



# 4. Show the data 
# i.e. visualizing the distribution is much more informative. 
# =>
# two ways to improve point plots 
# (1) add jitter 
# adds a small random shift to each point
# (2) alpha blending 
# making the points somewhat transparent. 
# more points fall on top of each other, the darker the plot, 
# which also helps us get a sense of the distribution.
#
heights %>%
  ggplot(aes(x = sex,
             y = height)) +
  geom_jitter(width = 0.1,
              alpha = 0.2)



# 5. For easier comparisons
# (1) Use common axes
#
# (2) Align plots vertically to see horizontal changes 
# and horizontally to see vertical changes
# note: facet_wrap can do this 
#
# (3) Consider transformations
# e.g. logistic for odds, square root for count data, lg for gdp
#
# (4) Visual cues to be compared should be adjacent
# even better to denote the two things we compare with colour



# 6. Colour blind friendly 
# R support self-defined colours 
color_blind_friendly_cols <-
  c("#999999", "#E69F00", "#56B4E9", "#009E73",
    "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# See the paper in the file for detials 



# 7. Plots for two variables 
# In general,should use scatterplots
# However, two exceptions:

# (1) Slope Charts
# comparing variables of the same type, 
# but at different time points and 
# for a relatively small number of comparisons
#
# Example
west <- c("Western Europe",
          "Northern Europe",
          "Southern Europe", 
          "Northern America",
          "Australia and New Zealand")
dat <- gapminder %>%
  filter(year %in% c(2010, 2015) &
           region %in% west &
           !is.na(life_expectancy) &
           population > 10^7)
dat %>%
  mutate(location = ifelse(year == 2010, 1, 2),
         # 给出国名位置
         location = ifelse(year == 2015 &
                    country %in% c("United Kingdom",
                                   "POrtugal"),
                    location + 0.22, location),
         # 微调国名较长的情况
         hjust = ifelse(year == 2010, 1, 0)) %>%
  mutate(year = as.factor(year)) %>%
  ggplot(aes(x = year, 
             y = life_expectancy,
             group = country)) +
  geom_line(aes(colour = country),
            show.legend = F) +
  # 用折线图制作坡面图
  geom_text(aes(x = location,
                label = country,
                hjust = hjust),
            show.legend = F) +
  xlab("") +
  ylab("Life Expectancy")
# permits us to quickly get an idea of changes 
# based on the slope of the lines

# (2) Bland-Altman Plot 
# a.k.a. Tukey mean-difference plot, MA-plot
# difference versus the average (x axis as average)
#
# Example 
# library(ggrepel)
dat %>%
  mutate(year = paste0("life_expectancy_",
                       year)) %>%
  select(country, year, life_expectancy) %>%
  pivot_wider(names_from = year, 
              values_from = life_expectancy) %>%
  mutate(average = (life_expectancy_2015 + 
                    life_expectancy_2010) / 2,
         difference = life_expectancy_2015 - 
                      life_expectancy_2010) %>%
  ggplot(aes(x = average,
             y = difference,
             label = country)) +
  geom_point() +
  geom_text_repel() +
  geom_abline(lty = 2) +
  xlab("Average of 2010 and 2015") +
  ylab("Difference between 2015 and 2010")
# we quickly see which countries have shown the most improvement



# 8. Encoding a third variable 
# (1) encode categorical variables with color and shape. 
# These shapes can be controlled with shape argument.
# 
# (2) For continuous variables, can use colour, intensity, or size
# For colours:
# library(RColorBrewer)
# Sequential colors for data that goes from high to low. 
# High values are clearly distinguished from low values.
display.brewer.all(type = "seq")
# Diverging colors for values that diverge from a center. 
# We put equal emphasis on both ends of the data range
display.brewer.all(type = "div")



# 9. Avoid pseudo-3D plots
# can easily use color to represent the categorical variable 
# instead of using a pseudo-3D:



# 10. Avoid too many significant digits
# two significant figures is more than enough
# can define the number of significant digits globally 
# options(digits = 3).



# 11. Place values being compared on columns rather than rows

#####



#### Exercises ####
# 4-5
dat <- us_contagious_diseases %>%
  filter(year == 1967 &
           disease == "Measles" &
           !is.na(population)) %>%
  mutate(rate = 
           count / population * 10^4 * 52 / weeks_reporting) %>%
  mutate(state = reorder(state, rate))
# reorder by disease rate
dat %>% 
  ggplot(aes(x = state,
                   y = rate)) +
  geom_bar(stat = "identity") +
  coord_flip()

# 7
murders %>%
  mutate(rate = total / population * 10^5) %>%
  ggplot(aes(x = reorder(region, rate, FUN = median),
             y = rate)) +
  # Note we can directly reorder at the aes, which is easier
  geom_boxplot() +
  ylab("Murder Rate by Region")

murders %>%   mutate(rate = total/population*100000) %>%  mutate(region = reorder(region, rate, FUN = median)) %>%  ggplot(aes(x = region,             y = rate)) +  geom_boxplot() +  geom_point()

# 两种写法均可

#####



#### Case Study: Vaccines and Infectious Diseases ####
dat <- us_contagious_diseases %>%
  filter(!state %in% c("Hawaii", "Alaska") &
           # 反向排除
           disease == "Measles"
           ) %>%
  mutate(rate = count / population*10000*52 / weeks_reporting)%>%
  mutate(state = reorder(state, rate))

dat %>% filter(state == "California" &
                 !is.na(rate)) %>%
  ggplot(aes(x = year,
             y = rate)) +
  geom_line() +
  ylab("Cases per 10,000") +
  geom_vline(xintercept = 1963, 
             colour = "blue") # add a vertical line for event
# 1963 is when the vaccine was introduced



# Now plots all states in one plot

# colour for rate 
dat %>%
  ggplot(aes(x = year,
             y = state,
             fill = rate)) +
  geom_tile(colour = "grey50") +
  # tile the region with colours representing disease rates
  scale_x_continuous(expand = c(0,0)) + # cancel auto expand
  # expand: 
  # For position scales, a vector of range expansion constants 
  # used to add some padding around the data to ensure 
  # that they are placed some distance away from the axes.
  scale_fill_gradientn(colours = brewer.pal(9, "Purples"),
                       trans = "sqrt") +
  # scale_*_gradientn creates a n-colour gradient
  # brewer.pal(n, name)
  # n : number of different colours; name: palette name 
  geom_vline(xintercept = 1963,
             colour = "green") +
  theme_minimal() + # 一个模版theme
  theme(panel.grid = element_blank(),
        legend.position = "bottom",
        text = element_text(size = 8)) +
  ggtitle("Measles") +
  ylab("") +
  xlab("")
# This plot makes a very striking argument 
# for the contribution of vaccines.
# But we cannot see the exact quantity 


# If we are willing to lose state information, 
# we can make a version that shows the values with position. 
# We can also show the average for the US
avg <- us_contagious_diseases %>%
  filter(disease == "Measles") %>%
  group_by(year) %>%
  summarise(us_rate = sum(count, na.rm = T) /
              sum(population, na.rm = T) * 10000)

dat %>%
  filter(!is.na(rate)) %>% 
  ggplot() +
  geom_line(aes(x = year,
                y = rate,
                group = state),
            colour = "grey50", # 找不到50种颜色，就用渐变色
            show.legend = F,
            alpha = 0.2,
            linewidth = 1) +
  # rates for all states
  geom_line(aes(x = year,
                y = us_rate),
            data = avg, 
            linewidth = 1) + 
  # rates for us-average
  scale_y_continuous(trans = "sqrt",
                     breaks = c(5, 25, 125, 300)) +
  ggtitle("Cases per 10,000 by states") +
  xlab("") + ylab("") +
  geom_text(data = data.frame(x = 1955, y = 50),
            mapping = aes(x, y, label = "US average"),
            colour = "black") +
  geom_vline(xintercept = 1963, 
             colour = "blue")

#####



#### Exercise ####
# 1-2. for smallpox 
dat <- us_contagious_diseases %>%
  filter(weeks_reporting >= 10 &
           disease == "Smallpox") %>%
  mutate(rate = count / population*10000*52 / weeks_reporting) %>%
  mutate(state = reorder(state, rate))

avg <- us_contagious_diseases %>%
  filter(disease == "Smallpox") %>%
  group_by(year) %>%
  summarise(us_rate = sum(count, na.rm = T) /
              sum(population, na.rm = T) * 10000)

dat %>%
  filter(!is.na(rate)) %>% 
  ggplot() +
  geom_line(aes(x = year,
                y = rate,
                group = state),
            colour = "grey50", # 找不到50种颜色，就用渐变色
            show.legend = F,
            alpha = 0.2,
            linewidth = 1) +
  # rates for all states
  geom_line(aes(x = year,
                y = us_rate),
            data = avg, 
            linewidth = 1) + 
  # rates for us-average
  scale_y_continuous(trans = "sqrt",
                     breaks = c(5, 25, 125, 300)) +
  ggtitle("Cases per 10,000 by states") +
  xlab("") + ylab("") +
  geom_text(data = data.frame(x = 1955, y = 50),
            mapping = aes(x, y, label = "US average"),
            colour = "black") 

  
# 3. 
dat <- us_contagious_diseases %>%
  filter(state == "California" &
           weeks_reporting >= 10) %>%
  mutate(rate = count / population*10000*52 / weeks_reporting)

dat %>%
  ggplot(aes(x = year,
             y = rate,
             colour = disease)) +
  geom_line() +
  ylab("Cases per 10,000") +
  ggtitle("Diseases in California")
 
 
# 4. 
avg <- us_contagious_diseases %>%
  summarise(us_rate = sum(count, na.rm = T) /
              sum(population, na.rm = T) * 10000,
            .by = c(disease, year))
# Notes: easier to use .by argument directly in summarise
# can group by more than one column
avg %>%
  ggplot(aes(x = year,
             y = us_rate,
             colour = disease
             )) +
  geom_line(linewidth = 0.3)

# 两种group_by都可以
us_contagious_diseases %>%  filter(!is.na(population)) %>%  group_by(year, disease) %>%  summarise(rate = sum(count, na.rm = T) /              sum(population, na.rm = T) * 10000) %>%  ggplot(aes(x = year,             y = rate,             colour = disease  )) +  geom_line(linewidth = 0.3)
#####
  



  
  
  
  
  





