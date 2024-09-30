#### Environment ####
library(tidyverse)
library(dslabs)
library(ggridges)

data(gapminder)
head(gapminder)

gapminder <- gapminder %>%
  mutate(dollars_per_day = gdp/population/365)

#####



#### fertility and life-expectancy across continent: Facet ####

filter(gapminder, year == 1962) %>%
  ggplot(aes(x = fertility,
             y = life_expectancy,
             color = continent)) +
  geom_point() 
# We want to compare 1962 and 2012 with side-by-side plots

# facet_grid function 
filter(gapminder, 
       year %in% c(1962, 2012)) %>%
  ggplot(aes(x = fertility,
             y = life_expectancy,
             color = continent)) +
  geom_point() +
  facet_grid(continent ~ year) # plot for each continent/year pair
# can compare only year
filter(gapminder, 
       year %in% c(1962, 2012)) %>%
  ggplot(aes(x = fertility,
             y = life_expectancy,
             color = continent)) +
  geom_point() +
  facet_grid(. ~ year) # . means that we only use "year" variable

# facet_wrap()
# automatically wrapping the series of plots
years <- c(1962, 1980, 1990, 2000, 2012)
continents <- c("Europe", "Asia")
gapminder %>%
  filter(year %in% years & continent %in% continents) %>%
  ggplot(aes(x = fertility,
             y = life_expectancy,
             col = continent)) +
  geom_point() +
  facet_wrap(. ~year)
# Note: scales of plots are automatically fixed for comparison
# can turn off by scales = "free", but it will make pic ugly

#####



#### Evolution of one nation: Time series plots ####

gapminder %>%
  filter(country == "United States") %>%
  ggplot(aes(x = year,
             y = fertility)) +
  geom_line()
# line is good for one nation, but comparing two nations need more

countries <- c("South Korea", "Germany")
gapminder %>%
  filter(country %in% countries & !is.na(fertility)) %>%
  # is.na() check whether the column is NA
  ggplot(aes(x = year,
             y = fertility,
             colour = country)) +
  # one colour for one country
  geom_line()

# can also use labels instead of legends
labels <- data.frame(country = countries, 
                     x = c(1975, 1965),
                     y = c(58, 72))
# 这里countries是用的前面的定义
# 这两行是在用图坐标定义label位置
gapminder %>%
  filter(country %in% countries) %>%
  ggplot(aes(x = year,
             y = life_expectancy,
             colour = country)) +
  geom_line() +
  geom_text(data = labels,
            aes(x, y, label = country),
            size = 5) +
  theme(legend.position = "none")
# theme:
# customize the non-data components of plots: 
# i.e. titles, labels, fonts, background, gridlines, and legends
# 内部选项繁多且自由，建议用help看文档

#####



#### Data Transformation ####

# Average Daily Income: (log2)
# more informative to check base 2:
# 1 extremely poor, 2 very poor, 4 poor, 8 middle, 16 well off,
# 32 rich, 64 very rich

gapminder %>%
  filter(year == 1970 & !is.na(gdp)) %>%
  ggplot(aes(log2(dollars_per_day))) +
  geom_histogram(binwidth = 1,
                 colour = "black")
# the plots give a good summary of mid to lower income countries

# Population (log10)
gapminder %>%
  filter(year == 1970) %>%
  ggplot(aes(log10(population))) +
  geom_histogram(binwidth = 0.5,
                 colour = "black")
# Note:
# can also use scale_x_continuous for transformation 
gapminder %>%
  filter(year == 1970) %>%
  ggplot(aes(population)) +
  geom_histogram(binwidth = 0.5,
                 colour = "black") +
  scale_x_continuous(trans = "log10")
# 差别在于x轴上标注会不同


# local modes:
# locations where distribution goes up and down again 
# say some distribution has multiple modes

#####



#### Compare multiple distributions ####
gapminder %>%
  filter(year == 1970 & !is.na(gdp)) %>%
  mutate(region = reorder(region, dollars_per_day,
                          FUN = median)) %>%
  # reorder by median of regional daily income
  ggplot(aes(x = dollars_per_day,
             y = region)) +
  geom_point() +
  scale_x_continuous(trans = "log2")

# Indeed there exists west vs. rest dichotomy
# further group the regions
gapminder <- gapminder %>%
  mutate(group = case_when
         (
           region %in% c("Western Europe", 
                         "Northern Europe",
                         "Southern Europe", 
                         "Northern America",
                         "Australia and New Zealand") ~ "West",
           region %in% c("Eastern Asia", 
                         "South-Eastern Asia") ~ "East Asia", 
           region %in% c("Caribbean", 
                         "Central America",
                         "South America") ~ "Latin America",
           continent == "Africa" & 
             region != "Northern Africa" ~ "Sub-Saharan",
           TRUE ~ "Others"
         ))
sum(is.na(gapminder$group)) # check whether there is na in group
gapminder <- gapminder %>%
  mutate(group = factor(group,
                        levels = c("Others",
                                   "Latin America",
                                   "East Asia",
                                   "Sub-Saharan",
                                   "West")))
# change to factor 



# Boxplots #
gapminder %>%
  filter(year == 1970 & !is.na(gdp)) %>%
  ggplot(aes(x = group,
             y = dollars_per_day)) +
  geom_boxplot() +
  scale_y_continuous(trans = "log2") +
  xlab("") + #取消x轴标题
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1)) #调整x轴图注
# boxplots summarise the data into five numbers
# can add geom_point to avoid this 
gapminder %>%
  filter(year == 1970 & !is.na(gdp)) %>%
  ggplot(aes(x = group,
             y = dollars_per_day)) +
  geom_boxplot() +
  scale_y_continuous(trans = "log2") +
  xlab("") + 
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1)) +
  geom_point(alpha = 0.5) #use transparent points 

# Note:
# boxplots cannot discover the shape of distribution 
# (since it summarises everything with five numbers)



# Ridge Plot #
# library(ggridges)

gapminder %>%
  filter(year == 1970 & !is.na(dollars_per_day)) %>%
  ggplot(aes(x = dollars_per_day,
             y = group)) +
  #note here group(categorical factor is the y axis)
  scale_x_continuous(trans = "log2") +
  geom_density_ridges(scale = 1,
  # scale = 1 means no overlap, larger value means more overlap
  jittered_points = T,
  # add data points to the ridge plot
  position = position_points_jitter(height = 0),
  point_shape = "|",
  # height of points means nothing, so move them to the x axis
  point_size = 3,
  point_alpha = 1,
  # make points small 
  alpha = 0.7
  )

#####



#### Multiple Distributions: Across Years ####
years <- c(1970, 2010)
gapminder %>%
  filter(year %in% years & !is.na(gdp)) %>%
  mutate(west = ifelse(group == "West",
                       "West", 
                       "Developing")) %>%
  # ifelse(test, yes, no)
  # test: 命题检验
  # yes: value returned for true result of test
  # no: value returned for false result of test
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1,
                 colour = "black", #边线颜色
                 fill = "blue") + #内填充颜色
  scale_x_continuous(trans = "log2") +
  facet_grid(year ~ west)

# We notice 2010 has more countries: filter data
country_list_1 <- gapminder %>%
  filter(year == 1970 & !is.na(dollars_per_day)) %>%
  pull(country)
country_list_2 <- gapminder %>%
  filter(year == 2010 & !is.na(dollars_per_day)) %>%
  pull(country)
country_list <- intersect(country_list_1, country_list_2)
# draw again
gapminder %>%
  filter(year %in% years & country %in% country_list) %>%
  mutate(west = ifelse(group == "West",
                       "West", 
                       "Developing")) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1,
                 colour = "black", #边线颜色
                 fill = "blue") + #内填充颜色
  scale_x_continuous(trans = "log2") +
  facet_grid(year ~ west)

# We see that rich countries have become a bit richer, but 
# in general poor countries have improved more


# Divide into regions: Boxplots
gapminder %>%
  filter(year %in% years &
         country %in% country_list) %>%
  mutate(year = factor(year)) %>%
  # turn year into a factor, so ggplot can separate and color it
  ggplot(aes(x = group,
             y = dollars_per_day,
             fill = year)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1)) +
  scale_y_continuous(trans = "log2") +
  xlab("")
# 直接比较每个区域的变化更为直观



# West vs. Rest: Density plots comparison
# the default is to have area of each distribution add up to 1
# but we have 87 developing countries and 21 western countries
# => 
# Accessing computed variables 
# multiply the y-axis values by the size of the group 

gapminder %>%
  filter(year %in% years & country %in% country_list) %>%
  mutate(group = ifelse(group == "West", 
                        "West",
                        "Developing")) %>%
  ggplot(aes(x = dollars_per_day,
             y = after_stat(count), 
             # multiply the y-axis by the size of the group 
  # aes有两层变换，为after_stat和after_scale
             fill = group)) +
  scale_x_continuous(trans = "log2",
                     limits = c(0.125, 300)) +
  geom_density(alpha = 0.2,
               bw = 0.75) +
  # 手动调整bandwidth使两图相同
  facet_grid(year ~ .)

  

# compare different regions to see the difference

# density ridges plots #
gapminder %>%
  filter(year %in% years &
           !is.na(dollars_per_day)) %>%
  ggplot(aes(x = dollars_per_day,
             y = group)) +
  scale_x_continuous(trans = "log2") +
  geom_density_ridges(scale = 1.5) +
  facet_grid(. ~ year)

# stacked density plots #
gapminder %>%
  filter(year %in% years & country %in% country_list) %>%
  group_by(year) %>%
  mutate(weight = population / sum(population) * 2) %>%
  ungroup() %>%
  # get the weight needed, and return the dataset to original
  ggplot(aes(x = dollars_per_day,
             fill = group,
             weight = weight # generate weighted density 
             )) +
  scale_x_continuous(trans = "log2",
                     limits = c(0.125, 300)) +
  geom_density(
               alpha = 0.2,
               bw = 0.75,
               position = "stack" # generate overlapping plots
               ) +
  facet_grid(year ~ .)
# weighted distributions are more accurate for countries with 
# a big population

#####



#### Ecological Fallacy: log of odds ####
# p is a probability
# p / (1-p) is called odds 

# ecological fallacy: 
# assuming conclusions made from the average apply to all members

# study infant mortality and income level 
gapminder <- gapminder %>%
  # redefine groups for our purpose
  mutate(group = case_when(
    region %in% c("Western Europe", 
                  "Northern Europe",
                  "Southern Europe", 
                  "Northern America",
                  "Australia and New Zealand") ~ "The West",
    region %in% "Northern Africa" ~ "Northern Africa",
    region %in% c("Eastern Asia", 
                  "South-Eastern Asia") ~ "East Asia",
    region == "Southern Asia" ~ "Southern Asia",
    region %in% c("Central America", 
                  "South America", 
                  "Caribbean") ~ "Latin America",
    continent == "Africa" 
    & region != "Northern Africa" ~ "Sub-Saharan Africa",
    region %in% c("Melanesia", 
                  "Micronesia", 
                  "Polynesia") ~ "Pacific Islands"
  ))

surv_income <- gapminder %>%
  filter(year == 2010 &
           !is.na(gdp) &
           !is.na(infant_mortality) &
           !is.na(group)) %>%
  group_by(group) %>%
  summarise(infant_survival_rate = 1 - 
  sum(infant_mortality/1000*population)/sum(population),
  income = sum(gdp)/sum(population)/365) 

surv_income %>% arrange(income)

surv_income %>%
  ggplot(aes(x = income,
             y = infant_survival_rate,
             label = group,
             colour = group)) +
  scale_x_continuous(trans = "log2",
                     limits = c(0.25, 150)) +
  scale_y_continuous(trans = "logit",
                     limits = c(0.875, 0.9981),
                     breaks = c(.85,
                                .90,
                                .95,
                                .99,
                                .995,
                                .998)) +
                     geom_label(size = 3,
                                show.legend = F)
#But should remember that average cannot conclude for every member




  



