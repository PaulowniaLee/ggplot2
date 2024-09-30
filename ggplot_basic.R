#### Environment ####
install.packages("ggthemes")
install.packages("ggrepel")
install.packages("gridExtra")

library(ggplot2)
library(tidyverse)
library(ggthemes)
library(ggrepel)
library(gridExtra)

library(dslabs)
data("murders")
head(murders)

data("heights")
head(heights)
#####



#### Basic Notes ####

# Break graph into components
# 1. Data
# data table used
# 2. Geometry
# e.g. scatterplot, barplot, histogram, smooth densities, qqplot, boxplot
# i.e. the "format" of plot
# 3. Aesthetic Mapping
# visual cues to represent information provided by dataset 
# (or how properties of the data are connected with features of the graph)
# e.g. x-axis, y-axis, colour

#####



#### Case: U.S. murder data ####

# ggplot object 
# created by ggplot()
murders %>%
  ggplot()


# Geometry 
# ggplot2 create graphs by adding layers (using symbol +)
# DATA %>% ggplot() + LAYER 1 + LAYER 2 + ... + LAYER N
# each layer is a function: geom_X, where X is the name of geometry


# Aesthetic mappings
# aes function connects data with graph by defining aesthetic mappings
murders %>% ggplot() +
  # create object 
  geom_point(aes(x = population/10^6, y = total)) 
  # first layer and its aesthetic mappings


# Adding more layers
murders %>% ggplot() +
  geom_point(aes(x = population/10^6, y = total)) +
  geom_text(aes(x = population/10^6, y = total, label = abb )) 
# label each state with abb


# Can also manipulate other argument of geometry function 
murders %>% ggplot() +
  geom_point(aes(x = population/10^6, y = total), 
             size = 3, color = "blue") +
  geom_text(aes(x = population/10^6, y = total, label = abb),
            size = 3.5, color = "red", nudge_x = 1.5) #nudge moves text
# since these arguments are not mappings, place them outside aes function


# Global aesthtic mappings
# if we define a mapping in ggplot, then all added layers will default 
# to this mapping
murders %>% ggplot(aes(x = population/10^6, y = total, label = abb)) +
  geom_point(size = 3, color = "blue") +
  geom_text(size = 3.5, color = "red", nudge_x = 1.5) +
  geom_text(aes(x = 10, y = 800, label = "Hello There!"))
  # can overwrite global mapping by defining local ones


# Scale 
# convert to log-scale by adding scale layer 
murders %>% ggplot(aes(x = population/10^6, y = total, label = abb)) +
  geom_point(size = 3, color = "blue") +
  geom_text(size = 3.5, color = "red", nudge_x = 0.05) + 
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10")
# nudge must be made smaller for log scale 


# Labels and Titles
murders %>% ggplot(aes(x = population/10^6, y = total, label = abb)) +
  geom_point(size = 3, color = "blue") +
  geom_text(size = 3.5, color = "red", nudge_x = 0.05) + 
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  labs(x = "Populations in millions (log scale)",
       y = "Total number of murders (log scale)",
       title = "US Gun Murders in 2010")


# Categories as Colours 
murders %>% ggplot(aes(x = population/10^6, y = total, label = abb)) +
  geom_point(aes(col = region), size = 3) +
  # note this automatically add a legend(图例), can turn off manually
  geom_text(size = 3.5, nudge_x = 0.05) + 
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  labs(x = "Populations in millions (log scale)",
       y = "Total number of murders (log scale)",
       title = "US Gun Murders in 2010")


# Annotation, shapes, and adjustment 
# (things not directly derived from aesthetic mapping)
r <- murders %>%
  summarise(rate = sum(total) / sum(population) * 10^6) %>%
  pull(rate) # compute national average murder rate

murders %>% ggplot(aes(x = population/10^6, y = total, label = abb)) +
  geom_point(aes(col = region), size = 3) +
  geom_abline(intercept = log10(r),
              lty = 1, color = "darkgrey") + # lty denotes line type
  # average murder rate line
  geom_text(size = 3.5, nudge_x = 0.05) + 
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  labs(x = "Populations in millions (log scale)",
       y = "Total number of murders (log scale)",
       title = "US Gun Murders in 2010") +
  scale_color_discrete(name = "Region") 
 # small adjustment to legend (chaneg region to Region)
# ggplot is flexible for similar adjustments


# Add-on Packages
# ggthemes
# provide pre-made styles for graph
# ggrepel 
# deal with position of labels
# e.g. prevent them falling on top of each other 

#####



#### Summary Code for Above Case ####
r <- murders %>%
  summarise(rate = sum(total) / sum(population) * 10^6) %>%
  pull(rate) # compute national average murder rate

murders %>% ggplot(aes(x = population/10^6, y = total, label = abb)) +
  geom_point(aes(col = region), size = 3) +
  geom_abline(intercept = log10(r),
              lty = 1, color = "darkgrey") + 
  geom_text(size = 3.5, nudge_x = 0.05) + 
  geom_text_repel() + # from ggrepel 
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  labs(x = "Populations in millions (log scale)",
       y = "Total number of murders (log scale)",
       title = "US Gun Murders in 2010") +
  scale_color_discrete(name = "Region") +
  theme_economist() # from ggthemes 

#####



#### Other Functions ####

# Quick Plots #
x <- log10(murders$population) 
y <- murders$total
qplot(x, y)
# Caution ! 
# qplot has been deprecated, it is recommended to use ggplot anyway

# Grids of Plots #
# gridExtra package
p1 <- qplot(x)
p2 <- qplot(x,y) 
grid.arrange(p1, p2, 
             ncol = 2)
# again not a good idea to use qplot


#####



#### Exercise ####

murders %>%
  ggplot() %>%
  class()
# class of ggplot object

murders %>%
  ggplot() %>%
  print()
# direct print get nothing

# 8 - 11
murders %>%
  ggplot(aes(x = population / 10^6, y = total, label = abb)) +
  geom_label(col = "blue")

# 12 - 13
murders %>%
  ggplot(aes(x = population / 10^6, y = total, label = abb)) +
  geom_label(aes(col = region), size = 3)

#####