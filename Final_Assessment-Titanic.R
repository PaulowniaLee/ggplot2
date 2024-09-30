#### Environment ####
install.packages("titanic")

library(tidyverse)
library(titanic)
library(ggridges)

titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))
#####



#### Finanl Assessment ####

# 1. Know the data
?titanic_train



# 2. density plots of age grouped by sex
titanic %>%
  filter(!is.na(Age)) %>%
  group_by(Sex) %>%
  ggplot(aes(x = Age,
             fill = Sex)) +
    geom_density(alpha = 0.2,
                 position = "stack") +
  scale_x_continuous(breaks = c(5, 18, 35, 40)) +
  coord_flip()

  
  titanic %>%
    filter(!is.na(Age)) %>%
    group_by(Sex) %>%
    ggplot(aes(x = Age,
               y = Sex)) +
    geom_density_ridges(scale = 1.5) +
    scale_x_continuous(breaks = c(5, 18, 35, 40)) +
    coord_flip()
  
titanic %>%
  filter(!is.na(Age)) %>%
  ggplot(aes(x = Age,
             y = after_stat(count),
             fill = Sex)) +
  geom_density(alpha = 0.2,
               bw = 0.75)

# Note:
# density stack, coord_flip, counts on y-axis, flexible breaks
# all useful for exploring data distribution 
    


# 3. qqplots for Age Distribution 
params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarise(mean = mean(Age), sd = sd(Age))

titanic %>%
  filter(!is.na(Age)) %>%
ggplot(aes(sample = Age)) +
geom_qq(dparams = params) +
  geom_abline()



# 4. Survival by Sex
titanic %>%
  ggplot(aes(x = Survived,
             fill = Sex)) +
  geom_bar()
# default plot, two bars, but can fill by sex

titanic %>%
  ggplot(aes(x = Survived,
             fill = Sex)) +
  geom_bar(position = position_dodge()) 
# each sex has two bars, more direct 



# 5. Survival by Age
titanic %>%
  filter(!is.na(Age)) %>%
  ggplot(aes(x = Age,
         y = after_stat(count),
         fill = Survived)) +
  geom_density(alpha = 0.2)
# count plot

titanic %>%
  filter(!is.na(Age)) %>%
  ggplot(aes(x = Age,
             fill = Survived)) +
  geom_density(alpha = 0.2)
# distribution plot



# 6. Survival by Fare
titanic %>%
  filter(!Fare == 0) %>%
  ggplot(aes(x = Survived,
             y = Fare)) +
  geom_boxplot() +
  scale_y_continuous(trans = "log2") +
  geom_jitter(width = 0.1,
              alpha = 0.2)



# 7. Survival by Passenger Class 
titanic %>%
  ggplot(aes(x = Pclass,
         fill = Survived)) +
  geom_bar()
# default plot

titanic %>%
  ggplot(aes(x = Pclass,
             fill = Survived)) +
  geom_bar(position = position_fill())
# relative proportions of survival within each class

titanic %>%
  ggplot(aes(x = Survived,
             fill = Pclass)) +
  geom_bar(position = position_fill())
# relative proportions of class within survival status



# 8. Survival by Age, Sex, and Passenger Class
titanic %>%
  filter(!is.na(Age)) %>%
  group_by(Pclass) %>%
  ggplot(aes(x = Age,
             y = after_stat(count),
             fill = Survived)) +
  geom_density() +
  facet_grid(Sex ~ Pclass ~ .)

#####

  
  


               


  

    