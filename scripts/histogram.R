# load the packages ----------------------------------------------------------
library(tidyverse)
library(patchwork)


# data ----------------------------------------------------------------------- 

# generate some random data from a negative binomial distribution
set.seed(123)
x <- rnbinom(1000, 10, 0.5)

# create a tibble with the data
dat <- as_tibble(x) %>% 
 rename(x = value)

# find the range of the values
range(dat$x)


# plots --------------------------------------------------------------------------

# create an histogram (with the frequency polygon)
p1 <- ggplot(data=dat, aes(x=x)) +
  geom_histogram(binwidth = 3, fill = "blue", color = "red", alpha = 0.2, boundary=0) + 
  stat_bin(binwidth = 3, geom="text", aes(label=..count..), size = 3, vjust=-0.5, boundary=0) +
  #geom_freqpoly(binwidth = 3, size = 1, boundary=0) +
  expand_limits(x = 0, y = 0) +
  scale_x_continuous(breaks = seq(0, 30, 3)) +
  theme_minimal()

p1 


# create an equivalent density plot
p2 <- ggplot(data=dat, aes(x=x)) +
  geom_density(bw=3, fill = "blue", color = "red", alpha = 0.2) + 
  expand_limits(x = 0, y = 0) +
  scale_x_continuous(breaks = seq(0, 30, 3)) +
  theme_minimal()

p2 


# combine vertically the two plots
p1 / p2



# Exercise: try to change the binwith from the histogram and bw from density plot
