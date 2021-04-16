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


# plots ------------------------------------------------------------------------------

# create an histogram (with the frequency polygon)
p1 <- ggplot(data=dat, aes(x=x)) +
  geom_histogram(binwidth = 3, fill = "blue", color = "red", alpha = 0.2, boundary=0) + 
  stat_bin(binwidth = 3, geom="text", aes(label=..count..), size = 3, vjust=-0.5, boundary=0) +
  #geom_freqpoly(binwidth = 3, size = 1, boundary=0) +
  expand_limits(x = 0, y = 0) +
  scale_x_continuous(breaks = seq(0, 30, 3)) +
  scale_y_continuous(breaks = seq(0, 300, 20)) +
  theme_minimal()

p1 


# create the histogram with a density scale and add a density curve
p2 <- ggplot(data=dat, aes(x=x)) +
  geom_histogram(aes(x = x, y = ..density..),
                 binwidth = 3, fill = "grey", alpha = 0.2, color = "black") +
  geom_density(bw=3, kernel = "gaussian", fill = "blue", color = "red", alpha = 0.2) + 
  expand_limits(x = 0, y = 0) +
  scale_x_continuous(breaks = seq(0, 30, 3)) +
  scale_y_continuous(breaks = seq(0, 0.1, 0.005)) +
  theme_minimal() +
  geom_rug(aes(x = x, y = 0), sides = "b", color = "green", position = position_jitter(height = 0))

p2 


# combine vertically the two plots
p1 / p2




# compute kernel density estimate -----------------------------------------------------
library(kdensity)


# set a kernel density function using the gaussian kernel and bandwidth = 3
kde <- kdensity(dat$x, bw = 3, kernel = "gaussian")

# for example for x = 12 the kernel density is:
kde(12)

# integrate the kernel density function for the range of x values
integrate(kde, lower = 0, upper = 30)



# NOTE: Gaussian kernel
# For each data point we generate a new value that is some function of the original value 
#at that point and the surrounding data points. With Gaussian smoothing, 
#the function that is used is the Gaussian curve


# Exercise: Try to change the binwith from the histogram and bw from density plot
