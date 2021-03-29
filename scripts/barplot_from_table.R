# load the packages --------------------------------------------------------------
library(finalfit)
library(rstatix)
library(tidyverse)


# create the table ---------------------------------------------------------------
dat <- c(7, 17, 2, 42)

tb <- matrix(dat, nrow = 2, dimnames = list(c("replacement", "synonymous"), c("fixed", "polymorphic ")))

tb



# create a data frame with raw data from the table frequencies -------------------

# create raw data from counts 
data_raw <- counts_to_cases(tb, count.col = "Freq") %>% 
  rename(synonymicity = Var1, fixity = Var2)
  

# inspect the table
table(data_raw$synonymicity, data_raw$fixity)


# obtain an informative table
data_raw <- data_raw %>%
  summary_factorlist(dependent = "fixity", add_dependent_label = T,
                     explanatory = "synonymicity", add_col_totals = T,
                     include_col_totals_percent = F,
                     column = FALSE, total_col = TRUE)
data_raw






# bar plot from table --------------------------------------------------------------

# using the initial 2x2 table `tb` create a long format table
tb1 <- as_tibble(tb) %>% 
  mutate(synonymicity = row_number(),
         synonymicity = factor(synonymicity, levels = c(1, 2), labels = c("replacement", "synonymous"))) %>% 
  pivot_longer(!synonymicity, names_to = "fixity", values_to = "Freq")

tb1


# next group the data by synonymicity and calculate the proportion 
tb2 <- tb1 %>% 
  group_by(synonymicity) %>% 
  mutate(perc = round(Freq/sum(Freq), 3)) 

tb2



# create the bar plot using the geometry `geom_col`
ggplot(tb2,
       aes(x = synonymicity, y = perc, 
           fill = fixity, cumulative = TRUE)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(perc*100,"%")), 
            position = position_stack(vjust = 0.5)) +
  scale_y_continuous(labels=scales::percent)

