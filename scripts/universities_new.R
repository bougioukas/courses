# top 15 universities appeared in author's affiliation list of OoSRs

# load the packages --------------------------------------------------------------------------
library(here)
library(tidyverse)
library(ggpubr)
library(ggimage)
library(ggthemes)


# load the data ------------------------------------------------------------------------------
library(readxl)
institutes <- read_excel(here("data", "universities.xlsx"))



# set new color palette -------------------------------------------------------------
library(paletteer)

scale_tableu <- function(){
  paletteer::scale_colour_paletteer_d("ggthemes::Classic_10_Medium", direction = 1)
  
}

options(ggplot2.discrete.colour = scale_tableu)





# data frames with images --------------------------------------------------------------------

###  universities as a vector
Institute <- institutes$Institute

# images from logos file as a vector
image_path <- here("logos", c("kings.png", "oxford.png",
                       "imperial.jpg", "toronto.jpg",
                       "alberta.png", "exeter.jpg",
                       "ottawa.png", "monash.png",
                       "manchester.jpg", "ioannina.png",
                       "sydney.png", "melbourne.jpg",
                       "mcmaster.png", "stanford.png",
                       "padova.jpeg"))
                       
# create a tibble with Institute and image path in x-position                      
images <- tibble(x = -3.5, Institute,  image_path)



# plot universities ---------------------------------------------------------------------------

fig8new <- ggplot(institutes , aes(x = Freq, y = reorder(Institute, Freq))) + 
  geom_point(aes(color = Country), size=13) + 
  geom_segment(aes(x=0,xend=Freq, y=Institute, yend=Institute), color = "grey10", size=3.3) + 
  geom_point(color = "grey10", size=5) + 
  geom_text(aes(label = Freq), size= 7, vjust = -1.3, hjust = 0.55) + 
  geom_text(aes(x = 1.5, label = Institute), size= 6.0, vjust = -0.9, hjust = 0.01) + 
  labs(x="Number of publications", y="University", color = "Country", size=17) +
  labs_pubr() +
  theme_pubclean(base_size = 20) +
  theme(panel.grid.major.x = element_line(linetype = "dotted", size = 1, color = "grey"),
        panel.grid.major.y = element_blank(),
        axis.title.x = element_text(hjust=1, face = "bold", margin = margin(t = 10, r = 0, b = 10, l = 0), size = 24),
        axis.title.y = element_text(face = "bold", margin = margin(t = 0, r = 20, b = 0, l = 0), size = 24),
        axis.text.x = element_text(color = "#464a62"),
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(),
        legend.title =  element_text(size = 18),
        legend.text =  element_text(size = 15),
        legend.key = element_rect(size = 16, colour = NA, fill = NA),
        legend.background=element_rect(fill='transparent'),
        legend.position="right") +
  scale_x_continuous(limits=c(-3.5, 70),breaks = seq(0, 70, 5)) +
  geom_image(data = images, aes(x = x, y = Institute, image = image_path), size = 0.055) 

fig8new


ggsave(here::here("figures", paste0("fig8new", format(Sys.time(), "%Y%m%d_%H%M%S"), ".tiff")), 
       type = "cairo", width = 15, height = 14, dpi = 320, compression = "lzw")
