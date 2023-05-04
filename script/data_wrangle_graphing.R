# Final project - making a graph and tables
# Jonathan Huang 2023-04-10

#load libraries
library(tidyverse)
library(here)

fish <- read_csv(here("data","songs_clean.csv")) 

songs_count <- fish %>% 
  group_by( year,total_length, species_code, reef_code) %>%  #group analysis 
  mutate(species_code = as_factor(factor(species_code,   #reorder the factors
                                         levels = c( "SEPU","PACL","PANE","EMJA","CHPU","OXCA"))),
         total_length = as.numeric(total_length), #change length to numeric
         reef_code = as_factor(reef_code),
         year = as.numeric(year)) %>%    
  summarise(count = sum(count), #sum the count through the years
            total_area_sampled = sum(total_area_sampled)) %>%    
  uncount(count) %>%                   #reorder so each row = 1 count
  select(year, species_code,reef_code,total_length,total_area_sampled) #reorder df order

#get summary of counts by site by year by species

songs_count_sum <- fish %>% 
  group_by( year,total_length, species_code, reef_code) %>%  #group analysis 
  mutate(species_code = as_factor(factor(species_code,   #reorder the factors
                                         levels = c( "SEPU","PACL","PANE","EMJA","CHPU","OXCA"))),
         total_length = as.numeric(total_length), #change length to numeric
         reef_code = as_factor(reef_code),
         year = as_factor(year)) %>%    
  summarise(count = sum(count) #sum the count through the years
) 

print(songs_count_sum)


#graphing multiple species on a panel to look over time
p1 <- ggplot(songs_count %>% filter(reef_code%in% "WNR") %>% 
               mutate(year = as_factor(year)),    #data 
             aes(x = total_length, color = year))+   #specify x & solor by year
  geom_density(linewidth = 1.5)+   #use density plot
  facet_wrap(~species_code,ncol=1, scales = "free")+    #separate plots by species
  ggh4x::facetted_pos_scales(x = NULL, y = list( #custom scales for each facet
    SEPU = scale_y_continuous(limits = c(0,0.075)),
    PACL = scale_y_continuous(limits = c(0,0.15)),
    PANE = scale_y_continuous(limits = c(0,0.24)),
    EMJA = scale_y_continuous(limits = c(0,0.15)),
    CHPU = scale_y_continuous(limits = c(0,0.9)),
    OXCA = scale_y_continuous(limits = c(0,1.0))
  ))+
  scale_x_continuous(breaks = seq(0,max(fish$total_length), 10),
                     limits = c(0,NA))+  #scale x to be max lenght at increment of 5cm
  labs(x = "Total Length (mm)",    #edit axis & legend labels
       y = "Density", 
       color = "Year",
       title = element_text("Size distribution on Artifical Reef (WNR)"))+
  scale_color_viridis_d()+  #color palatte
  theme_classic()+
  theme(panel.background = element_rect(fill = "grey90", color = "black"),
        plot.title = element_text(hjust = 0.5, size = 40),
        axis.text = element_text(size = 30),
        axis.title = element_text(size = 17),
        strip.text = element_text(size = 30),
        legend.text = element_text(size = 30),
        legend.key.size = unit(1.5,"cm"),
        legend.title = element_text(size = 50)) #remove lengend for ggarrange
p1
# ggsave(here("output", "size_densitydistribution_WNR.pdf"),
#        width = 20, height = 10)

p2 <- songs_count %>% filter(reef_code%in% c("BK","SMK")) %>% 
  mutate(year = as_factor(year)) %>% 
  ggplot()+   #specify x & solor by year
  geom_density(
    aes(x = total_length, color = year, group = year),linewidth = 1.5)+   #use density plot
  facet_wrap(~species_code, ncol=1, scales = "free")+ #separate plots by species
  ggh4x::facetted_pos_scales(x = NULL, y = list( #custom scales for each facet
    SEPU = scale_y_continuous(limits = c(0,0.075)),
    PACL = scale_y_continuous(limits = c(0,0.15)),
    PANE = scale_y_continuous(limits = c(0,0.24)),
    EMJA = scale_y_continuous(limits = c(0,0.15)),
    CHPU = scale_y_continuous(limits = c(0,0.9)),
    OXCA = scale_y_continuous(limits = c(0,1.0))
  ))+
  scale_x_continuous(breaks = seq(0,max(fish$total_length), 10),
                     limits = c(0,NA))+  #scale x to be max lenght
  labs(x = "Total Length (mm)",    #edit axis & legend labels
       y = "Density", 
       color = "Year",
       title = element_text("Size distribution on Natural Reefs (BK & SMK)"))+
  # scale_color_viridis_d()+  #color palatte
  scale_fill_manual( values = c("#440154","#481f70","#443983",
                                         "#3b528b","#31688e","#287c8e",
                                         "#21918c","#20a486", "#35b779",
                                         "#5ec962", "#90d743","#c8e020",
                                         "#fde725"),
                                         aesthetics = c("color", "fill"),
                     name = "Year")+
  theme_classic()+
  theme(panel.background = element_rect(fill = "grey90", color = "black"),
        plot.title = element_text(hjust = 0.5, size = 40),
        axis.text = element_text(size = 30),
        axis.title = element_text(size = 17),
        strip.text = element_text(size = 30),
        legend.text = element_text(size = 30),
        legend.key.size = unit(1.5,"cm"),
        legend.title = element_text(size = 50))

p2
# ggsave(here("output", "size_densitydistribution_BK_SMK.pdf"),
#        width = 20, height = 10)

# COMBINE THE PLOTS TO HAVE ONE PLOT
arrange <- ggarrange((p1+rremove("xlab")+rremove("ylab")+rremove("legend"))+(p2+rremove("ylab")+rremove("xlab")),
                     labels = NULL,
                     common.legend = TRUE,
                     legend = "right")
annotate_figure(arrange, 
                left = text_grob("Density", #add common axis
                                 rot = 90, 
                                 size = 45),
                bottom = text_grob("Total Length (mm)",
                                   size = 45))+
  theme(plot.margin = margin(1,1,1,1, "cm"))
