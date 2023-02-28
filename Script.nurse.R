save.image ("C:/Users/humay/Dropbox/Personal Drive/Own project/Nurses/Suicide among nurses/data.nurses.RData")

#Mapping:
install.packages("rjson")
library (rjson)

install.packages("cartography")
install.packages("sf")
install.packages("tidyverse")

library(cartography)
library(sf)
library(tidyverse)

library(maps)
library(ggplot2)

adm1<- read_sf("C:/Users/humay/Dropbox/Personal Drive/Own project/Nursing students/Main data/Map/Practice/BD/BGD_ADM1.shp")

adm1$NAME

data.nurses$Division.map= recode (data.nurses$Division_of_job_center,
                             "Dhaka"="Dhaka Division",
                             "Chattogram"= "Chittagong Division",
                             "Khulna"= "Khulna Division",
                             "Rajshahi" = "Rajshahi Division",
                             "Rangpur" = "Rangpur Division",
                             "Sylhet" = "Sylhet Division",
                             "Barishal" = "Barisal Division",
                             "Mymensingh" = "Mymensingh Division")


data.nurses$Division_of_job_center

data.nurses$NAME=data.nurses$Division.map 

table (data.nurses$Division.map)
table (data.nurses$NAME)


map.data.nurses<-merge(adm1, data.nurses, by="NAME")

map.data.nurses$NAME

map.data.nurses$NAME_new= recode (map.data.nurses$NAME,
                          "Dhaka Division"="Dhaka",
                          "Chittagong Division"="Chattagram",
                          "Khulna Division"="Khulna",
                          "Rajshahi Division"="Rajshahi",
                          "Rangpur Division"="Rangpur",
                          "Sylhet Division"="Sylhet",
                          "Barisal Division"= "Barishal",
                          "Mymensingh Division"="Mymensingh")

map.data.nurses$Division.suicide= map.data.nurses$NAME_new
table (map.data.nurses$Division.suicide)

map.data.nurses$Division.suicide_prev= recode (map.data.nurses$Division.suicide,
                                   "Dhaka"="9.87",
                                   "Chattagram"= "12.12",
                                   "Khulna"= "6.90",
                                   "Rajshahi" = "8.82",
                                   "Rangpur" = "6.45",
                                   "Sylhet" = "20.27",
                                   "Barishal" = "16.67",
                                   "Mymensingh" = "19.05")

map.data.nurses$Division.suicide_prev= as.integer(map.data.nurses$Division.suicide_prev)

map.data.nurses$Division.suicide_prev
table (map.data.nurses$Division.suicide_prev)



#Map1
map_suicide=ggplot(data=map.data.nurses)+
  geom_sf(aes(fill=Division.suicide_prev), color="snow")+
  scale_fill_viridis_c(option = "viridis", trans = "sqrt")+
  geom_sf_text (data=map.data.nurses,
                aes (label= NAME_new),
                color = "white",
                size=3)+
  xlab("")+ ylab("")+
  labs(title = "Suicidal ideation among nurses",  
       subtitle = "Across eight divisions of Bangladesh",  
       fill = "Prevalence") + 
  theme(plot.title =  
          element_text(hjust = 0.5)) +  
  theme(plot.subtitle =
          element_text(hjust = 0.5))+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank())

map_suicide

#map2
map_suicide=ggplot(data=map.data.nurses)+
  geom_sf(aes(fill=Division.suicide_prev), color="snow")+
  scale_fill_viridis_c(option = "viridis", trans = "sqrt")+
  geom_sf_text (data=map.data.nurses,
                aes (label= NAME_new),
                color = "#808000",
                size=4)+
  xlab("")+ ylab("")+
  labs(title = "Suicidal ideation among nurses",  
       subtitle = "Across eight divisions of Bangladesh",  
       fill = "Prevalence") + 
  theme(plot.title =  
          element_text(hjust = 0.5)) +  
  theme(plot.subtitle =
          element_text(hjust = 0.5))+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank())

map_suicide

#map3
map_suicide=ggplot(data=map.data.nurses)+
  geom_sf(aes(fill=Division.suicide_prev), color="black",alpha=0.05)+
  scale_fill_viridis_c(option = "viridis", trans = "sqrt")+
  geom_sf_text (data=map.data.nurses,
                aes (label= NAME_new),
                color = "white",
                size=3.5,
                fontface = "bold")+
  xlab("")+ ylab("")+
  labs( 
       fill = "Prevalence") + 
  theme(plot.title =  
          element_text(hjust = 0.5)) +  
  theme(plot.subtitle =
          element_text(hjust = 0.5))+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank())

map_suicide

