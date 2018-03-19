# Source this file to load functions

library(tidyverse)
library(rtweet)
library(hrbrthemes)
library(viridis)

get_density <- function(x, y, n = 100) {
  dens <- MASS::kde2d(x = x, y = y, n = n)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}

follower_data<-function(screen_name){
  follower_ids<-get_followers(screen_name, n = 90000)
  follower_data<-lookup_users(follower_ids)
  follower_data_flip<-follower_data[seq(dim(follower_data)[1],1),]
  follower_data_flip$follower_number <- seq.int(nrow(follower_data_flip))
  follower_data_flip$density <- get_density(x=follower_data_flip$follower_number, y=as.numeric(follower_data_flip$account_created_at))
  return(follower_data_flip)
}

follower_plot<-function(df, title){
  df%>%
  ggplot(aes(follower_number, account_created_at, color = density))+
  geom_point(size=.1) +
  scale_y_datetime(date_breaks = "1 year", date_labels = "%Y") + 
  scale_color_viridis(option = "B", end = .9)+
  theme_ipsum("sans", , grid = F)+
  theme(legend.position = "none")+
  labs(title=title, x = "Follower Number", y = "Account Creation Date")
}

follower_plot_auto<-function(screen_name){
  df<-follower_data(screen_name)
  follower_plot(df, title = paste0("@",screen_name," follower plot"))
}
