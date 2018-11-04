library(geostatsp)
library(ggplot2)
data("swissRain")
d<-data.frame(x=swissRain$x-2514337,y=swissRain$y-1094673,z=swissRain$rain)
ggplot(data=d)+geom_point(aes(x=x,y=y,color=z))

d_list =as.list(d)
