install.packages("gganimate")
install.packages("plotly")
install.packages("png")
install.packages("paletteer")
library(tidyverse)
library(gganimate)
library(plotly)
library(gifski)
library(png)

#---------------------------------------------------------------------
# Lectura de datos
gapminder <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-04-24/gapminder_es.csv")
View(gapminder)
#---------------------------------------------------------------------
# Limpieza y procesamiento
#---------------------------------------------------------------------
#Filtro países de America
america <-gapminder %>% filter(continente== "Americas")
View(america)
america[america$pais == "Venezuela (Republica Bolivariana de)",1]<-"Venezuela"
america[america$pais == "Bolivia (Estado Plurinacional de)",1]<-"Bolivia"
head(america)
tail(america)
#---------------------------------------------------------------------
#Filtro países de America latina, es decir, de habla hispana
latam <- filter(america, pais %in% c("Argentina", "Bolivia" , "Brasil","Chile", "Colombia", "Costa Rica", "Cuba", "Ecuador", "El Salvador", "Guatemala", "Honduras", "México", "Nicaragua", "Panamá", "Paraguay", "Puerto Rico", "Perú", "República Dominicana", "Uruguay", "Venezuela"))
 View(latam)
 tail(latam)
#---------------------------------------------------------------------
#filtro paises de America del sur
americaDelSur <- filter(latam, pais %in% c("Argentina", "Bolivia" , "Brasil","Chile", "Colombia", "Ecuador",  "Paraguay", "Perú", "Uruguay", "Venezuela", "Guyana", "Surinam"))
 View(americaDelSur)
 #un gráfico simple para ver la distribución de los datos
 qplot(x=anio, y=poblacion, data=america, color=pais, main = "Población de America desde 1950 a 2007",
       xlab  = "Año", ylab = "Población")

# GRÁFICOS
#---------------------------------------------------------------------
# ggplot de América PUBLICADO en Rpubs
#---------------------------------------------------------------------
pamerica <-ggplot(data=america,aes(x=anio, y=poblacion, color=pais, text = paste('Año:',anio,'<br>Población:', as.double(poblacion), 'millones'))) +
  geom_point(show.legend = TRUE) +
  labs (title= "Evolución de la población de América", subtitle="Período: 1950- 2007", x = "Año", y = "Población", color="")+
  theme(axis.text.x = element_text(angle = 50, vjust = 1.5, hjust=1.4)) +  
  theme(plot.title = element_text(family="Verdana",
                                  size=rel(1),        
                                  vjust=2,            #Para separarlo del gráfico
                                  position_identity(center),   
                                  face="bold",       
                                  color="black",     #Color del título: maroon, lightblue, lightblue,darkblue, darkorange, black.
                                  lineheight=1.2), legend.position= "right", legend.justification = "center")
      
 pamerica
#---------------------------------------------------------------------
# plotly de America
#---------------------------------------------------------------------
ggplotly(pamerica, hoverformat='2.F', tooltip = "text")
#---------------------------------------------------------------------
#---------------------------------------------------------------------
# ggplot de Paises de Latinoamérica: correcto
#---------------------------------------------------------------------
platam <-ggplot(data=latam,aes(x=anio, y=poblacion, color=pais, text = paste('Población:', poblacion))) +
  geom_point(show.legend = TRUE) +
  labs (title= "Población de America desde 1950 a 2007", x = "Año", y = "Población", color="")+
  theme(axis.text.x = element_text(angle = 50, vjust = 1.5, hjust=1.4)) +  
  theme(plot.title = element_text(family="Courier",
                                  size=rel(1),        
                                  vjust=2,            #Para separarlo del gráfico
                                  position_identity(center),   
                                  face="bold",       
                                  color="black",     #Color del título: maroon, lightblue, lightblue,darkblue, darkorange, black.
                                  lineheight=1.2), legend.position= "right", legend.justification = "center")
  platam
#---------------------------------------------------------------------
# plotly de latam
#---------------------------------------------------------------------
ggplotly(platam, tooltip = "text")
#---------------------------------------------------------------------

  
#---------------------------------------------------------------------
# 1- ggplot de Paises de America del sur (con theme dark)
#---------------------------------------------------------------------
  
pdelsur1 <-ggplot(data=americaDelSur,aes(x=anio, y=poblacion, color=pais, text = paste('Población:', poblacion))) +
  geom_point(show.legend = TRUE) +
  labs (title= "América del Sur",subtitle = "Período: 1950 - 2007", x= "Año", y= "Población", color="")+
  theme(axis.text.x = element_text(angle = 50, vjust = 1.5, hjust=1.4)) +  
  theme_dark()          
  theme(plot.title = element_text(family="Courier",
                                  size=rel(1),        
                                  vjust=2,            #Para separarlo del gráfico
                                  position_identity(center),   
                                  face="bold",       
                                  color="black",     #Color del t?tulo: maroon, lightblue, lightblue,darkblue, darkorange, black.
                                  lineheight=1.2), legend.position= "right", legend.justification = "center")
pdelsur1

#---------------------------------------------------------------------
#plotly de america del sur
#---------------------------------------------------------------------
ggplotly(pdelsur1, tooltip = "text")
#---------------------------------------------------------------------


#---------------------------------------------------------------------
#  2- ggplot de Paises de America del sur (con facet_wrap + gganimate)
#---------------------------------------------------------------------
pdelsur3 <-ggplot(data=americaDelSur,aes(x=anio, y=poblacion, color=pais, text = paste('Población:', poblacion))) +
  geom_point(size=2.5, show.legend = TRUE) +
  labs (title= "América del Sur",subtitle = "Período: 1950 - 2007", x= "", y= "Población", color="") +
  theme_dark() +
  theme(axis.text.x =element_text(angle= 60, vjust = 1.5, hjust=1.4), plot.title = element_text(family="Courier",
                                  size=rel(1),        
                                  vjust=2,            #Para separarlo del gráfico
                                  position_identity(center),   
                                  face="bold",       
                                  color="black",     #Color del t?tulo: maroon, lightblue, lightblue,darkblue, darkorange, black.
                                  lineheight=1.2), legend.position= "right", legend.justification = "center" ) +
  facet_wrap(~pais)+
  transition_time(anio) +
  ease_aes('linear')+
  shadow_mark(alpha = 0.3, size = 2)
# grafica el ggplot de acuerdo a los parametros pasados anteriormente 
  pdelsur3 
  # ultimo gganimate: last_animate
  # para guardar: anim_save()
  
#---------------------------------------------------------------------
#plotly de Paises de America del sur 
#---------------------------------------------------------------------
pdelsur3 <-ggplot(data=americaDelSur,aes(x=anio, y=poblacion, color=pais, text = paste('Población:', poblacion))) +
    geom_point(size=2.5, show.legend = TRUE) +
    labs (title= "América del Sur",subtitle = "Período: 1950 - 2007", x= "", y= "Población", color="") +
    theme_dark() +
    theme(axis.text.x =element_text(angle= 60, vjust = 1.5, hjust=1.4), plot.title = element_text(family="Courier",
                                                                                                  size=rel(1),        
                                                                                                  vjust=2,            #Para separarlo del gráfico
                                                                                                  position_identity(center),   
                                                                                                  face="bold",       
                                                                                                  color="black",     #Color del t?tulo: maroon, lightblue, lightblue,darkblue, darkorange, black.
                                                                                                  lineheight=1.2), legend.position= "right", legend.justification = "center" ) +
    facet_wrap(~pais)
  
ggplotly(pdelsur3, tooltip = "text")


#---------------------------------------------------------------------
#  3- ggplot + gganimate de Paises de America del sur - GRÁFICO PUBLICADO
#---------------------------------------------------------------------
pdelsur3 <-ggplot(data=americaDelSur,aes(x=anio, y=poblacion, size=poblacion, color=pais, text = paste('Población:', poblacion))) +
  geom_point(size=2.9, show.legend = TRUE) +
  labs (title= "Evolución de la población en América del Sur",subtitle = 'Año: {round(frame_time,0)}', 
        x= "", y= "Población", color="",
        caption="#DatosDeMiercoles por Patricia Loto") +
  theme_dark() +
  theme(axis.text.x =element_text(angle= 60, vjust = 1.5, hjust=1.4), plot.title = element_text(family="Verdana",
                                                                                                size=rel(1.2),        
                                                                                                hjust=0.5,          
                                                                                                vjust=2,            
                                                                                                position_identity(center),   
                                                                                                face="bold",       
                                                                                                color="darkgrey",     
                                                                                                lineheight=0.5), legend.position= "right", legend.justification = "center",
  plot.subtitle = element_text(hjust = 0.5),
  plot.caption = element_text(color = "maroon", face = "bold")) +
  scale_size(range = c(2.5, 4.5))+
  facet_wrap(~pais)+
  transition_time(anio) +
  ease_aes('linear')+
  shadow_mark(alpha = 0.6, size = 2.5)

pdelsur3                                    
#last_animation()                  or
animate(pdelsur3, height= 550, weight= 550)
anim_save("gganimate_orange")


#  si quisiera el plotly de america del sur
ggplotly(pdelsur3, tooltip = "text")
#---------------------------------------------------------------------

#---------------------------------------------------------------------
# ejemplo del desarrollador del paquete  gganimate thomasp85
#---------------------------------------------------------------------
ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = country)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~continent) +
  # Here comes the gganimate specific bits
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(year) +
  ease_aes('linear')