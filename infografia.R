library(tidyverse)
library(nycflights13)
view(flights)

glimpse(flights) #reviso tipo de datos

#Ver si hay datos repetidos
flights %>%
  summarise(distintos=n_distinct(flight), total=n())

unique(flights$year) #----> 2013
unique(flights$month) # ----> 12 meses
unique(flights$day) #------> 31 dias
unique(flights$dep_delay)
#unique(flights$dep_time)
#unique(flights$sched_dep_time)
#unique(flights$arr_time)
unique(flights$carrier)
#unique(flights$flight)
#unique(flights$tailnum)
unique(flights$origin)
unique(flights$dest)
#unique(flights$air_time)
#unique(flights$distance)

flights <- flights %>%
  mutate(dep_delay = as.numeric(dep_delay))

glimpse(flights)


flights %>% summarise(vuelos = n(),
                       aerolinea = sum(is.na(carrier)),
                       destino = sum(is.na(dest)),
                      origen= sum(is.na(dest)),
                      hora_salida= sum(is.na(sched_dep_time)),
                      retraso = sum(is.na(dep_delay)),
                      despegue= sum(is.na(dep_time))
)

#Hay muchos NA en tiempo de retraso, analizar los retrasos en vuelos a Florida. 
#Evaluar por aerolinea/destino los retrasos... ver guia6 para separar 

aerop_playas= c('MIA' , 'TPA', 'SRQ', 'PBI', 'MCO', 'JAX', 'RSW', 'FLL', 'EYW', 'HNL', 'SAN', 'ILM')

vuelos_a_playas <- flights %>%    #dataset con vuelos a florida y hawaii
  filter(dest %in% aerop_playas)


vuelos_a_playas%>%
  count()


vuelos_a_playas%>% summarise(vuelos = n(),
                      aerolinea = sum(is.na(carrier)),
                      destino = sum(is.na(dest)),
                      origen= sum(is.na(dest)),
                      hora_salida= sum(is.na(sched_dep_time)),
                      retraso = sum(is.na(dep_delay)),      #el dataset tiene 65 NA es retrasos
                      despegue= sum(is.na(dep_time)))

#flights %>%  #evalúo si vale la pena agregar vuelos a hawaii, como la cantidad de vuelos son 707 no los incluyo en el dataset
 # filter(dest == 'HNL')%>%
  #count()


vuelos_a_playas%>%
  group_by(origin)%>%
  summarise(n())

proporcion <- flights%>%
  group_by(origin)%>%
  summarise(vuelos_totales= n(), a_playas= sum(dest %in% aerop_playas) )

proporcion

flights <-  flights%>%
  mutate(Playas = if_else(dest %in% aerop_playas, "si", "no"))
    
view(flights)


ggplot(flights)+
  geom_bar( aes(x=origin, fill= Playas), position="fill")+
xlab("Aeropuerto")+
  scale_x_discrete(labels=c('Newark', 'J.F. Kennedy', 'La Guardia'))+
  scale_fill_manual(name='Destino', labels=c('Otro', 'Playa'), values = c("grey","orange"))+
  ylab("Proporción[Vuelos]")+
  labs(title= "Vuelos a destinos playeros desde Nueva York" ,
       subtitle="Comparación entre aeropuertos",
       caption= "Fig. 1")+
  theme(axis.title = element_text(size=12),
        axis.text = element_text(size=10),
        title = element_text(size=14),
        legend.title = element_text(size=12)) 

ggplot(flights)+
  geom_bar( aes(x=origin, fill= Playas))


#########################################

# Se filtro el dataset solo para los vuelos que salen de JFK y LGA hacia destinos playeros

flights_2 <- flights%>%
  filter(origin %in% c('JFK', 'LGA') & dest %in% aerop_playas)
view(flights_2) 

flights_2%>%
  summarise(vuelos = n(),
          aerolinea = sum(is.na(carrier)),
          destino = sum(is.na(dest)),
          origen= sum(is.na(dest)),
          hora_salida= sum(is.na(sched_dep_time)),
          retraso = sum(is.na(dep_delay)),      #el dataset tiene 373 NA en  retrasos
          despegue= sum(is.na(dep_time)))

#Metricas de resumen sobre retrasos de cada aeropuerto

retrasos_resumen<-flights_2 %>%
  group_by(origin)%>%
  summarise(name = c('Min','Quantil 0,05', '1er Quartil', 'Mediana', '3er Quartil','Quantil 0,95', 'Max'),
            value = c(min(dep_delay, na.rm=T),
                      quantile(dep_delay, probs = 0.05, na.rm=T),
                      quantile(dep_delay, probs = 0.25, na.rm=T),
                      median(dep_delay, na.rm=T),
                      quantile(dep_delay, probs = 0.75, na.rm=T), 
                      quantile(dep_delay, probs = 0.95, na.rm=T),
                      max(dep_delay, na.rm=T)))%>%
  pivot_wider( names_from=name, values_from=value)

retrasos_resumen

ggplot(flights_2)+
  geom_boxplot(aes(x=origin, y=dep_delay), colour="black", fill=c("blue","lightblue"))+
  coord_cartesian(ylim=c(-5,30))+
  xlab("Aeropuerto")+
  scale_x_discrete(labels=c('J.F. Kennedy', 'La Guardia'))+
  ylab("Demora[Minutos]")+
  labs(title= "Demoras de vuelos a destinos playeros" ,
       subtitle="Comparación entre aeropuertos",
       caption= "Fig. 2 - Gráfico con zoom para mostrar los datos de mayor importancia para éste análisis")+
  theme(axis.title = element_text(size=12),
        axis.text = element_text(size=10),
        title = element_text(size=14),
        legend.title = element_text(size=12)) 


sin_outliers <- flights_2%>%
  filter(dep_delay >= -10 & dep_delay<= 83 )


ggplot(sin_outliers)+
  geom_density(aes(x= dep_delay, fill= origin),alpha= 0.6, position="identity")+
  ylab("Densidad")+
  scale_fill_manual(name= "Aeropuerto", labels=c('J.F. Kennedy', 'La Guardia'), values = c("blue", "lightblue"))+
  xlab("Retraso[Minutos]")+
  labs(title= "Densidad de demoras de vuelos a destinos playeros" ,
       subtitle="Comparación entre aeropuertos")+
  theme(axis.title = element_text(size=12),
        axis.text = element_text(size=10),
        title = element_text(size=14),
        legend.title = element_text(size=12)) 


ggplot(sin_outliers)+
  geom_density(aes(x= dep_delay, fill= origin),alpha= 0.6, position="identity")+
  ylab("Densidad")+
  scale_fill_manual(name= "Aeropuerto", labels=c('J.F. Kennedy', 'La Guardia'), values = c("blue", "lightblue"))+
  xlab("Demora[Minutos]")+
  labs(title= "Densidad de demoras de vuelos a destinos playeros" ,
       subtitle="Comparación entre aeropuertos",
       caption = "Fig. 3 - Gráfico con zoom para mostrar los datos de mayor importancia para éste análisis.")+
  theme(axis.title = element_text(size=12),
        axis.text = element_text(size=10),
        title = element_text(size=14))+
  coord_cartesian (xlim = c(0, 40))



aerop_jfk <- flights_2%>%
  filter(origin == "JFK")


ggplot(aerop_jfk)+
  geom_histogram(aes(x=sched_dep_time), position="identity",  colour= "blue",fill= "orange", bins=18)+
  geom_freqpoly(aes(x=sched_dep_time) ,binwidth = 100, colour ="red", linewidth= 0.8)+
  ylab(NULL)+
  scale_y_discrete(NULL)+
  scale_x_continuous(
    breaks = c(500, 1000, 1500, 2000),
    labels =c("5", "10", "15", "20")
    )+
  xlab("Horario de vuelos[Hora]")+
  labs(title= "Frecuencia de horarios de vuelos hacia destinos con playas" ,
       subtitle="Aeropuerto J. F. Kennedy",
       caption= "Fig. 4")+
  theme(axis.title = element_text(size=12),
        axis.text = element_text(size=10),
        title = element_text(size=14))
  
  
  #facet_grid(cols = vars(origin))
  #facet_grid(rows = vars(origin))
  #facet_grid(vars(origin), vars(dest))

horario_salida <- unique(flights_2$sched_dep_time)
min(horario_salida)
max(horario_salida)



#Agrego columna con rangos horarios
aerop_jfk <- aerop_jfk%>%
  mutate(rango_horario = case_when(
    sched_dep_time > 500 & sched_dep_time <= 700 ~ 1,
    sched_dep_time > 700 & sched_dep_time <= 900 ~ 2,
    sched_dep_time > 900 & sched_dep_time <= 1100 ~ 3,
    sched_dep_time > 1100 & sched_dep_time <= 1300 ~ 4,
    sched_dep_time > 1300 & sched_dep_time <= 1500 ~ 5,
    sched_dep_time > 1500 & sched_dep_time <= 1700 ~ 6,
    sched_dep_time > 1700 & sched_dep_time <= 1900 ~ 7,
    sched_dep_time > 1900 & sched_dep_time <= 2100 ~ 8,
    sched_dep_time > 2100 ~ 9,
    
  ))
view(flights_2)  

sum(is.na(flights_2$rango_horario)) #No quedaron valores NA


aerop_jfk <- aerop_jfk%>%
  mutate(rango_horario = case_when(
    sched_dep_time >= 500 & sched_dep_time < 800 ~ "5 - 8",
    sched_dep_time >= 800 & sched_dep_time < 1200 ~ "8 - 12",
    sched_dep_time >= 1200 & sched_dep_time < 1600 ~ "12 - 16",
    sched_dep_time >= 1600 & sched_dep_time < 2000 ~ "16 - 20",
    sched_dep_time >= 2000 & sched_dep_time <2300 ~ "20 - 23",
    
  ))



ggplot(aerop_jfk)+
  geom_freqpoly(aes(x=rango_horario),alpha=0.5, position= "identity")+
  scale_x_discrete(limits= c("5 - 8","8 - 12", "12 - 16", "16 - 20", "20 - 23"))

ggplot(aerop_jfk)+
  geom_freqpoly(aes(x=rango_horario),alpha=0.5, position= "identity" )
