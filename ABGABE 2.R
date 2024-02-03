### Cometabgabe
### 3.1
N <- 10.                  #Anzahl der Personen
my_Geschenk <- 1          #Anzahl meiner Geschenke
all_Geschenke <- 1:10.    #Alle möglichen Geschenke 
iterations <- 1000      #Anzahl Simulationen 
counter <- 0             #Zählt positive Fälle



for (i in 1:iterations) {
  Geschenke <- sample(x = all_Geschenke, size = N, replace = TRUE)
  if (my_Geschenk %in% Geschenke ) {
    counter <- counter + 1
  }  
}

counter / iterations 
= 0.639 = 63.9%

### 3.2
wichtel_unglueck <- function(n, k, iterationen = 1000) {
  erfolge <- 0
  for (i in 1:iterationen) {
    geschenke <- sample(1:n)
    if (sum(geschenke == 1:n) >= k) {
      erfolge <- erfolge + 1
    }
  }
  wahrscheinlichkeit <- erfolge / iterationen
  return(wahrscheinlichkeit)
}
# Die berechnete Wahrscheinlichkeit, dass unter 10 Personen mindestens 2 ihre eigenen Geschenke in einem Wichtelspiel zurückerhalten, 
# beträgt etwa 25.1% basierend auf 1000 Simulationen.

= 0.251 = 25.1%

### 3.3
# Die Funktion wichtel_unglueck berechnet die Wahrscheinlichkeit, dass unter n Personen mindestens k ihre eigenen Geschenke zurückerhalten.
# Args:
#   n: Anzahl der Personen (und Geschenke)
#   k: Mindestanzahl von Personen, die ihr eigenes Geschenk zurückerhalten sollen
#   iterationen: Anzahl der Simulationen (Standardwert ist 1000)
# Returns:
#   Die Wahrscheinlichkeit, dass mindestens k Personen ihr eigenes Geschenk zurückerhalten
wichtel_unglueck <- function(n, k, iterationen = 1000) {
  # ... (Code der Funktion)
}

### 3.4
#laden erforderlicher Pakete
install.packages("testthat")
library(testthat)   

#testen der Funktion mit 4 Testfällen:
#1. n=1 und k=1 soll immer 1 als Ergebnis ausgeben
#2. wenn k größer als n ist soll ein Fehler ausgegeben werden
#3. wenn bei Iterationen keine ganze Zahl steht soll ein
#   Fehler ausgegeben werden
#4. wenn bei Iterationen keine Zahl sondern ein Wort steht soll ein
#   Fehler ausgegebn werden

test_that(            
  "Test der Funktion wichtel_unglueck",
  {expect_equal(wichtel_unglueck(n=1, k=1, 10000),1)
    
    expect_error(wichtel_unglueck(n=10, k=11, 10000))   
    
    expect_error(wichtel_unglueck(n=10, k=1, 10/3))
    
    expect_error(wichtel_unglueck(n=10, k=1, hundert))
  }
)

Die Wahrscheinlichkeit, dass mindestens 1 unter 1 Personen ihr eigenes Geschenk erhalten, beträgt etwa 0.0001
Die Wahrscheinlichkeit, dass mindestens 11 unter 10 Personen ihr eigenes Geschenk erhalten, beträgt etwa 0.0001
Die Wahrscheinlichkeit, dass mindestens 1 unter 10 Personen ihr eigenes Geschenk erhalten, beträgt etwa 0.3000

### 3.5
?read.csv
file = file.choose()
data <- read.csv(
  file = "/Users/alexanderenns/Downloads/Bikeshare2.csv",
  header = TRUE,
  sep = ",",
  dec = "."
)               # Datensatz eingelesen
class(data)

#Punkt 1:
subset(data,station == "11th & Kenyon St NW") # Datensatz auf unsere Station gefiltert
gefilterte_Daten <- subset(data,station == "11th & Kenyon St NW") # Umgenannt

#Punkt 2:
anyNA(gefilterte_Daten) # Prüfe ob es NA's gibt, entweder TRUE or FALSE
which(is.na(gefilterte_Daten)) # Gibt den Datensatz mit NA's an
complete.cases(gefilterte_Daten) # Überprüfen, ob die Datenzeile vollständig ist

# Mit range haben wir den Plausibilitätscheck durchgeführt
range(gefilterte_Daten$date)
range(gefilterte_Daten$station)
range(gefilterte_Daten$count)
range(gefilterte_Daten$wind_speed)
range(gefilterte_Daten$precipitation)
range(gefilterte_Daten$snowfall)
range(gefilterte_Daten$snow_depth)
range(gefilterte_Daten$mean_temperature)
range(gefilterte_Daten$max_temperature)


mean(gefilterte_Daten$count,na.rm = TRUE)    # Ausgerechneter Mittelwert ohne NA's
mean_count <- mean(gefilterte_Daten$count,na.rm = TRUE) # Diesen umbenannt auf mean_count
na <- which(is.na(gefilterte_Daten$count))  # NA's benannt
gefilterte_Daten[na,]       # Datensatz mit NA angezeigt
gefilterte_Daten[na,"count"] <- mean_count  # Ausgerechneter Mittelwert für NA eingesetzt

mean(gefilterte_Daten$mean_temperature,na.rm = TRUE) # Ausgerechneter Mittelwert ohne NA's
mean_temperature <- mean(gefilterte_Daten$mean_temperature,na.rm = TRUE) # Diesen umbeannt auf mean_temperature
na2 <- which(is.na(gefilterte_Daten$mean_temperature)) # NA's benannt
gefilterte_Daten[na2,]   # Datensatz mit NA angezeigt
gefilterte_Daten[na2,"mean_temperature"] <- mean_temperature # Ausgerechneter Mittelwert ohne NA's
gefilterte_Daten

anyNA(gefilterte_Daten)

#Punkt 3
gefilterte_Daten <- gefilterte_Daten[gefilterte_Daten$"wind_speed" > 0,] # Alle Werte von wind_speed größer 0 gesetzt

### 4.1
# Alle Pakete downloaden
install.packages("ggplot2")
library(ggplot2)

install.packages("dplyr")
library(dplyr)

install.packages("gapminder")
library(gapminder)
data(gapminder)
install.packages("plotly")
library(plotly)

# Temperatur
library(ggplot2)
temperature <- gefilterte_Daten$mean_temperature
Fahrraeder <- gefilterte_Daten$count
Zeit <- gefilterte_Daten$date
rainfall <- gefilterte_Daten$precipitation
windspeed <- gefilterte_Daten$wind_speed

count_meantemp <-ggplot(data = gefilterte_Daten, aes(x = temperature, y = Fahrraeder)) +
  geom_point(aes(color = temperature)) + 
  geom_smooth(method = "lm", se = FALSE)+
 labs(x="Temperatur", y="Anzahl ausgeliehener Fahrräder", titel="Zusammenhang Temperatur und Anzahl ausgeliehener Fahrräder")

# Niederschlagsmenge

  count_prec <- ggplot(data = gefilterte_Daten, aes(x = rainfall, y = Fahrraeder)) +
  geom_point(aes(color = rainfall)) +
  geom_smooth(method = 'lm', se = FALSE) +
  labs(x = "Niederschlagsmenge", y = "Anzahl ausgeliehener Fahrräder", title = "Zusammenhang zwischen Niederschlagsmenge und Anzahl ausgeliehener Fahrräder") +
  theme_minimal()

# Windgeschwindigkeit

count_wind <- ggplot(data = gefilterte_Daten, aes(x = windspeed, y = Fahrraeder)) +
  geom_point(aes(color = windspeed)) +
  geom_smooth(method = 'lm', se = FALSE) +
  labs(x = "Windgeschwindigkeit", y = "Anzahl ausgeliehener Fahrräder", title = "Zusammenhang zwischen Windgeschwindigkeit und Anzahl ausgeliehener Fahrräder") +
  theme_minimal()

# Zeit

library(ggplot2)
install.packages("dplyr")
library(dplyr)
library(gridExtra)

count_date <- ggplot(data=gefilterte_Daten) +
  geom_point(aes(x=date,
                 y=count),col="red") +
  xlab("Tag") +
  ylab("Fahrradausleihen pro Tag") +
  ggtitle("Fahrradausleihen pro Tag") +
  theme_minimal()

#Alles in einer Graphik erstellen
grid.arrange(count_meantemp, count_prec,
             count_wind, count_date,
             nrow=2, ncol=2)


# 4.2 

#Grafik stellt den Zusammenhang zwischen Fahrradausleihen pro Tag
#und der Durchschnittstemperatur (in °F), an Tagen an denen es nicht
#geregnet hat, dar
count_meantemp_norain <- ggplot(data=filter(gefilterte_Daten, 
                                            precipitation == 0)) + 
  geom_point(aes(x=mean_temperature,
                 y=count),col="red") +
  xlab("Durchschnittstemperatur (in °F)") +
  ylab("Anzahl ausgeliehener Fahrräder pro Tag") +
  ggtitle("Fahrradausleihen pro Tag vs. Durchschnittstemperatur (kein Regen)") +
  theme_minimal()

count_meantemp_rain <- ggplot(data=filter(gefilterte_Daten, percipation > 0)) + 
  geom_point(aes(x=mean_temperature,  y=count),col="blue") +
  xlab("Durchschnittstemperatur (in °F)") +
  ylab("Fahrradausleihen pro Tag") +
  ggtitle("Fahrradausleihen pro Tag vs. Durchschnittstemperatur (Regen)") +
  theme_minimal()

grid.arrange(count_meantemp_norain, count_meantemp_rain,
             ncol=2, nrow=1)
-------------------------------------------------------
#Unsers
subset(gefilterte_Daten,precipitation<"0.00")
kein_regen <- subset(datensatz,precipitation<"0.00")

subset(datensatz,precipitation>"0.00")
regen <- subset(datensatz,precipitation>"0.00")

temperatur <- kein_regen$mean_temperature
Fahrräder <- kein_regen$count

ggplot(data = kein_regen) + 
  geom_point(aes(x=temperatur,y=Fahrräder))


temperatur <- regen$mean_temperature
Fahrräder <- regen$count

ggplot(data = regen) + 
  geom_point(aes(x=temperatur,y=Fahrräder))


#GPT:
  ggplot(data = subset(data, rainfall), aes(x = Temperature, y = BikeCount)) +
  geom_point(aes(color = Temperature)) +
  geom_smooth(method = 'lm', se = FALSE) +
  labs(x = "Temperatur", y = "Anzahl ausgeliehener Fahrräder", title = "Regentage: Temperatur vs. Anzahl ausgeliehener Fahrräder") +
  theme_minimal()

# Plot für Tage, an denen es nicht geregnet hat
 ggplot(data = subset(gefilterte_Daten, !RainyDay), aes(x = Temperature, y = BikeCount)) +
  geom_point(aes(color = Temperature)) +
  geom_smooth(method = 'lm', se = FALSE) +
  labs(x = "Temperatur", y = "Anzahl ausgeliehener Fahrräder", title = "Tage ohne Regen: Temperatur vs. Anzahl ausgeliehener Fahrräder") +
  theme_minimal()

### 4.3

#zuerst wird die Verteilung der Fahrradausleihen
#pro Tag als Histogramm dargestellt
 verteilung_count <- ggplot(data=gefilterte_Daten) +
   geom_histogram(aes(x=count, y=after_stat(density)),
                  col="#de2d26",fill="#fc9272") +
   xlab("Fahrradausleihen pro Tag") +
   ylab("Dichte") +
   ggtitle("Verteilung der Fahrradausleihen pro Tag") +
   theme_minimal()
 
#danach die Verteilung der Durchschnittstemperaturen (in °F) als Histogramm
 verteilung_meantempature <- ggplot(data=gefilterte_Daten) +
   geom_histogram(aes(x=mean_temperature, y=after_stat(density)),
                  col="#756bb1", fill="#bcbddc") +
   xlab("Durchschnittstemperatur (in °F)") +
   ylab("Dichte") +
   ggtitle("Verteilung der Durchschnittstemperaturen") +
   theme_minimal()
  
#dann die Verteilung der Niederschlagsmenge (in inch) als Histogramm 
 verteilung_prec <- ggplot(data=gefilterte_Daten) +
   geom_histogram(aes(x=precipitation, y=after_stat(density)),
                  col="#636363",fill="#bdbdbd") +
   xlab("Niederschlagsmenge (in inch)") +
   ylab("Dichte") +
   ggtitle("Verteilung der Niederschlagsmengen") +
   theme_minimal()+
   coord_cartesian(xlim = c(0, 4))
 
#und dann die Verteilung der Windgeschwindigkeit (in mph) als Histogramm
 verteilung_wind <- ggplot(data=gefilterte_Daten) +
   geom_histogram(aes(x=wind_speed, y=after_stat(density)),
                  col="#ffffcc",fill="#fed976") +
   xlab("Windgeschwindigkeit (in mph)") +
   ylab("Dichte") +
   ggtitle("Verteilung der Durchschnittstemperaturen") +
   theme_minimal()
 
 #alle Grafen in einem Plot zusammen 
 grid.arrange(verteilung_count, verteilung_meantempature,
              verteilung_precipitation, verteilung_wind,
              nrow=2, ncol=2)         
 
### 4.4
# Erweiterung mit den 4 Jahreszeiten
 
 Jahreszeiten <- cut(gefilterte_Daten$date, breaks = as.Date(c("2022-01-01",
                                                               "2022-03-01",
                                                               "2022-06-01",
                                                               "2022-09-01",
                                                               "2022-12-01")),
                     labels = c("Winter", "Frühling", "Sommer", "Herbst"),
                     right = FALSE)
 
 ggplot(gefilterte_Daten, aes(x = count, fill = Jahreszeiten)) + 
   geom_density(alpha = 0.5) + 
   scale_fill_manual(values = c("Winter" = "green", "Frühling" = "blue", "Sommer" = "red", "Herbst" = "yellow")) + 
   labs(x = "Anzahl ausgeliehener Fahrräder", y = "Dichte") + 
   ggtitle("Verteilung der ausgeliehenen Fahrräder nach Jahreszeit")
 


### 4.5
 #laden erforderlicher Pakete
install.packages("rpotly") 
library(plotly)
 
 #Grafik stellt die Fahrradausleihen pro Tag der Windgeschwindigkeit (in mph) und 
 #der Durchschnittstemperatur (in °F) gegenüber
 count_wind_meantemp <- plot_ly(data=gefilterte_Daten,
                                x=~mean_temperature,
                                y=~wind_speed,
                                z=~count,
                                type="scatter3d",
                                mode="markers",
                                marker=list(size=5, opacity=0.5), color=~count) %>% layout(
                                  scene=list(xaxis=list(title="Durchschnittstemperatur (in °F)"),
                                             yaxis=list(title="Windgeschwindigkeit (in mph)"),
                                             zaxis=list(title="Anzahl ausgeliehener Fahrräder pro Tag")),
                                  title=
                                    "Fahrradausleihen pro Tag vs. Windgeschwindigkeit vs. Durchschnittstemperatur")


 plot_ly(data = gefilterte_Daten,
         x = ~mean_temperature,
         y = ~wind_speed,
         z = ~count,
         type = "scatter3d",
         mode = "markers",
         marker = list(size = 5, opacity = 0.5, color = ~count, colorscale = "YlOrRd")) %>%
   layout(
     scene = list(
       xaxis = list(title = "Durchschnittstemperatur (in °F)"),
       yaxis = list(title = "Windgeschwindigkeit (in mph)"),
       zaxis = list(title = "Anzahl ausgeliehener Fahrräder pro Tag")
     ),
     title = "Fahrradausleihen pro Tag vs. Windgeschwindigkeit vs. Durchschnittstemperatur"
   )