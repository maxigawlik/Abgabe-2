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
= 0.659 = 65.9%

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
wichtel_unglueck(10,2)
# Die berechnete Wahrscheinlichkeit, dass unter 10 Personen mindestens 2 
# ihre eigenen Geschenke in einem Wichtelspiel zurückerhalten, 
# beträgt etwa 25.1% basierend auf 1000 Simulationen.

= 0.256 = 25.6%

### 3.3
# Die Funktion wichtel_unglueck berechnet die Wahrscheinlichkeit,
# dass unter n Personen mindestens k ihre eigenen Geschenke zurückerhalten.
# Variablen:
#   n: Anzahl der Personen (und Geschenke)
#   k: Mindestanzahl von Personen, die ihr eigenes Geschenk zurückerhalten sollen
#   iterationen: Anzahl der Simulationen (Standardwert ist 1000)
# Returns:
#   Die Wahrscheinlichkeit, dass mindestens k Personen ihr eigenes Geschenk zurückerhalten


### 3.4
install.packages("testthat")
library(testthat)    # alle Packages installieren

wichtel_unglueck <- function(n, k, iterationen = 1000) {
  if (!is.numeric(iterationen) || iterationen <= 0 || (iterationen %% 1) != 0) {
    stop("iterationen muss eine positive ganze Zahl sein")
  }
  
  treffer <- 0
  for (i in 1:iterationen) {
    geschenke <- sample(n)
    if (sum(geschenke == 1:n) >= k) {
      treffer <- treffer + 1
    }
  }
  wahrscheinlichkeit <- treffer / iterationen
  return(wahrscheinlichkeit)   
}
# das ist die Funktion bei der nun n, k und die Iterationen ausgewählt werden können

Test 1: Wenn n = 1 und k = 1, sollte die Wahrscheinlichkeit immer 1 sein.
test_that("Test 1", {
  expect_equal(wichtel_unglueck(1, 1), 1)
}) # Test passed

Test 2: Wenn iterationen eine ungültige Eingabe ist, sollte die Funktion einen Fehler zurückgeben.
test_that("Test 2", {
  expect_error(wichtel_unglueck(3, 1, iterationen = "ganz, ganz viele"))
}) # Test passed

### 3.5
?read.csv
data <- read.csv(
  file = file.choose(),
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
install.packages("plotly")
library(plotly)

# Temperatur
library(ggplot2)
temperature <- gefilterte_Daten$mean_temperature
Fahrraeder <- gefilterte_Daten$count
Zeit <- gefilterte_Daten$date
rainfall <- gefilterte_Daten$precipitation
windspeed <- gefilterte_Daten$wind_speed  
# hier haben wir die einzelnen Daten bennanten damit wir mit ihnen weiterarbeiten können

ggplot(data = gefilterte_Daten, aes(x = temperature, y = Fahrraeder)) +
  geom_point(aes(color = temperature)) + 
  geom_smooth(method = "lm", se = FALSE)+
  labs(x="Temperatur", y="Anzahl ausgeliehener Fahrräder", titel="Zusammenhang Temperatur und Anzahl ausgeliehener Fahrräder")
# goem_point, da wir uns alle punkte in den gegebenen Daten anschauen

count_meantemp <-ggplot(data = gefilterte_Daten, aes(x = temperature, y = Fahrraeder)) +
  geom_point(aes(color = temperature)) + 
  geom_smooth(method = "lm", se = FALSE)+
 labs(x="Temperatur", y="Anzahl ausgeliehener Fahrräder", titel="Zusammenhang Temperatur und Anzahl ausgeliehener Fahrräder")
# hier haben wir den Plot noch benannt damit wir damit in der nächsten Aufgabe arbeiten können

# Niederschlagsmenge
ggplot(data = gefilterte_Daten, aes(x = rainfall, y = Fahrraeder)) +
  geom_point(aes(color = rainfall)) +
  geom_smooth(method = 'lm', se = FALSE) +
  labs(x = "Niederschlagsmenge", y = "Anzahl ausgeliehener Fahrräder", title = "Zusammenhang zwischen Niederschlagsmenge und Anzahl ausgeliehener Fahrräder") +
  theme_minimal()
# die Befehle wiederholen sich hier nur mit anderen Werten, welche verwendet werden

count_perc <- ggplot(data = gefilterte_Daten, aes(x = rainfall, y = Fahrraeder)) +
  geom_point(aes(color = rainfall)) +
  geom_smooth(method = 'lm', se = FALSE) +
  labs(x = "Niederschlagsmenge", y = "Anzahl ausgeliehener Fahrräder", title = "Zusammenhang zwischen Niederschlagsmenge und Anzahl ausgeliehener Fahrräder") +
  theme_minimal()
# Plot wird wieder benannt 

# Windgeschwindigkeit
ggplot(data = gefilterte_Daten, aes(x = windspeed, y = Fahrraeder)) +
  geom_point(aes(color = windspeed)) +
  geom_smooth(method = 'lm', se = FALSE) +
  labs(x = "Windgeschwindigkeit", y = "Anzahl ausgeliehener Fahrräder", title = "Zusammenhang zwischen Windgeschwindigkeit und Anzahl ausgeliehener Fahrräder") +
  theme_minimal()

count_wind <- ggplot(data = gefilterte_Daten, aes(x = windspeed, y = Fahrraeder)) +
  geom_point(aes(color = windspeed)) +
  geom_smooth(method = 'lm', se = FALSE) +
  labs(x = "Windgeschwindigkeit", y = "Anzahl ausgeliehener Fahrräder", title = "Zusammenhang zwischen Windgeschwindigkeit und Anzahl ausgeliehener Fahrräder") +
  theme_minimal()

# Zeit
install.packages("dplyr")
install.packages("ggplot2")
install.packages("lubridate")
library(dplyr)
library(gridExtra)
library(scales)
library(lubridate)     # erst wurden alle packages installiert
library(ggplot2)
class(gefilterte_Daten$date)
gefilterte_Daten$date <- as.Date(gefilterte_Daten$date) # Umbennenung der Daten

ggplot(data=gefilterte_Daten) +
  geom_point(aes(x=date,
                 y=count),col="red") +
  xlab("verschiedene Tage") +
  ylab("Fahrradausleihen pro Tag") +
  ggtitle("Fahrradausleihen pro Tag") +
  scale_x_date(date_labels = "%b %d, %Y", date_breaks = "1 month") +
  theme_minimal()             # geom_point Plot für die verschiedenen Daten

count_date <- ggplot(data=gefilterte_Daten) +
  geom_point(aes(x=date,
                 y=count),col="red") +
  xlab("verschiedene Tage") +
  ylab("Fahrradausleihen pro Tag") +
  ggtitle("Fahrradausleihen pro Tag") +
  scale_x_date(date_labels = "%b %d, %Y", date_breaks = "1 month") +
  theme_minimal() 

#Alles in einer Graphik erstellen
grid.arrange(count_date,count_meantemp,count_perc,count_wind,
             nrow=2, ncol=2)


# 4.2 
#zuerst wurde der Graf mit ausschließlich den Tagen ohne Regen erstellt 
keinregen <- ggplot(data=filter(gefilterte_Daten, 
                                            precipitation == 0)) + 
  geom_point(aes(x=mean_temperature,
                 y=count),col="red") +
  xlab("Durchschnittstemperatur (in °F)") +
  ylab("Anzahl ausgeliehener Fahrräder pro Tag") +
  ggtitle("Fahrradausleihen pro Tag vs. Durchschnittstemperatur (kein Regen)") +
  scale_y_continuous(limits = c(0, max(gefilterte_Daten$count) + 50)) +
  theme_minimal()

#danach der Graf mit Regentagen
regen <- ggplot(data=filter(gefilterte_Daten, precipitation > 0)) + 
  geom_point(aes(x=mean_temperature,  y=count),col="blue") +
  xlab("Durchschnittstemperatur (in °F)") +
  ylab("Fahrradausleihen pro Tag") +
  ggtitle("Fahrradausleihen pro Tag vs. Durchschnittstemperatur (Regen)") +
  scale_y_continuous(limits = c(0, max(gefilterte_Daten$count) + 50)) +
  theme_minimal()

grid.arrange(keinregen, regen,
             ncol=2, nrow=1)


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
              verteilung_prec, verteilung_wind,
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