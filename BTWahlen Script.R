##Let's get started!

# Setup----
Sys.info()

sessionInfo()

getwd()


## first and foremost
Sys.setenv(language="en")

## clean old workspace
rm(list = ls())

## load packages----
pac <- "pacman" %in% rownames(installed.packages())
if(!pac) install.packages("pacman")
library(pacman)
rm(pac)
p_load("scales", "styler", "haven", "dplyr", "ggplot2", "rgdal", "gganimate", "gifski", "png",
       "here", "stringr", "qdapRegex", "readr", "tidyr", "leaflet", "leaflet.extras",
       "htmlwidgets", "widgetframe", "plotly", "htmltools", "viridis", "hrbrthemes", "extrafont", 
       "data.table", "tidyverse", "forcats", "crosstalk")

wd <- getwd()

## load data----

file.choose()


# 

BTGesamt <- fread("C:\\Fun Fun Fun\\Projekte\\Wahlen\\BT Wahlen\\Bt Wahlen 49-21.csv", 
                  skip = 0, header = TRUE, fill = TRUE, stringsAsFactors = FALSE)

# 
colnames(BTGesamt)[c(5,6)] <- c("Grüne", "Bündnis90/ Die Grünen")

# Drop Sonstige Kleinstparteien

BTGesamt <- BTGesamt %>% 
  select(-Sonstige)

# Wide to Long Format

BTGesamt <- melt(BTGesamt, id.vars = "Jahr")

#

colnames(BTGesamt)[c(1:3)] <- c("Wahljahr", "Partei", "Prozent")

#

BTGesamt$Prozent <- as.numeric(gsub(",", ".", BTGesamt$Prozent))



## Gesamtdaten----

str(BTGesamt) 


#
## Visualisierung Gesamt----
#

#Step 1 - Custom Theme

theme_BtWahl <-  function() {
  theme_minimal() +
    theme(
      text = element_text(family = "Bookman", color = "gray25"), 
      plot.title = element_text(size = 12, hjust = 0.5),
      plot.subtitle = element_text(size = 10, hjust = 0.5), 
      plot.caption = element_text(color = "gray30", hjust = 0),
      plot.background = element_rect(fill = "gray91"),
      plot.margin = unit(c(2.5, 10, 2.5, 10), units = "mm"), 
      panel.background = element_rect(fill = "gray89"), 
      legend.text = element_text(size = 10, hjust = 1), 
      legend.title = element_text(hjust = 0.5), 
      legend.box.spacing = unit(c(5,5), units = "mm")
    )
}


#
## Visualisierung Gesamt----
#

colorPartei <- c("CDU/CSU" = "#000000", 
                 "SPD" = "#CC0033", 
                 "FDP" = "#FFFF33", 
                 "Grüne" = "#336600", 
                 "Bündnis90/ Die Grünen" = "#66FF33", 
                 "Die Linke. PDS" = "#FF3300", 
                 "AfD" = "#66CCFF")

#

BTGesamtPlot <- ggplot(BTGesamt, aes(x = Wahljahr, y = Prozent, color = Partei)) +
  geom_point(size = 0.75, alpha = 0.5) +
  geom_line(size = 0.5, alpha = 0.5) +
  scale_colour_manual(values = colorPartei) +
  geom_hline(yintercept = 5, linetype = "dashed", color = "#FF3300") +
  theme_BtWahl() +
  labs(x = "Wahljahr", 
       y = "Prozent der Zweitstimmen", 
       title = "Zweitstimmenanteile der Bundestagswahlen je Partei",
       subtitle = "1949 - 2021, exklusive sonstiger Kleinstparteien", 
       caption = "Datenquelle: Deutscher Bundestag, 2021;
eigene Darstellung") +
  coord_cartesian(xlim = c(1949, 2021))



BTGesamtPlot

# Plotly - Interactiv-----

# static and interactive

ggplotly(BTGesamtPlot)

# animation - accumulated graph all Parties

BTGesamt <- BTGesamt %>% 
  arrange(Wahljahr)

jahre <- unique(BTGesamt$Wahljahr)

# remove NAs from df

BTGesamt[is.na(BTGesamt)] <- 0

#

Parteien <- BTGesamt %>% 
  arrange(Wahljahr) %>% 
  split(.$Wahljahr) %>% 
  accumulate(~bind_rows(.x, .y)) %>% 
  set_names(jahre) %>% 
  bind_rows(.id = "frame") %>% 
  plot_ly(x = ~Wahljahr, y = ~Prozent,
          color = ~Partei, colors = colorPartei) %>% 
  add_lines(frame = ~frame, ids = ~Partei, showlegend = TRUE)

Parteien <- Parteien %>% 
  animation_slider(currentvalue = list(prefix = "Wahljahr: "), 
                   font = list(color = "black", 
                               size = 12)) %>% 
  animation_opts(frame = 700, transition = 350, 
                 redraw = FALSE, mode = "immediate") %>% 
  layout(title = "Zweitstimmen je Partei",
         xaxis = list(title = ""), 
         plot_bgcolor = "#E5E5E5", 
         paper_bgcolor = "#E5E5E5")

Parteien

# Save Animation

ParteienHTML <- saveWidget(Parteien, "BTWahlen4921.html", 
                           selfcontained = F, background = "#E5E5E5", 
                           libdir = wd)

# all parties animated

PlotlyTest <- BTGesamt %>% 
  plot_ly(x = ~Wahljahr, y = ~Prozent) %>% 
  add_lines(alpha = 0.5, color = ~Partei, linetype = ~Partei) %>% 
  add_markers(frame = ~Wahljahr,
              showlegend = FALSE) %>% 
  animation_opts(frame = 1000, transition = 500) %>% 
  animation_slider(color = "black")

PlotlyTest



# Results per Party per federated state-----

file.choose()

#load data

Laender <-fread("C:\\Fun Fun Fun\\Projekte\\Wahlen\\BT Wahlen\\kerg2(1).csv", 
                             skip = 9, header = TRUE, fill = TRUE, 
                             stringsAsFactors = FALSE)

#preprocessing data


LaenderPartei <- Laender %>% 
  filter(UegGebietsnummer == 99)

LaenderPartei$Prozent <- as.numeric(gsub(",", ".", LaenderPartei$Prozent))

# Erststimmen je Partei und Land

LaenderParteiErst <- LaenderPartei %>% 
  filter(Gruppenart == "Partei") %>% 
  filter(Prozent >= 5) %>% 
  filter(Stimme == 1)

# select cols

LaenderParteiErst <- LaenderParteiErst %>% 
  select(5,9,13)

# Wide Format 

# LaenderParteiErst <- spread(LaenderParteiErst, Gruppenname, Prozent)

# Zweitstimmen je Partei und Land

LaenderParteiZweit <- LaenderPartei %>%
  filter(Gruppenart == "Partei") %>% 
  filter(Prozent >= 5) %>% 
  filter(Stimme == 2) 

# select cols 


LaenderParteiZweit <- LaenderParteiZweit %>% 
  select(5,9,13)

# Wahlkreise

WK <- Laender %>% 
  filter(Gebietsart == "Wahlkreis")

WK$Prozent <- as.numeric(gsub(",", ".", WK$Prozent))

WK1 <- WK %>% 
  filter(Gruppenart == "Partei") %>% 
  filter(Prozent >= 5) %>% 
  filter(Stimme == 1) 


WK2 <- WK %>% 
  filter(Gruppenart == "Partei") %>% 
  filter(Prozent >= 5) %>% 
  filter(Stimme == 2) 


# Custom Colorisation


colorPartei2 <- c("CDU" = "#000000", 
                  "CSU" = "#0080c9",
                 "SPD" = "#e3001b", 
                 "FDP" = "#ffee02", 
                 "GRÜNE" = "#43933c", 
                 "DIE LINKE" = "#df0404", 
                 "AfD" = "#047ec9", 
                 "FREIE WÄHLER" = "#09488f"
                 )


# Test 1 - Erststimmen

shared_data <- LaenderParteiErst %>% 
  SharedData$new()

p1 <- shared_data %>%
  plot_ly(x =  ~Gruppenname, y = ~round(Prozent, digit = 2), 
          color = ~Gruppenname, colors = colorPartei2, 
          type = "bar") %>% 
  layout(
    title = "Erststimmen je Bundesgebiet und Partei - Bundestagswahl 2021",
    xaxis = list(title = ""),
    yaxis = list(title = "Prozent"),
    plot_bgcolor = "#E5E5E5", 
    paper_bgcolor = "#E5E5E5")
    

p1

p1 <- bscols(widths = c(2, 10),
            filter_checkbox(id = "Gebietsname", label = "Bundesland", 
                              sharedData = shared_data, group = ~Gebietsname),
             p1)

p1

# Daten abspeichern----
dir.create(paste0(getwd(),"/interactive Plots"))

results_dir = paste0(getwd(),"/interactive Plots") # get directory

htmlwidgets::saveWidget(p1, "index.html")

# Test 2 - Zweitstimmen

shared_data <- LaenderParteiZweit %>% 
  SharedData$new()

p2 <- shared_data %>%
  plot_ly(x =  ~Gruppenname, y = ~round(Prozent, digit = 2), 
          color = ~Gruppenname, colors = colorPartei2, 
          type = "bar") %>% 
  layout(
    title = "Zweitstimmen je Bundesgebiet und Partei",
    xaxis = list(title = ""),
    yaxis = list(title = "Prozent"))


p2

p2 <- bscols(widths = c(2, 9),
       filter_checkbox(id = "Gebietsname", label = "Bundesland", 
                       sharedData = shared_data, group = ~Gebietsname),
       p2) 

p2

#Wahlkreise 

shared_datawk <- WK1 %>% 
  SharedData$new()

WkPlot1 <- shared_datawk %>%
  plot_ly(x =  ~Gruppenname, y = ~round(Prozent, digit = 2), 
          color = ~Gruppenname, colors = colorPartei2, 
          type = "bar") %>% 
  layout(
    title = "Zweitstimmen je Wahlkreis und Partei",
    xaxis = list(title = ""),
    yaxis = list(title = "Prozent"))


WkPlot1

WkPlot1 <- bscols(widths = c(2, 9),
             filter_select(id = "Gebietsname", label = "Wahlkreis", 
                             sharedData = shared_datawk, group = ~Gebietsname),
             WkPlot1) 

WkPlot1

# Strukturdaten BT Wahl 21----

Strukturdaten <- fread("C:\\Fun Fun Fun\\Projekte\\Wahlen\\BT Wahlen\\btw21_strukturdaten.csv", 
                       skip = 8, header = TRUE, fill = TRUE, dec = ".",
                       stringsAsFactors = FALSE)

str(Strukturdaten)

Strukturdaten <- Strukturdaten %>%
  filter(`Wahlkreis-Nr.` < 900)

# to merge with WK dataframe, it is necessary to spread the WK Df

WK <- Laender %>% 
  filter(Gebietsart == "Wahlkreis")

#

WK <- WK %>% select(5,8, 9, 11, 13)

#

WKErst <- WK %>% 
  filter(Gruppenart == "Partei", Stimme == 1) 

#

WKErst$Prozent <- as.numeric(gsub(",", ".", WKErst$Prozent))

# change NA to 0

WKErst[is.na(WKErst)] <- 0

#

WKSpreadErst <- WKErst %>%
  select(1, 3, 5) %>%
  spread(Gruppenname, Prozent)

# merge respective dataframes 

# align the col names for identification

colnames(Strukturdaten)[3] <- "Gebietsname"


WKStrukturdaten <- merge(WKSpreadErst, Strukturdaten, by='Gebietsname', all.x=TRUE)

WKStrukturdaten[is.na(WKStrukturdaten)] <- 0

save(WKStrukturdaten, file = "WKStrukturdaten.Rda")











# Funktionen----
# Plotly transforms ----
plotly.transforms <- function(x) {
  
  y <- list(
    list(
      type = "filter",
      target = ~Gebietsname,
      operation = "=",
      value = unique(x)[1]
    )
  )
  
  return(y) 
  
}

plotly.buttons <- function(x, pos.x = 2, pos.y = 1) {
  
  y <- 
    list(
      list(
        active = 0,
        showactive = T,
        y = pos.y, x = pos.x, yref = "paper",
        buttons = apply(
          as.data.frame(unique(x)), 1,
          function(z) {
            list(
              method = "restyle",
              args = list("transforms[0].value", z), label = z
            )
          }
        )
      )
    )
  
  return(y) 
  
}

