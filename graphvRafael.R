## -----------------------------------------------
## Federal University of Para - Brazil
## Mathematical-Statistical Modelling Lab
## Prof. Dr. rer. nat. Joao Marcelo Brazao Protazio
## Modific.: Rafael Barbosa
## mprotazio@gmail.com
## Belem, 26. Mai 2019
## ------------------------------------------------

rm(list = ls())

## ---------------------
## ---------------------
## [0] Packages
## ---------------------
## ---------------------


if(!require(tidyverse)) {
  install.packages("tidyverse", dependencies = T);
  require(tidyverse)
}


if(!require(openxlsx)) {
  install.packages("openxlsx", dependencies = T);
  require(openxlsx)
}


if(!require(lubridate)) {
  install.packages("lubridate", dependencies = T);
  require(lubridate)
}



## ---------------------
## ---------------------
## [1] Data Organization
## ---------------------
## ---------------------


data <- read.xlsx(xlsxFile = "dados1.xlsx")


## ---------------------
## ---------------------
## [2] Visualization
## ---------------------
## ---------------------


#--- Figure 1

data %>% count(Year)


meses <- c(rep(x = 1:12, 3), 1:11, rep(x = 1:12, 6), 1)


data %>%
  arrange(Year) %>%
  mutate(meses1 = meses) %>%
  mutate(periodo = as.Date(paste(Year, meses1, "01", sep = "-"))) %>%
  ggplot(data = .) +
  geom_line(aes(x = periodo, y = `Chl-a(mg.m-3)`)) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.y = element_text(colour = "black", face = "bold", size = 14),
        axis.title.x = element_text(colour = "black", face = "bold", size = 14),
        axis.text = element_text(colour = "black", size = 16),
        strip.text.x = element_text(size = 12, colour = "white"),
        strip.text.y = element_text(size = 12, colour = "white"),
        legend.title = element_text(size = 16, color = "black", face = "bold"),
        legend.text = element_text(size = 16, color = "black"),
        axis.line = element_line(size = 0.5, colour = "black")) +
  labs(x = "Year", y = "Precipitation (mm)") +
  scale_x_date(breaks = seq(as.Date("2006-01-01"), as.Date("2016-12-31"), by="1 years"),
               labels = scales::date_format("%Y"))




data %>%
  arrange(Year) %>%
  mutate(meses1 = meses) %>%
  mutate(periodo = as.Date(paste(Year, meses1, "01", sep = "-"))) %>%
  mutate(coisa1 = case_when(meses1 == 12 | meses1 == 1 | meses1 == 2 ~ "Rising",
                            meses1 == 3 | meses1 == 4 | meses1 == 5 ~ "High water",
                            meses1 == 6 | meses1 == 7 | meses1 == 8 ~ "Falling",
                            meses1 == 9 | meses1 == 10 | meses1 == 11 ~ "Low water"
  )) %>%
  ggplot(data = ., aes(x = factor(meses1), y = `Temp.(oC)`, fill = coisa1)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.y = element_text(colour = "black", face = "bold", size = 14),
        axis.title.x = element_text(colour = "black", face = "bold", size = 14),
        axis.text = element_text(colour = "black", size = 16),
        strip.text.x = element_text(size = 12, colour = "white"),
        strip.text.y = element_text(size = 12, colour = "white"),
        legend.title = element_text(size = 16, color = "black", face = "bold"),
        legend.text = element_text(size = 16, color = "black"),
        axis.line = element_line(size = 0.5, colour = "black")) +
  scale_fill_brewer(palette = "Dark2")





banco <- read.table(file = "dados_precipitacao.txt", header = T, sep = "\t", dec = ".")

require(lubridate)



banco %>%
  mutate(ano = str_sub(string = Data, start = 7, end = 10),
         mes = str_sub(string = Data, start = 4, end = 5),
         dia = str_sub(string = Data, start = 1, end = 2)) %>%
  mutate(nova_data = as_date(paste(ano, mes, dia, sep = "/"))) %>%
  filter(nova_data > "2006-01-01") %>%
  mutate(periodo = case_when(month(nova_data) == 12 | month(nova_data) == 1 | month(nova_data) == 2 ~ "Rising",
                             month(nova_data) == 3 | month(nova_data) == 4 | month(nova_data) == 5 ~ "High water",
                             month(nova_data) == 6 | month(nova_data) == 7 | month(nova_data) == 8 ~ "Falling",
                             month(nova_data) == 9 | month(nova_data) == 10 | month(nova_data) == 11 ~ "Low water"
  )) %>%
  mutate(periodo = factor(x = periodo)) %>%
  ggplot(data = .) +
  geom_line(aes(x = nova_data, y = PrecipitacaoTotal, colour = periodo)) +
  #facet_wrap(~ periodo, scales = "free") +
  labs(x = "Year", y = "Precipitation (mm)") +
  scale_x_date(breaks = seq(as.Date("2006-01-01"), as.Date("2017-01-01"), by="1 years"),
               labels = scales::date_format("%Y")) +
  theme_bw()




#----------------------- Dataset (chuva)



banco <- openxlsx::read.xlsx(xlsxFile = "chuva1.xlsx", detectDates = T) %>%
  mutate(Cycle = factor(x = Cycle, levels = c("R", "H", "F", "L")))



#----------------------- Graphic precipitation (mm)


banco %>%
  ggplot(data = .) +
  geom_line(aes(x = Data, y = PrecipitacaoTotal)) +
  scale_x_date(breaks = seq(as.Date("2006-01-01"), as.Date("2016-12-31"), by="1 years"),
               labels = scales::date_format("%Y")) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.y = element_text(colour = "black", face = "bold", size = 14),
        axis.title.x = element_text(colour = "black", face = "bold", size = 14),
        axis.text = element_text(colour = "black", size = 14),
        strip.text.x = element_text(size = 12, colour = "white"),
        strip.text.y = element_text(size = 12, colour = "white"),
        legend.title = element_text(size = 16, color = "black", face = "bold"),
        legend.text = element_text(size = 16, color = "black"),
        axis.line = element_line(size = 0.5, colour = "black")) +
  labs(x = "Year", y = "Precipitation (mm)")



#----------------------- Graphic mean temperature (ºC)

banco %>%
  mutate(temperatura =  rowMeans(select(., TempMaximaMedia, TempMinimaMedia), na.rm = TRUE)) %>%
  ggplot(data = .) +
  geom_line(aes(x = Data, y = temperatura)) +
  scale_x_date(breaks = seq(as.Date("2006-01-01"), as.Date("2016-12-31"), by="1 years"),
               labels = scales::date_format("%Y")) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.y = element_text(colour = "black", face = "bold", size = 14),
        axis.title.x = element_text(colour = "black", face = "bold", size = 14),
        axis.text = element_text(colour = "black", size = 12),
        strip.text.x = element_text(size = 12, colour = "white"),
        strip.text.y = element_text(size = 12, colour = "white"),
        legend.title = element_text(size = 16, color = "black", face = "bold"),
        legend.text = element_text(size = 16, color = "black"),
        axis.line = element_line(size = 0.5, colour = "black")) +
  labs(x = "Year", y = "Temperature (ºC)")


#----------------------- Graphic temperature min (ºC)

banco %>%
  ggplot(data = .) +
  geom_line(aes(x = Data, y = TempMinimaMedia)) +
  scale_x_date(breaks = seq(as.Date("2006-01-01"), as.Date("2016-12-31"), by="1 years"),
               labels = scales::date_format("%Y")) +
  scale_y_continuous(limits = c(min(banco$TempMinimaMedia), max(banco$TempMinimaMedia))) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.y = element_text(colour = "black", face = "bold", size = 14),
        axis.title.x = element_text(colour = "black", face = "bold", size = 14),
        axis.text = element_text(colour = "black", size = 14),
        strip.text.x = element_text(size = 12, colour = "white"),
        strip.text.y = element_text(size = 12, colour = "white"),
        legend.title = element_text(size = 16, color = "black", face = "bold"),
        legend.text = element_text(size = 16, color = "black"),
        axis.line = element_line(size = 0.5, colour = "black")) +
  labs(x = "Year", y = "Minimum temperature (ºC)")




#----------------------- Graphic temperature max (ºC)

banco %>%
  ggplot(data = .) +
  geom_line(aes(x = Data, y = TempMaximaMedia)) +
  scale_x_date(breaks = seq(as.Date("2006-01-01"), as.Date("2016-12-31"), by="1 years"),
               labels = scales::date_format("%Y")) +
  scale_y_continuous(limits = c(min(banco$TempMaximaMedia), max(banco$TempMaximaMedia))) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.y = element_text(colour = "black", face = "bold", size = 14),
        axis.title.x = element_text(colour = "black", face = "bold", size = 14),
        axis.text = element_text(colour = "black", size = 14),
        strip.text.x = element_text(size = 12, colour = "white"),
        strip.text.y = element_text(size = 12, colour = "white"),
        legend.title = element_text(size = 16, color = "black", face = "bold"),
        legend.text = element_text(size = 16, color = "black"),
        axis.line = element_line(size = 0.5, colour = "black")) +
  labs(x = "Year", y = "Maximum temperature (ºC)")

