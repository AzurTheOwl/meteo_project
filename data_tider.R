#TODO
#разные варианты выбора временных интервалов
#проверить суммирование



#загружаем библиотеки
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(ggplot2)
library(ggmap)
library(maps)
library(raster)

setwd("C:/Users/User/Desktop/Meteo")#устанавливаем рабочую директорию

#читаем имена колонок
columns <- scan('idx.txt',
                what = character(),
                encoding = 'UTF-8')

#читаем инфо о метеостанции
stations <- readr::read_delim('spisok.csv',
                       delim = ';',
                       col_names = c(columns[1], 'Локация_', 'Широта_', 'СЮ', 'Долгота_', 'ЗВ', 'Подъем_'),
                       trim_ws = T) %>%
            #форматируем координаты
            mutate(Широта_ = case_when(СЮ == 'ю.ш.' ~ -Широта_, TRUE ~ Широта_)) %>%
            mutate(Долгота_ = case_when(ЗВ == 'з.д.' ~ -Долгота_, TRUE ~ Долгота_)) %>%
            dplyr::select(-СЮ, -ЗВ)

target_data <- list()

#читаем список файлов
files <- list.files(path="data/", pattern="*.txt", full.names=T, recursive=FALSE)

#подготавливаем файлы данных
for (i in 1:length(files)) {
  #читаем данные с метеостанции
  data_set <- read_delim(files[i],
                         delim = ';',
                         col_types = cols(.default = "c"),
                         col_names = columns,
                         trim_ws = T) %>%
    #Выбираем нужные столбцы и форматируем типы данных
    dplyr::select(c('Синоптический_индекс_станции_',
             'Год___по_Гринвичу_',
             'Месяц_по_Гринвичу_',
             'День__по_Гринвичу_',
             'Срок__по_Гринвичу_',
             'Сумма_осадков_',
             'Макс._темперура_воздуха_между_сроками_')) %>%
    mutate(Синоптический_индекс_станции_ = as.integer(Синоптический_индекс_станции_),
            Год___по_Гринвичу_ = as.integer(Год___по_Гринвичу_),
            Месяц_по_Гринвичу_ = as.integer(Месяц_по_Гринвичу_),
            День__по_Гринвичу_ = as.integer(День__по_Гринвичу_),
            Срок__по_Гринвичу_ = as.integer(Срок__по_Гринвичу_),
            Сумма_осадков_ = as.numeric(Сумма_осадков_),
            Макс._темперура_воздуха_между_сроками_ = as.numeric(Макс._темперура_воздуха_между_сроками_)) %>%
    #убираем строки с NA
    na.omit() %>%
    #группируем по индексу станции
    group_by(Синоптический_индекс_станции_)%>%
    #форматируем время
    unite(Дата_по_Гринвичу_, Год___по_Гринвичу_, Месяц_по_Гринвичу_, День__по_Гринвичу_, Срок__по_Гринвичу_)%>%
    mutate(Дата_по_Гринвичу_ = ymd_h(Дата_по_Гринвичу_)) %>%
    #добавляем стобец временных периодов
    mutate(Начало_периода_ = cut(Дата_по_Гринвичу_, breaks = "12 hours")) %>%
    group_by(Синоптический_индекс_станции_, Начало_периода_) %>%
    #подсчитываем осадки и среднюю температуру за период
    summarize(Сумма_осадков_ = sum(Сумма_осадков_), Средняя_температура_ = mean(as.numeric(Макс._темперура_воздуха_между_сроками_))) %>% 
    #отбрасываем неподходящие строки
    filter(Средняя_температура_ < 0 & Сумма_осадков_ >= 30) %>%
    #добавляем информацию о станции
    inner_join(stations, by = 'Синоптический_индекс_станции_') %>%
    #формируем финальную таблицу
    dplyr::select(Синоптический_индекс_станции_, Локация_, Широта_, Долгота_, Подъем_, Начало_периода_, Сумма_осадков_, Средняя_температура_)
  
  #добавляем таблицу в итоговый список
  target_data[[i]] <- data_set
  
}

x#создаем финальную таблицу
final_data_set <- bind_rows(target_data)
#подсчитываем количество упоминаний станции в таблице
counts <- count(final_data_set, Локация_)
#добавляем число упоминаний отдельным столбцом и сортируем по возрастанию осадков
final_data_set <- inner_join(final_data_set, counts[, c(2,3)], by ="Локация_") %>%
  #ungroup() %>%
  #mutate(colorscalerate = (Сумма_осадков_-min(Сумма_осадков_))/(max(Сумма_осадков_)-min(Сумма_осадков_))) %>%
  #group_by(Синоптический_индекс_станции_, Начало_периода_) %>%
  arrange(Сумма_осадков_)

#строим график температура/осадки
final_data_set %>%
  ggplot(aes(x = Средняя_температура_, y = Сумма_осадков_)) +
  geom_point() #+
  #facet_grid(Синоптический_индекс_станции_ ~ Начало_периода_)
ggsave("Осадки-температура.png")

#загружаем карту России
russia <- getData("GADM", country= "RUS", level=1)
russia <- fortify(russia)
#делаем смещение для корректного отображения карты
russia = within(russia, {long = ifelse(long < 0, long + 360, long)})
final_data_set = within(final_data_set, {Долгота_ = ifelse(Долгота_ < 0, Долгота_ + 360, Долгота_)})

#строим карту с расположением метеостанций
locations_plot <- ggplot() +
  geom_map(data= russia, map= russia, aes(x=long,y=lat, map_id=id,group=group), fill="white", colour="black") +
  geom_point(data = final_data_set, aes(x = Долгота_, y = Широта_, color = Локация_), size = 2) +
  scale_size(range=c(2,7)) +
  labs(title= "Расположение метеостанций", x="Longitude", y= "Latitude") +
  coord_map()
#сохраняем карту
ggsave("Расположение метеостанций.png")
#отображаем карту (можно закомментировать)
locations_plot

#строим карту с отображением частоты осадков
precipitation_freq_plot <- ggplot() +
  geom_map(data= russia, map= russia, aes(x=long,y=lat, map_id=id,group=group), fill="white", colour="black") +
  geom_point(data = final_data_set, aes(x = Долгота_, y = Широта_, color = Локация_, size = n)) +
  scale_size(range=c(2,7)) +
  labs(title= "Частота осадков", x="Longitude", y= "Latitude") +
  coord_map()
#сохраняем карту
ggsave("Осадки (частота).png")
#отображаем карту (можно закомментировать)
precipitation_freq_plot

#строим карту с отображением уровня осадков
precipitation_vol_plot <- ggplot() +
  geom_map(data = russia, map = russia, aes(x = long, y = lat, map_id=id,group=group), fill="white", colour="black") +
  geom_point(data = final_data_set, aes(x = Долгота_, y = Широта_, color = Сумма_осадков_), size = 3) +
  scale_colour_gradient(low = "#add8e6", high = "#0000FF") +
  scale_size(range=c(2,7)) +
  labs(title= "Объём осадков", x="Longitude", y= "Latitude") +
  coord_map()
#сохраняем карту
ggsave("Осадки (объем).png")
#отображаем карту (можно закомментировать)
precipitation_vol_plot



 

