#TODO
#разные варианты выбора временных интервалов
#проверить суммирование



#загружаем библиотеки
library(dplyr)
library(tidyr)
library(readr)
library(data.table)
library(lubridate)
library(ggplot2)
library(ggmap)
library(maps)
library(raster)

findCondition <- function(data_set){
  
  output_data <- data.frame()
  
  for(i in 1:nrow(data_set)){
    if(data_set[i, "Макс._темперура_воздуха_между_сроками"] >= 0){
      next
    }
    
    timediff <- difftime(data_set[i+1, "Дата_по_Гринвичу"], data_set[i, "Дата_по_Гринвичу"], units = "hours")
    
    if(timediff > 4 | is.na(timediff)){
      if(data_set[i, "Сумма_осадков"] < 20){
        next
      }else{
        #save to out
        new_row <- data.frame(data_set[i,]$Синоптический_индекс_станции,
                              data_set[i,]$Дата_по_Гринвичу,
                              data_set[i,]$Сумма_осадков,
                              data_set[i,]$Макс._темперура_воздуха_между_сроками)
        names(new_row) <- c("Синоптический_индекс_станции", "Дата_по_Гринвичу", "Сумма_осадков", "Макс._темперура_воздуха_между_сроками")
        output_data <- rbind(output_data, new_row)
        next
      }
    }else{
      timediff <- difftime(data_set[i+2, "Дата_по_Гринвичу"], data_set[i, "Дата_по_Гринвичу"], units = "hours")
      if(timediff > 7 | is.na(timediff)){
        if((data_set[i, "Сумма_осадков"] + data_set[i+1, "Сумма_осадков"]) < 20){
          next
        }else{
          if(data_set[i+1, "Макс._темперура_воздуха_между_сроками"] >= 0){
            next
          }
          #save to out with date of last measurement
          new_row <- data.frame(data_set[i,]$Синоптический_индекс_станции,
                                data_set[i,]$Дата_по_Гринвичу,
                                sum(data_set[i:(i+1),]$Сумма_осадков),
                                mean(data_set[i:(i+1),]$Макс._темперура_воздуха_между_сроками))
          names(new_row) <- c("Синоптический_индекс_станции", "Дата_по_Гринвичу", "Сумма_осадков", "Макс._темперура_воздуха_между_сроками")
          output_data <- rbind(output_data, new_row)
          i = i + 1
          next
        } 
      }else{
        timediff <- difftime(data_set[i+3, "Дата_по_Гринвичу"], data_set[i, "Дата_по_Гринвичу"], units = "hours")
        if(timediff > 10 | is.na(timediff)){
          if((data_set[i, "Сумма_осадков"] + data_set[i+1, "Сумма_осадков"] + data_set[i+2, "Сумма_осадков"]) < 20){
            next
          }else{
            if(data_set[i+1, "Макс._темперура_воздуха_между_сроками"] >= 0){
              next
            }
            if(data_set[i+2, "Макс._темперура_воздуха_между_сроками"] >= 0){
              next
            }
            #save to out with date of last measurement
            new_row <- data.frame(data_set[i,]$Синоптический_индекс_станции,
                                  data_set[i,]$Дата_по_Гринвичу,
                                  sum(data_set[i:(i+2),]$Сумма_осадков),
                                  mean(data_set[i:(i+2),]$Макс._темперура_воздуха_между_сроками))
            names(new_row) <- c("Синоптический_индекс_станции", "Дата_по_Гринвичу", "Сумма_осадков", "Макс._темперура_воздуха_между_сроками")
            output_data <- rbind(output_data, new_row)
            i = i + 2
            next
          } 
        }else{
          if((data_set[i, "Сумма_осадков"] + data_set[i+1, "Сумма_осадков"] + data_set[i+2, "Сумма_осадков"] + data_set[i+3, "Сумма_осадков"]) < 20){
            next
          }else{
            if(data_set[i+1, "Макс._темперура_воздуха_между_сроками"] >= 0){
              next
            }
            if(data_set[i+2, "Макс._темперура_воздуха_между_сроками"] >= 0){
              next
            }
            if(data_set[i+3, "Макс._темперура_воздуха_между_сроками"] >= 0){
              next
            }
            #save to out with date of last measurement
            new_row <- data.frame(data_set[i,]$Синоптический_индекс_станции,
                                  data_set[i,]$Дата_по_Гринвичу,
                                  sum(data_set[i:(i+3),]$Сумма_осадков),
                                  mean(data_set[i:(i+3),]$Макс._темперура_воздуха_между_сроками))
            names(new_row) <- c("Синоптический_индекс_станции", "Дата_по_Гринвичу", "Сумма_осадков", "Макс._темперура_воздуха_между_сроками")
            output_data <- rbind(output_data, new_row)
            i = i + 3
            next
          } 
        }
      }
    }
  }
  return(output_data)
}

setwd("C:/Users/User/Desktop/Meteo")#устанавливаем рабочую директорию

#читаем имена колонок
columns <- scan('colnames.txt',
                what = character(),
                encoding = 'UTF-8')

# for(i in 1:length(columns)){
#  columns[i] = substr(columns[i], 1, nchar(columns[i])-1)
# }

columns <- gsub('[_]$', '', columns)


#читаем инфо о метеостанции
stations <- readr::read_delim('meteo_stations.csv',
                       delim = ';',
                       col_names = c(columns[1], 'Локация', 'Широта', 'СЮ', 'Долгота', 'ЗВ', 'Подъем'),
                       trim_ws = T) %>%
            #форматируем координаты
            mutate(Широта = case_when(СЮ == 'ю.ш.' ~ -Широта, TRUE ~ Широта)) %>%
            mutate(Долгота = case_when(ЗВ == 'з.д.' ~ -Долгота, TRUE ~ Долгота)) %>%
            dplyr::select(-СЮ, -ЗВ)

#target_data <- list()
final_data <- data.frame()

#читаем список файлов
files <- list.files(path="data/", pattern="*.txt", full.names=T, recursive=FALSE)

#подготавливаем файлы данных
for (i in 1:2) {
  #читаем данные с метеостанции
  data_buff <- fread(files[i], header = F,
                    stringsAsFactors = F,
                    nrows = -1,
                    select = c(1:5, 48, 69),
                    col.names = columns[c(1:5, 48, 69)],
                    blank.lines.skip = T,
                    fill = T,
                    strip.white = T,
                    na.strings = c("NA", "N/A", "null" ),
                    colClasses = list(character=1:90)) %>%
    #           read_delim(files[i],
    #                      delim = ';',
    #                      col_types = cols(.default = "c"),
    #                      col_names = columns,
    #                      trim_ws = T) %>%
    # #Выбираем нужные столбцы и форматируем типы данных
    # dplyr::select(c('Синоптический_индекс_станции',
    #          'Год___по_Гринвичу',
    #          'Месяц_по_Гринвичу',
    #          'День__по_Гринвичу',
    #          'Срок__по_Гринвичу',
    #          'Сумма_осадков',
    #          'Макс._темперура_воздуха_между_сроками')) %>%
    mutate(Синоптический_индекс_станции = as.integer(Синоптический_индекс_станции),
            Год___по_Гринвичу = as.integer(Год___по_Гринвичу),
            Месяц_по_Гринвичу = as.integer(Месяц_по_Гринвичу),
            День__по_Гринвичу = as.integer(День__по_Гринвичу),
            Срок__по_Гринвичу = as.integer(Срок__по_Гринвичу),
            Сумма_осадков = as.numeric(Сумма_осадков),
            Макс._темперура_воздуха_между_сроками = as.numeric(Макс._темперура_воздуха_между_сроками)) %>%
    #убираем строки с NA
    na.omit() %>%
    #форматируем время
    unite(Дата_по_Гринвичу, Год___по_Гринвичу, Месяц_по_Гринвичу, День__по_Гринвичу, Срок__по_Гринвичу, sep = "_")%>%
    mutate(Дата_по_Гринвичу = ymd_h(Дата_по_Гринвичу)) %>%
    #группируем по индексу станции
    #group_by(Синоптический_индекс_станции)%>%
    # #добавляем стобец временных периодов
    # mutate(Начало_периода = cut.Date(Дата_по_Гринвичу, breaks = "12 hours")) %>%
    # group_by(Синоптический_индекс_станции, Начало_периода) %>%
    #подсчитываем осадки и среднюю температуру за период
    # summarize(Сумма_осадков = sum(Сумма_осадков), Средняя_температура = mean(as.numeric(Макс._темперура_воздуха_между_сроками))) %>%
    # #отбрасываем неподходящие строки
    # filter(Средняя_температура < 0 & Сумма_осадков >= 30) %>%
    # # #добавляем информацию о станции
    findCondition()
  final_data <- rbind(final_data, data_buff)
}
# %>%
#     inner_join(stations, by = 'Синоптический_индекс_станции') %>%
#     #формируем финальную таблицу
#     #dplyr::select(Синоптический_индекс_станции, Локация, Широта, Долгота, Подъем, Начало_периода, Сумма_осадков, Средняя_температура)
#     dplyr::select(Синоптический_индекс_станции, Локация, Широта, Долгота, Подъем, Сумма_осадков, Средняя_температура)
#   #добавляем таблицу в итоговый список
#   target_data[[i]] <- data_set
#   print(i)
#   
# }

#создаем финальную таблицу
final_data_set <- bind_rows(target_data)
#подсчитываем количество упоминаний станции в таблице
counts <- count(final_data_set, Локация)
#добавляем число упоминаний отдельным столбцом и сортируем по возрастанию осадков
final_data_set <- inner_join(final_data_set, counts[, c(2,3)], by ="Локация") %>%
  #ungroup() %>%
  #mutate(colorscalerate = (Сумма_осадков-min(Сумма_осадков))/(max(Сумма_осадков)-min(Сумма_осадков))) %>%
  #group_by(Синоптический_индекс_станции, Начало_периода) %>%
  arrange(Сумма_осадков)

#строим график температура/осадки
final_data_set %>%
  ggplot(aes(x = Средняя_температура, y = Сумма_осадков)) +
  geom_point() #+
  #facet_grid(Синоптический_индекс_станции ~ Начало_периода)
ggsave("Осадки-температура.png")

#загружаем карту России
russia <- getData("GADM", country= "RUS", level=1)
russia <- fortify(russia)
#делаем смещение для корректного отображения карты
russia = within(russia, {long = ifelse(long < 0, long + 360, long)})
final_data_set = within(final_data_set, {Долгота = ifelse(Долгота < 0, Долгота + 360, Долгота)})

#строим карту с расположением метеостанций
locations_plot <- ggplot() +
  geom_map(data= russia, map= russia, aes(x=long,y=lat, map_id=id,group=group), fill="white", colour="black") +
  geom_point(data = final_data_set, aes(x = Долгота, y = Широта, color = Локация), size = 2) +
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
  geom_point(data = final_data_set, aes(x = Долгота, y = Широта, color = Локация, size = n)) +
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
  geom_point(data = final_data_set, aes(x = Долгота, y = Широта, color = Сумма_осадков), size = 3) +
  scale_colour_gradient(low = "#add8e6", high = "#0000FF") +
  scale_size(range=c(2,7)) +
  labs(title= "Объём осадков", x="Longitude", y= "Latitude") +
  coord_map()
#сохраняем карту
ggsave("Осадки (объем).png")
#отображаем карту (можно закомментировать)
precipitation_vol_plot



 

