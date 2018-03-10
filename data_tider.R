#TODO
#разные варианты выбора временных интервалов


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

addRow <- function(data_set, final_set, iter, iter_range){
  new_row <- data.frame(data_set[iter,]$Синоптический_индекс_станции,
                        data_set[iter,]$Дата_по_Гринвичу,
                        sum(data_set[iter:(iter + iter_range),]$Сумма_осадков),
                        mean(data_set[iter:(iter + iter_range),]$Макс._темперура_воздуха_между_сроками))
  names(new_row) <- c("Синоптический_индекс_станции", "Дата_по_Гринвичу", "Сумма_осадков", "Средняя_температура")
  rbind(final_set, new_row)
}

findCondition <- function(data_set){

  output_data <- data.frame()
  i = 1
  
  while(i <= nrow(data_set)){
    if(data_set[i, "Макс._темперура_воздуха_между_сроками"] >= 0){
      i = i + 1
      next
    }
    
    timediff <- difftime(data_set[i+1, "Дата_по_Гринвичу"], data_set[i, "Дата_по_Гринвичу"], units = "hours")
    
    if(timediff > 4 | is.na(timediff)){
      if(data_set[i, "Сумма_осадков"] < 20){
        i = i + 1
        next
      }else{
        #save to out
        output_data <- addRow(data_set, output_data, i, 0)

        i = i + 1
        next
      }
    }else{
      timediff <- difftime(data_set[i+2, "Дата_по_Гринвичу"], data_set[i, "Дата_по_Гринвичу"], units = "hours")
      if(timediff > 7 | is.na(timediff)){
        if(sum(data_set[i:(i+1), "Сумма_осадков"]) < 20){
          i = i + 1
          next
        }else{
          if(data_set[i+1, "Макс._темперура_воздуха_между_сроками"] >= 0){
            if(data_set[i, "Сумма_осадков"] >= 20){
              output_data <- addRow(data_set, output_data, i, 0)
            }
            i = i + 2
            next
          }
          output_data <- addRow(data_set, output_data, i, 1)
          i = i + 2
          next
        } 
      }else{
        timediff <- difftime(data_set[i+3, "Дата_по_Гринвичу"], data_set[i, "Дата_по_Гринвичу"], units = "hours")
        if(timediff > 10 | is.na(timediff)){
          if(sum(data_set[i:(i+2), "Сумма_осадков"]) < 20){
            i=i+1
            next
          }else{
            if(data_set[i+1, "Макс._темперура_воздуха_между_сроками"] >= 0){
              if(data_set[i, "Сумма_осадков"] >= 20){
                output_data <- addRow(data_set, output_data, i, 0)
              }
              i = i + 2
              next
            }
            if(data_set[i+2, "Макс._темперура_воздуха_между_сроками"] >= 0){
              if(sum(data_set[i:(i+1), "Сумма_осадков"]) >= 20){
                output_data <- addRow(data_set, output_data, i, 1)
              }
              i = i + 3
              next
            }
            output_data <- addRow(data_set, output_data, i, 2)
            i = i + 3
            next
          } 
        }else{
          if(sum(data_set[i:(i+3), "Сумма_осадков"]) < 20){
            i = i + 1
            next
          }else{
            if(data_set[i+1, "Макс._темперура_воздуха_между_сроками"] >= 0){
              if(data_set[i, "Сумма_осадков"] >= 20){
                output_data <- addRow(data_set, output_data, i, 0)
              }
              i = i + 2
              next
            }
            if(data_set[i+2, "Макс._темперура_воздуха_между_сроками"] >= 0){
              if(sum(data_set[i:(i+1), "Сумма_осадков"]) >= 20){
                output_data <- addRow(data_set, output_data, i, 1)
              }
              i = i + 3
              next
            }
            if(data_set[i+3, "Макс._темперура_воздуха_между_сроками"] >= 0){
              if(sum(data_set[i:(i+2), "Сумма_осадков"]) >= 20){
                output_data <- addRow(data_set, output_data, i, 2)
              }
              i = i + 4
              next
            }
            output_data <- addRow(data_set, output_data, i, 3)
            i = i + 4
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


final_data <- data.frame()

#читаем список файлов
files <- list.files(path="data/", pattern="*.txt", full.names=T, recursive=FALSE)

#подготавливаем файлы данных
for (file_num in 1:length(files)) {
  #читаем данные с метеостанции
  data_buff <- fread(files[file_num], header = F,
                    stringsAsFactors = F,
                    nrows = -1,
                    select = c(1:5, 48, 69),
                    col.names = columns[c(1:5, 48, 69)],
                    blank.lines.skip = T,
                    fill = T,
                    strip.white = T,
                    na.strings = c("NA", "N/A", "null" ),
                    colClasses = list(character=1:90)) %>%
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

    findCondition()
  final_data <- rbind(final_data, data_buff)
}

write_excel_csv(final_data, "final_data.csv")

results <- inner_join(final_data, stations, by = 'Синоптический_индекс_станции') %>% 
  dplyr::select(Синоптический_индекс_станции, Локация, Широта, Долгота, Подъем, Дата_по_Гринвичу, Сумма_осадков, Средняя_температура)

write_excel_csv(results, "results.csv")

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

results_for_graphs <- filter(results, Сумма_осадков < 75, Средняя_температура > -35)

#строим график температура/осадки
results_for_graphs %>%
  ggplot(aes(x = Средняя_температура, y = Сумма_осадков)) +
  geom_point() +
  #facet_grid(Синоптический_индекс_станции ~ Начало_периода)
ggsave("Осадки-температура.png")

results_for_graphs %>%
  ggplot(aes(x = Дата_по_Гринвичу, y = Сумма_осадков)) +
  geom_point() +
  geom_smooth(method = "lm")
#facet_grid(Синоптический_индекс_станции ~ Начало_периода)
ggsave("Осадки-время.png")

results_for_graphs %>%
  ggplot(aes(x = Дата_по_Гринвичу, y = Средняя_температура)) +
  geom_point() +
  geom_smooth(method = "lm")
#facet_grid(Синоптический_индекс_станции ~ Начало_периода)
ggsave("Температура-время.png")

results_by_year <- mutate(results_for_graphs, Дата_по_Гринвичу = year(Дата_по_Гринвичу))

results_by_year %>%
  ggplot(aes(x = Дата_по_Гринвичу, y = Сумма_осадков)) +
  geom_point() +
  geom_smooth(method = "lm")
#facet_grid(Синоптический_индекс_станции ~ Начало_периода)
ggsave("Осадки-годы.png")

results_by_year %>%
  ggplot(aes(Дата_по_Гринвичу)) +
  geom_bar() #+
  #geom_smooth(method = "lm")
#facet_grid(Синоптический_индекс_станции ~ Начало_периода)
ggsave("События-годы.png")

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



 

