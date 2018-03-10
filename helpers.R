#testing script

library(data.table)

my_data <- fread(files[1], header = F,
                 stringsAsFactors = F,
                 nrows = 1000000,
                 select = c(1:5, 48, 69),
                 col.names = columns[c(1:5, 48, 69)])
#                 colClasses = c("integer","integer","integer","integer","integer","numeric","numeric"))



#читаем данные с метеостанции
data_set <- fread(files[1], header = F,
                    stringsAsFactors = F,
                    nrows = 200000,
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
              mutate(Дата_по_Гринвичу = ymd_h(Дата_по_Гринвичу))



final_data <- data.frame()
#names(final_data) <- names(data_set)#c("Синоптический_индекс_станции", "Дата_по_Гринвичу", "Сумма_осадков", "Макс._темперура_воздуха_между_сроками")

# for(i in 1:nrow(data_set)){
#   if(data_set[i, "Макс._темперура_воздуха_между_сроками"] >= 0){
#     next
#   }
#   if(difftime(data_set[i+1, "Дата_по_Гринвичу"], data_set[i, "Дата_по_Гринвичу"], units = "hours") > 4){
#     if(data_set[i, "Сумма_осадков"] < 20){
#       next
#     }else{
#       #save to out
#       new_row <- c(data_set[i,]$Синоптический_индекс_станции,
#                   data_set[i,]$Дата_по_Гринвичу,
#                   data_set[i,]$Сумма_осадков,
#                   data_set[i,]$Макс._темперура_воздуха_между_сроками)
#       final_data <- rbind(final_data, new_row)
#       next
#     }
#   }else{
#     if(difftime(data_set[i+2, "Дата_по_Гринвичу"], data_set[i, "Дата_по_Гринвичу"], units = "hours") > 7){
#       if((data_set[i, "Сумма_осадков"] + data_set[i+1, "Сумма_осадков"]) < 20){
#         next
#       }else{
#         if(data_set[i+1, "Макс._темперура_воздуха_между_сроками"] >= 0){
#           next
#         }
#         #save to out with date of last measurement
#         new_row <- c(data_set[i,]$Синоптический_индекс_станции,
#                      data_set[i,]$Дата_по_Гринвичу,
#                      sum(data_set[i:(i+1),]$Сумма_осадков),
#                      mean(data_set[i:(i+1),]$Макс._темперура_воздуха_между_сроками))
#         final_data <- rbind(final_data, new_row)
#         i = i + 1
#         next
#       } 
#     }else{
#       if(difftime(data_set[i+3, "Дата_по_Гринвичу"], data_set[i, "Дата_по_Гринвичу"], units = "hours") > 10){
#         if((data_set[i, "Сумма_осадков"] + data_set[i+1, "Сумма_осадков"] + data_set[i+2, "Сумма_осадков"]) < 20){
#           next
#         }else{
#           if(data_set[i+1, "Макс._темперура_воздуха_между_сроками"] >= 0){
#             next
#           }
#           if(data_set[i+2, "Макс._темперура_воздуха_между_сроками"] >= 0){
#             next
#           }
#           #save to out with date of last measurement
#           new_row <- c(data_set[i,]$Синоптический_индекс_станции,
#                        data_set[i,]$Дата_по_Гринвичу,
#                        sum(data_set[i:(i+2),]$Сумма_осадков),
#                        mean(data_set[i:(i+2),]$Макс._темперура_воздуха_между_сроками))
#           final_data <- rbind(final_data, new_row)
#           i = i + 2
#           next
#         } 
#       }else{
#         if((data_set[i, "Сумма_осадков"] + data_set[i+1, "Сумма_осадков"] + data_set[i+2, "Сумма_осадков"] + data_set[i+3, "Сумма_осадков"]) < 20){
#           next
#         }else{
#           if(data_set[i+1, "Макс._темперура_воздуха_между_сроками"] >= 0){
#             next
#           }
#           if(data_set[i+2, "Макс._темперура_воздуха_между_сроками"] >= 0){
#             next
#           }
#           if(data_set[i+3, "Макс._темперура_воздуха_между_сроками"] >= 0){
#             next
#           }
#           #save to out with date of last measurement
#           new_row <- c(data_set[i,]$Синоптический_индекс_станции,
#                        data_set[i,]$Дата_по_Гринвичу,
#                        sum(data_set[i:(i+3),]$Сумма_осадков),
#                        mean(data_set[i:(i+3),]$Макс._темперура_воздуха_между_сроками))
#           final_data <- rbind(final_data, new_row)
#           i = i + 3
#           next
#         } 
#       }
#     }
#   }
# }

j=0

while(j <= 5){
  if(j == 3){
    j <- j+2
    next
  }
  print(j)
  j <- j+1
}




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
      final_data <- rbind(final_data, new_row)
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
        final_data <- rbind(final_data, new_row)
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
          final_data <- rbind(final_data, new_row)
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
          final_data <- rbind(final_data, new_row)
          i = i + 3
          next
        } 
      }
    }
  }
}



if(hour(data_set[4, "Дата_по_Гринвичу"])-hour(data_set[1, "Дата_по_Гринвичу"]) > 2)
  print("yes")



data_set <- fread(files[1], header = F,
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
    na.omit() %>%
    unite(Дата_по_Гринвичу, Год___по_Гринвичу, Месяц_по_Гринвичу, День__по_Гринвичу, Срок__по_Гринвичу, sep = "_")%>%
    mutate(Дата_по_Гринвичу = ymd_h(Дата_по_Гринвичу))
