#testing script

library(data.table)

my_data <- fread(files[1], header = F,
                 stringsAsFactors = F,
                 nrows = 10,
                 select = c(1:5, 48, 69),
                 col.names = columns[c(1:5, 48, 69)])
#                 colClasses = c("integer","integer","integer","integer","integer","numeric","numeric"))


for(i in 1:nrow(df)){
  if(df[i, "Средняя_температура"] >= 0){
    next
  }
  if(difftime(data_set[i+1, "Дата_по_Гринвичу"], data_set[i, "Дата_по_Гринвичу"], units = "hours") > 4){
    if(df[i, "Сумма_осадков"] < 20){
      next
    }else{
      #save to out
      next
    }
  }else{
    if(difftime(data_set[i+2, "Дата_по_Гринвичу"], data_set[i, "Дата_по_Гринвичу"], units = "hours") > 7){
      if((df[i, "Сумма_осадков"] + df[i+1, "Сумма_осадков"]) < 20){
        next
      }else{
        if(df[i+1, "Средняя_температура"] >= 0){
          next
        }
        #save to out with date of last measurement
        i = i + 1
        next
      } 
    }else{
      if(difftime(data_set[i+3, "Дата_по_Гринвичу"], data_set[i, "Дата_по_Гринвичу"], units = "hours") > 10){
        if((df[i, "Сумма_осадков"] + df[i+1, "Сумма_осадков"] + df[i+2, "Сумма_осадков"]) < 20){
          next
        }else{
          if(df[i+1, "Средняя_температура"] >= 0){
            next
          }
          if(df[i+2, "Средняя_температура"] >= 0){
            next
          }
          #save to out with date of last measurement
          i = i + 2
          next
        } 
      }else{
        if((df[i, "Сумма_осадков"] + df[i+1, "Сумма_осадков"] + df[i+2, "Сумма_осадков"] + df[i+3, "Сумма_осадков"]) < 20){
          next
        }else{
          if(df[i+1, "Средняя_температура"] >= 0){
            next
          }
          if(df[i+2, "Средняя_температура"] >= 0){
            next
          }
          if(df[i+3, "Средняя_температура"] >= 0){
            next
          }
          #save to out with date of last measurement
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
