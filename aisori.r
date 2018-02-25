#TODO
#сделать цикл по файлам, желательно с распаковкой из архива
#брать из файла названий метеостанций названия и координаты
#записать в файл


library(xlsx)
library(dplyr)

setwd(("C:/Users/Dmitriy/Desktop/Meteo"))#устанавливаем рабочую директорию

#читаем данные метеостанции
tab <- read.csv2("SCH121.txt",
                 sep = ';',
                 dec = '.',
                 header = FALSE,
                 encoding = 'UTF-8')

#читаем индексы колонок
columns <- read.csv2('idx.txt',
                     sep = ';',
                     encoding = 'UTF-8',
                     header = FALSE)

spisok <- read.csv2('spisok.csv',
                    sep = ';',
                    encoding = 'UTF-8',
                    header = FALSE)

spisok <- spisok[,c(1,2,3,5,7)]

colnames(spisok) <- c('St_Num', 'St_Name','Lat','Lon','Elev')
#создаем вектор названий из файла индексов и присваиваем названия колонок
col_names <- as.vector(columns[[1]])
colnames(tab) <- col_names

#выделяем нужные столбцы
selection <- c('Синоптический_индекс_станции_',
               'Год___по_Гринвичу_',
               'Месяц_по_Гринвичу_',
               'День__по_Гринвичу_',
               'Срок__по_Гринвичу_',
               'Сумма_осадков_',
               'Макс._темперура_воздуха_между_сроками_')
#создаем новую таблицу с нужными параметрами
ftab <- subset(tab, select = selection)
#суммируем осадки чтобы получить сумму за 12 ч
n = 4 
prec_sum <- colSums(matrix(ftab$Сумма_осадков_, nrow = n))
out_tab = ftab[seq(1, nrow(ftab), n), ]
#выбираем максимальную температуру за эти 4 срока (за 12ч)
max_temp <- sapply(split(ftab$Макс._темперура_воздуха_между_сроками_, rep(1:(nrow(ftab)/n), each = n)), max)
#присваиваем названия
out_tab$Сумма_осадков_ <- prec_sum
out_tab$Макс._темперура_воздуха_между_сроками_ <- max_temp
#удаляем все строки, где содержится NA
out_tab <- na.omit(out_tab)
#задаем условия: температура меньше 0 и сумма осадков больше 30
condition <- out_tab$Макс._темперура_воздуха_между_сроками_ < 0 & out_tab$Сумма_осадков_ > 30
out_tab <- subset(out_tab, condition)

out_tab$St_Name <- spisok$St_Name[match(out_tab$Синоптический_индекс_станции_, spisok$St_Num)]
out_tab$Lat <- spisok$Lat[match(out_tab$Синоптический_индекс_станции_, spisok$St_Num)]
out_tab$Lon <- spisok$Lon[match(out_tab$Синоптический_индекс_станции_, spisok$St_Num)]

