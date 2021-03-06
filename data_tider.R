#TODO
#������ �������� ������ ��������� ����������


#��������� ����������
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
  new_row <- data.frame(data_set[iter,]$�������������_������_�������,
                        data_set[iter,]$����_��_��������,
                        sum(data_set[iter:(iter + iter_range),]$�����_�������),
                        mean(data_set[iter:(iter + iter_range),]$����._���������_�������_�����_�������))
  names(new_row) <- c("�������������_������_�������", "����_��_��������", "�����_�������", "�������_�����������")
  rbind(final_set, new_row)
}

findCondition <- function(data_set){

  output_data <- data.frame()
  i = 1
  
  while(i <= nrow(data_set)){
    if(data_set[i, "����._���������_�������_�����_�������"] >= 0){
      i = i + 1
      next
    }
    
    timediff <- difftime(data_set[i+1, "����_��_��������"], data_set[i, "����_��_��������"], units = "hours")
    
    if(timediff > 4 | is.na(timediff)){
      if(data_set[i, "�����_�������"] < 20){
        i = i + 1
        next
      }else{
        #save to out
        output_data <- addRow(data_set, output_data, i, 0)

        i = i + 1
        next
      }
    }else{
      timediff <- difftime(data_set[i+2, "����_��_��������"], data_set[i, "����_��_��������"], units = "hours")
      if(timediff > 7 | is.na(timediff)){
        if(sum(data_set[i:(i+1), "�����_�������"]) < 20){
          i = i + 1
          next
        }else{
          if(data_set[i+1, "����._���������_�������_�����_�������"] >= 0){
            if(data_set[i, "�����_�������"] >= 20){
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
        timediff <- difftime(data_set[i+3, "����_��_��������"], data_set[i, "����_��_��������"], units = "hours")
        if(timediff > 10 | is.na(timediff)){
          if(sum(data_set[i:(i+2), "�����_�������"]) < 20){
            i=i+1
            next
          }else{
            if(data_set[i+1, "����._���������_�������_�����_�������"] >= 0){
              if(data_set[i, "�����_�������"] >= 20){
                output_data <- addRow(data_set, output_data, i, 0)
              }
              i = i + 2
              next
            }
            if(data_set[i+2, "����._���������_�������_�����_�������"] >= 0){
              if(sum(data_set[i:(i+1), "�����_�������"]) >= 20){
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
          if(sum(data_set[i:(i+3), "�����_�������"]) < 20){
            i = i + 1
            next
          }else{
            if(data_set[i+1, "����._���������_�������_�����_�������"] >= 0){
              if(data_set[i, "�����_�������"] >= 20){
                output_data <- addRow(data_set, output_data, i, 0)
              }
              i = i + 2
              next
            }
            if(data_set[i+2, "����._���������_�������_�����_�������"] >= 0){
              if(sum(data_set[i:(i+1), "�����_�������"]) >= 20){
                output_data <- addRow(data_set, output_data, i, 1)
              }
              i = i + 3
              next
            }
            if(data_set[i+3, "����._���������_�������_�����_�������"] >= 0){
              if(sum(data_set[i:(i+2), "�����_�������"]) >= 20){
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

setwd("C:/Users/User/Desktop/Meteo")#������������� ������� ����������

#������ ����� �������
columns <- scan('colnames.txt',
                what = character(),
                encoding = 'UTF-8')

columns <- gsub('[_]$', '', columns)


#������ ���� � ������������
stations <- readr::read_delim('meteo_stations.csv',
                       delim = ';',
                       col_names = c(columns[1], '�������', '������', '��', '�������', '��', '������'),
                       trim_ws = T) %>%
            #����������� ����������
            mutate(������ = case_when(�� == '�.�.' ~ -������, TRUE ~ ������)) %>%
            mutate(������� = case_when(�� == '�.�.' ~ -�������, TRUE ~ �������)) %>%
            dplyr::select(-��, -��)


final_data <- data.frame()

#������ ������ ������
files <- list.files(path="data/", pattern="*.txt", full.names=T, recursive=FALSE)

#�������������� ����� ������
for (file_num in 1:length(files)) {
  #������ ������ � ������������
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
    mutate(�������������_������_������� = as.integer(�������������_������_�������),
            ���___��_�������� = as.integer(���___��_��������),
            �����_��_�������� = as.integer(�����_��_��������),
            ����__��_�������� = as.integer(����__��_��������),
            ����__��_�������� = as.integer(����__��_��������),
            �����_������� = as.numeric(�����_�������),
            ����._���������_�������_�����_������� = as.numeric(����._���������_�������_�����_�������)) %>%
    #������� ������ � NA
    na.omit() %>%
    #����������� �����
    unite(����_��_��������, ���___��_��������, �����_��_��������, ����__��_��������, ����__��_��������, sep = "_")%>%
    mutate(����_��_�������� = ymd_h(����_��_��������)) %>%

    findCondition()
  final_data <- rbind(final_data, data_buff)
}

write_excel_csv(final_data, "final_data.csv")

results <- inner_join(final_data, stations, by = '�������������_������_�������') %>% 
  dplyr::select(�������������_������_�������, �������, ������, �������, ������, ����_��_��������, �����_�������, �������_�����������)

write_excel_csv(results, "results.csv")

# #������� ��������� �������
# final_data_set <- bind_rows(target_data)
# #������������ ���������� ���������� ������� � �������
# counts <- count(final_data_set, �������)
# #��������� ����� ���������� ��������� �������� � ��������� �� ����������� �������
# final_data_set <- inner_join(final_data_set, counts[, c(2,3)], by ="�������") %>%
#   #ungroup() %>%
#   #mutate(colorscalerate = (�����_�������-min(�����_�������))/(max(�����_�������)-min(�����_�������))) %>%
#   #group_by(�������������_������_�������, ������_�������) %>%
#   arrange(�����_�������)

results_for_graphs <- filter(results, �����_������� < 75, �������_����������� > -35)

#������ ������ �����������/������
results_for_graphs %>%
  ggplot(aes(x = �������_�����������, y = �����_�������)) +
  geom_point() +
  theme_light() +
  labs(title = "������-�����������", x = "������� �����������", y = "����� �������")
  #facet_grid(�������������_������_������� ~ ������_�������)
ggsave("������-�����������.png")

results_for_graphs %>%
  ggplot(aes(x = ����_��_��������)) +
  geom_point(aes(y = �����_�������, colour = "������")) +
  geom_point(aes(y = �������_����������� + 75, colour = "�����������")) +
  scale_y_continuous(sec.axis = sec_axis(trans = ~.-75, name = "������� �����������")) +
  theme_light() +
  labs(title = "������-�����������-�����", x = "���", y = "����� �������", colour = "��������") +
  theme(legend.position = "top")
#facet_grid(�������������_������_������� ~ ������_�������)
ggsave("������-�����������-�����.png")

results_for_graphs %>%
  ggplot(aes(x = ����_��_��������)) +
  stat_bin_2d(aes(y = �����_�������, colour = "������"), bins = 20) +
  stat_bin_2d(aes(y = �������_�����������+75, colour = "�����������"), bins = 20) +
  scale_y_continuous(sec.axis = sec_axis(trans = ~.-75, name = "������� �����������")) +
  theme_light() +
  labs(title = "������-�����������-�����", x = "���", y = "����� �������", colour = "��������") +
  theme(legend.position = "top")
#facet_grid(�������������_������_������� ~ ������_�������)
ggsave("������-�����������-����� ���������.png")

results_for_graphs %>%
  ggplot(aes(x = �������_�����������, y = �����_�������)) +
  stat_density2d(aes(fill = ..level..), geom = "polygon", n = 100, contour = T) +
  theme_light() +
  labs(title = "������-�����������", x = "������� �����������", y = "����� �������")
  #facet_grid(�������������_������_������� ~ ������_�������)
ggsave("������-����������� ������ ����.png")

results_for_graphs %>%
  ggplot(aes(x = �������_�����������, y = �����_�������)) +
  stat_bin2d(aes(fill = ..density..), bins = 15) +
  theme_light() +
  labs(title = "������-�����������", x = "������� �����������", y = "����� �������")
#facet_grid(�������������_������_������� ~ ������_�������)
ggsave("������-����������� ������.png")

results_for_graphs %>%
  ggplot(aes(x = �������_�����������, y = �����_�������, z = ����_��_��������)) +
  stat_summary_hex() +
  theme_light() +
  labs(title = "������-�����������", x = "������� �����������", y = "����� �������")
#facet_grid(�������������_������_������� ~ ������_�������)
ggsave("������-����������� �����.png")

results_for_graphs %>%
  ggplot(aes(x = ����_��_��������, y = �����_�������)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_light() +
  scale_x_datetime() + 
  scale_y_continuous(trans = "log") +
  labs(title = "������-�����", x = "���", y = "����� �������")
#facet_grid(�������������_������_������� ~ ������_�������)
ggsave("������-�����.png")

results_for_graphs %>%
  mutate(����_��_�������� = floor_date(����_��_��������, years(5))) %>%
  # mutate(����_��_�������� = round(as.numeric(����_��_��������)/5)*5) %>%
  # mutate(����_��_�������� = factor(����_��_��������)) %>%
  ggplot(aes(x = ����_��_��������, y = �����_�������)) +
  geom_boxplot(aes(group = ����_��_��������)) +
  geom_smooth(method = "lm") +
  #geom_line(aes(group = �����_�������), colour = "blue") +
  theme_light() +
  # scale_x_datetime() + 
  scale_y_continuous(trans = "log10") +
  labs(title = "������-�����", x = "���", y = "����� �������")
#facet_grid(�������������_������_������� ~ ������_�������)
ggsave("������-����� box.png")

library(scales)

reverselog_trans <- function(base = exp(1)) {
  trans <- function(x) -log(x, base)
  inv <- function(x) base^(-x)
  trans_new(paste0("reverselog-", format(base)), trans, inv, 
            log_breaks(base = base), 
            domain = c(1e-100, Inf))
}

results_for_graphs %>%
  ggplot(aes(x = ����_��_��������, y = -�������_�����������)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_light() +
  scale_y_continuous(trans=reverselog_trans(base=2), 
                     labels=trans_format("identity", function(x) -x)) +
  # scale_y_continuous(trans = "log2") +
  # scale_y_continuous(trans = "reverse") +
  labs(title = "�����������-�����", x = "���", y = "������� ����������� �� �������")
#facet_grid(�������������_������_������� ~ ������_�������)
ggsave("�����������-�����.png")

results_for_graphs %>%
  mutate(����_��_�������� = floor_date(����_��_��������, years(5))) %>%
  ggplot(aes(x = ����_��_��������, y = -�������_�����������)) +
  geom_boxplot(aes(group = ����_��_��������)) +
  geom_smooth(method = "lm") +
  theme_light() +
  scale_y_continuous(trans=reverselog_trans(base=2), 
                     labels=trans_format("identity", function(x) -x)) +
  # scale_y_continuous(trans = "log2") +
  # scale_y_continuous(trans = "reverse") +
  labs(title = "�����������-�����", x = "���", y = "������� ����������� �� �������")
#facet_grid(�������������_������_������� ~ ������_�������)
ggsave("�����������-����� box.png")

results_by_year <- mutate(results_for_graphs, ����_��_�������� = year(����_��_��������))

results_by_year %>%
  ggplot(aes(x = ����_��_��������, y = �����_�������)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_light()
#facet_grid(�������������_������_������� ~ ������_�������)
ggsave("������-����.png")

results_by_year %>%
  ggplot(aes(����_��_��������)) +
  stat_bin(aes(fill = -..density..), binwidth = 5, center = 2.5) +
  theme_light() +
  labs(title = "���������-����", x = "���", y = "����� �������") #+
  #coord_cartesian(xlim = c(1974, 2016))
  # xlim(1974, 2016) +
  # scale_x_continuous(limits = c(1974, 2016))

  #scale_fill_brewer(palette = "Blues") 
  #geom_histogram(binwidth = 5)
  #geom_bar() #+
  #geom_smooth(method = "lm")
#facet_grid(�������������_������_������� ~ ������_�������)
ggsave("�������-����.png")



results_by_month <- results_for_graphs %>%
  mutate(����_��_�������� = month(����_��_��������))

results_by_month %>%
  ggplot(aes(����_��_��������)) +
  stat_bin(aes(fill = -..density..), binwidth = 1, center = 2.5) +
  theme_light() +
  scale_x_discrete(labels = month.abb) +
  labs(title = "���������-������", x = "�����", y = "����� �������")
#coord_cartesian(xlim = c(1974, 2016))
# xlim(1974, 2016) +
# scale_x_continuous(limits = c(1974, 2016))

#scale_fill_brewer(palette = "Blues") 
#geom_histogram(binwidth = 5)
#geom_bar() #+
#geom_smooth(method = "lm")
#facet_grid(�������������_������_������� ~ ������_�������)
ggsave("�������-������.png")



#��������� ����� ������
russia <- getData("GADM", country= "RUS", level=1)
russia <- fortify(russia)
#������ �������� ��� ����������� ����������� �����
russia = within(russia, {long = ifelse(long < 0, long + 360, long)})
results_by_year_map = within(results_by_year, {������� = ifelse(������� < 0, ������� + 360, �������)})

#������ ����� � ������������� ������������
locations_plot <- ggplot() +
  geom_map(data= russia, map= russia, aes(x=long,y=lat, map_id=id,group=group), fill="white", colour="black") +
  geom_point(data = results_by_year_map, aes(x = �������, y = ������, color = �������), size = 2) +
  scale_size(range=c(2,7)) +
  labs(title= "������������ ������������", x="Longitude", y= "Latitude") +
  theme(legend.position="none") +
  coord_map(projection = "azequidistant")
#��������� �����
ggsave("������������ ������������.png")
#���������� ����� (����� ����������������)
locations_plot

#������ ����� � ������������ ������� �������
precipitation_freq_plot <- ggplot() +
  geom_map(data= russia, map= russia, aes(x=long,y=lat, map_id=id,group=group), fill="white", colour="black") +
  geom_count(data = results_by_year_map, aes(x = �������, y = ������, color = ..n..)) +
  scale_colour_gradient(low = "#56B1F7", high = "#132B43") +
  scale_size(range=c(2,7)) +
  theme_light() +
  labs(title= "������� ����������", x="�������", y= "������", color = "����� �������", size = "����� �������") +
  coord_map(projection = "azequidistant")
#��������� �����
ggsave("������ (�������).png")
#���������� ����� (����� ����������������)
precipitation_freq_plot



#������ ����� � ������������ ������ �������
precipitation_vol_plot <- ggplot() +
  geom_map(data = russia, map = russia, aes(map_id=id,group=group), fill="white", colour="black") +
  geom_point(data = arrange(results_by_year_map, �����_�������), aes(x = �������, y = ������, color = �����_�������), size = 3) +
  #scale_color_hue(direction = -1) +
  scale_colour_gradient(low = "#56B1F7", high = "#132B43") +
  #scale_colour_gradient(low = "#deebf7", high = "#3182bd") +
  #scale_color_brewer(palette = "Blues") +
  scale_size(range=c(2,7)) +
  theme_light() +
  labs(title= "�������� ������� �� 12 �����", x="�������", y= "������", color = "������ (��)") +
  coord_map(projection = "azequidistant")
#��������� �����
ggsave("������ (�����).png")
#���������� ����� (����� ����������������)
precipitation_vol_plot



 

