#testing script

library(data.table)

my_data <- fread(files[1], header = F,
                 stringsAsFactors = F,
                 nrows = 1000000,
                 select = c(1:5, 48, 69),
                 col.names = columns[c(1:5, 48, 69)])
#                 colClasses = c("integer","integer","integer","integer","integer","numeric","numeric"))



#������ ������ � ������������
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
              mutate(����_��_�������� = ymd_h(����_��_��������))



final_data <- data.frame()
#names(final_data) <- names(data_set)#c("�������������_������_�������", "����_��_��������", "�����_�������", "����._���������_�������_�����_�������")

# for(i in 1:nrow(data_set)){
#   if(data_set[i, "����._���������_�������_�����_�������"] >= 0){
#     next
#   }
#   if(difftime(data_set[i+1, "����_��_��������"], data_set[i, "����_��_��������"], units = "hours") > 4){
#     if(data_set[i, "�����_�������"] < 20){
#       next
#     }else{
#       #save to out
#       new_row <- c(data_set[i,]$�������������_������_�������,
#                   data_set[i,]$����_��_��������,
#                   data_set[i,]$�����_�������,
#                   data_set[i,]$����._���������_�������_�����_�������)
#       final_data <- rbind(final_data, new_row)
#       next
#     }
#   }else{
#     if(difftime(data_set[i+2, "����_��_��������"], data_set[i, "����_��_��������"], units = "hours") > 7){
#       if((data_set[i, "�����_�������"] + data_set[i+1, "�����_�������"]) < 20){
#         next
#       }else{
#         if(data_set[i+1, "����._���������_�������_�����_�������"] >= 0){
#           next
#         }
#         #save to out with date of last measurement
#         new_row <- c(data_set[i,]$�������������_������_�������,
#                      data_set[i,]$����_��_��������,
#                      sum(data_set[i:(i+1),]$�����_�������),
#                      mean(data_set[i:(i+1),]$����._���������_�������_�����_�������))
#         final_data <- rbind(final_data, new_row)
#         i = i + 1
#         next
#       } 
#     }else{
#       if(difftime(data_set[i+3, "����_��_��������"], data_set[i, "����_��_��������"], units = "hours") > 10){
#         if((data_set[i, "�����_�������"] + data_set[i+1, "�����_�������"] + data_set[i+2, "�����_�������"]) < 20){
#           next
#         }else{
#           if(data_set[i+1, "����._���������_�������_�����_�������"] >= 0){
#             next
#           }
#           if(data_set[i+2, "����._���������_�������_�����_�������"] >= 0){
#             next
#           }
#           #save to out with date of last measurement
#           new_row <- c(data_set[i,]$�������������_������_�������,
#                        data_set[i,]$����_��_��������,
#                        sum(data_set[i:(i+2),]$�����_�������),
#                        mean(data_set[i:(i+2),]$����._���������_�������_�����_�������))
#           final_data <- rbind(final_data, new_row)
#           i = i + 2
#           next
#         } 
#       }else{
#         if((data_set[i, "�����_�������"] + data_set[i+1, "�����_�������"] + data_set[i+2, "�����_�������"] + data_set[i+3, "�����_�������"]) < 20){
#           next
#         }else{
#           if(data_set[i+1, "����._���������_�������_�����_�������"] >= 0){
#             next
#           }
#           if(data_set[i+2, "����._���������_�������_�����_�������"] >= 0){
#             next
#           }
#           if(data_set[i+3, "����._���������_�������_�����_�������"] >= 0){
#             next
#           }
#           #save to out with date of last measurement
#           new_row <- c(data_set[i,]$�������������_������_�������,
#                        data_set[i,]$����_��_��������,
#                        sum(data_set[i:(i+3),]$�����_�������),
#                        mean(data_set[i:(i+3),]$����._���������_�������_�����_�������))
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
  if(data_set[i, "����._���������_�������_�����_�������"] >= 0){
    next
  }
  
  timediff <- difftime(data_set[i+1, "����_��_��������"], data_set[i, "����_��_��������"], units = "hours")
  
  if(timediff > 4 | is.na(timediff)){
    if(data_set[i, "�����_�������"] < 20){
      next
    }else{
      #save to out
      new_row <- data.frame(data_set[i,]$�������������_������_�������,
                            data_set[i,]$����_��_��������,
                            data_set[i,]$�����_�������,
                            data_set[i,]$����._���������_�������_�����_�������)
      names(new_row) <- c("�������������_������_�������", "����_��_��������", "�����_�������", "����._���������_�������_�����_�������")
      final_data <- rbind(final_data, new_row)
      next
    }
  }else{
    timediff <- difftime(data_set[i+2, "����_��_��������"], data_set[i, "����_��_��������"], units = "hours")
    if(timediff > 7 | is.na(timediff)){
      if((data_set[i, "�����_�������"] + data_set[i+1, "�����_�������"]) < 20){
        next
      }else{
        if(data_set[i+1, "����._���������_�������_�����_�������"] >= 0){
          next
        }
        #save to out with date of last measurement
        new_row <- data.frame(data_set[i,]$�������������_������_�������,
                              data_set[i,]$����_��_��������,
                              sum(data_set[i:(i+1),]$�����_�������),
                              mean(data_set[i:(i+1),]$����._���������_�������_�����_�������))
        names(new_row) <- c("�������������_������_�������", "����_��_��������", "�����_�������", "����._���������_�������_�����_�������")
        final_data <- rbind(final_data, new_row)
        i = i + 1
        next
      } 
    }else{
      timediff <- difftime(data_set[i+3, "����_��_��������"], data_set[i, "����_��_��������"], units = "hours")
      if(timediff > 10 | is.na(timediff)){
        if((data_set[i, "�����_�������"] + data_set[i+1, "�����_�������"] + data_set[i+2, "�����_�������"]) < 20){
          next
        }else{
          if(data_set[i+1, "����._���������_�������_�����_�������"] >= 0){
            next
          }
          if(data_set[i+2, "����._���������_�������_�����_�������"] >= 0){
            next
          }
          #save to out with date of last measurement
          new_row <- data.frame(data_set[i,]$�������������_������_�������,
                                data_set[i,]$����_��_��������,
                                sum(data_set[i:(i+2),]$�����_�������),
                                mean(data_set[i:(i+2),]$����._���������_�������_�����_�������))
          names(new_row) <- c("�������������_������_�������", "����_��_��������", "�����_�������", "����._���������_�������_�����_�������")
          final_data <- rbind(final_data, new_row)
          i = i + 2
          next
        } 
      }else{
        if((data_set[i, "�����_�������"] + data_set[i+1, "�����_�������"] + data_set[i+2, "�����_�������"] + data_set[i+3, "�����_�������"]) < 20){
          next
        }else{
          if(data_set[i+1, "����._���������_�������_�����_�������"] >= 0){
            next
          }
          if(data_set[i+2, "����._���������_�������_�����_�������"] >= 0){
            next
          }
          if(data_set[i+3, "����._���������_�������_�����_�������"] >= 0){
            next
          }
          #save to out with date of last measurement
          new_row <- data.frame(data_set[i,]$�������������_������_�������,
                                data_set[i,]$����_��_��������,
                                sum(data_set[i:(i+3),]$�����_�������),
                                mean(data_set[i:(i+3),]$����._���������_�������_�����_�������))
          names(new_row) <- c("�������������_������_�������", "����_��_��������", "�����_�������", "����._���������_�������_�����_�������")
          final_data <- rbind(final_data, new_row)
          i = i + 3
          next
        } 
      }
    }
  }
}



if(hour(data_set[4, "����_��_��������"])-hour(data_set[1, "����_��_��������"]) > 2)
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

  mutate(�������������_������_������� = as.integer(�������������_������_�������),
                                     ���___��_�������� = as.integer(���___��_��������),
                                     �����_��_�������� = as.integer(�����_��_��������),
                                     ����__��_�������� = as.integer(����__��_��������),
                                     ����__��_�������� = as.integer(����__��_��������),
                                     �����_������� = as.numeric(�����_�������),
                                     ����._���������_�������_�����_������� = as.numeric(����._���������_�������_�����_�������)) %>%
    na.omit() %>%
    unite(����_��_��������, ���___��_��������, �����_��_��������, ����__��_��������, ����__��_��������, sep = "_")%>%
    mutate(����_��_�������� = ymd_h(����_��_��������))
