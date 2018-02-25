#TODO
#������� ���� �� ������, ���������� � ����������� �� ������
#����� �� ����� �������� ������������ �������� � ����������
#�������� � ����


library(xlsx)
library(dplyr)

setwd(("C:/Users/Dmitriy/Desktop/Meteo"))#������������� ������� ����������

#������ ������ ������������
tab <- read.csv2("SCH121.txt",
                 sep = ';',
                 dec = '.',
                 header = FALSE,
                 encoding = 'UTF-8')

#������ ������� �������
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
#������� ������ �������� �� ����� �������� � ����������� �������� �������
col_names <- as.vector(columns[[1]])
colnames(tab) <- col_names

#�������� ������ �������
selection <- c('�������������_������_�������_',
               '���___��_��������_',
               '�����_��_��������_',
               '����__��_��������_',
               '����__��_��������_',
               '�����_�������_',
               '����._���������_�������_�����_�������_')
#������� ����� ������� � ������� �����������
ftab <- subset(tab, select = selection)
#��������� ������ ����� �������� ����� �� 12 �
n = 4 
prec_sum <- colSums(matrix(ftab$�����_�������_, nrow = n))
out_tab = ftab[seq(1, nrow(ftab), n), ]
#�������� ������������ ����������� �� ��� 4 ����� (�� 12�)
max_temp <- sapply(split(ftab$����._���������_�������_�����_�������_, rep(1:(nrow(ftab)/n), each = n)), max)
#����������� ��������
out_tab$�����_�������_ <- prec_sum
out_tab$����._���������_�������_�����_�������_ <- max_temp
#������� ��� ������, ��� ���������� NA
out_tab <- na.omit(out_tab)
#������ �������: ����������� ������ 0 � ����� ������� ������ 30
condition <- out_tab$����._���������_�������_�����_�������_ < 0 & out_tab$�����_�������_ > 30
out_tab <- subset(out_tab, condition)

out_tab$St_Name <- spisok$St_Name[match(out_tab$�������������_������_�������_, spisok$St_Num)]
out_tab$Lat <- spisok$Lat[match(out_tab$�������������_������_�������_, spisok$St_Num)]
out_tab$Lon <- spisok$Lon[match(out_tab$�������������_������_�������_, spisok$St_Num)]

