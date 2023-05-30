data = tsda::sql_select2(token = '7434C334-6E04-43C7-B103-634963BF78EF',
                         'select * from rds_scheduler')


library(readxl)
data <- read_excel("data-raw/棱星数据项目管理套表_W21_v1.3_胡立磊_20230520.xlsx",
                   sheet = "data", col_types = c("text",
                                                 "text", "text", "text", "text", "date",
                                                 "date", "text", "numeric", "text",
                                                 "text"))
tsda::db_writeTable2(token ='7434C334-6E04-43C7-B103-634963BF78EF',table_name = 'rds_scheduler',r_object = data,append = T )
