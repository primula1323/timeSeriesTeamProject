library(dplyr)

d1990 <- read.csv("D://시계열프로젝트//csv//1990_분기자료(1990~2016)_20250514_12093.csv", fileEncoding = "cp949")
d1991 <- read.csv("D://시계열프로젝트//csv//1991_분기자료(1990~2016)_20250514_12093.csv", fileEncoding = "cp949")
d1992 <- read.csv("D://시계열프로젝트//csv//1992_분기자료(1990~2016)_20250514_12093.csv", fileEncoding = "cp949")
d1993 <- read.csv("D://시계열프로젝트//csv//1993_분기자료(1990~2016)_20250514_12093.csv", fileEncoding = "cp949")
d1994 <- read.csv("D://시계열프로젝트//csv//1994_분기자료(1990~2016)_20250514_12093.csv", fileEncoding = "cp949")
d1995 <- read.csv("D://시계열프로젝트//csv//1995_분기자료(1990~2016)_20250514_31998.csv", fileEncoding = "cp949")
d1996 <- read.csv("D://시계열프로젝트//csv//1996_분기자료(1990~2016)_20250514_31998.csv", fileEncoding = "cp949")
d1997 <- read.csv("D://시계열프로젝트//csv//1997_분기자료(1990~2016)_20250514_31998.csv", fileEncoding = "cp949")
d1998 <- read.csv("D://시계열프로젝트//csv//1997_분기자료(1990~2016)_20250514_31998.csv", fileEncoding = "cp949")
d1999 <- read.csv("D://시계열프로젝트//csv//1999_분기자료(1990~2016)_20250514_31998.csv", fileEncoding = "cp949")
d2000 <- read.csv("D://시계열프로젝트//csv//2000_분기자료(1990~2016)_20250508_85793.csv", fileEncoding = "cp949")
d2001 <- read.csv("D://시계열프로젝트//csv//2001_분기자료(1990~2016)_20250508_85793.csv", fileEncoding = "cp949")
d2002 <- read.csv("D://시계열프로젝트//csv//2002_분기자료(1990~2016)_20250508_85793.csv", fileEncoding = "cp949")
d2003 <- read.csv("D://시계열프로젝트//csv//2003_분기자료(1990~2016)_20250508_85793.csv", fileEncoding = "cp949")
d2004 <- read.csv("D://시계열프로젝트//csv//2004_분기자료(1990~2016)_20250508_85793.csv", fileEncoding = "cp949")
d2005 <- read.csv("D://시계열프로젝트//csv//2005_분기자료(1990~2016)_20250508_96025.csv", fileEncoding = "cp949")
d2006 <- read.csv("D://시계열프로젝트//csv//2006_분기자료(1990~2016)_20250508_96025.csv", fileEncoding = "cp949")
d2007 <- read.csv("D://시계열프로젝트//csv//2007_분기자료(1990~2016)_20250508_96025.csv", fileEncoding = "cp949")
d2008 <- read.csv("D://시계열프로젝트//csv//2008_분기자료(1990~2016)_20250508_96025.csv", fileEncoding = "cp949")
d2009 <- read.csv("D://시계열프로젝트//csv//2009_분기자료(1990~2016)_20250508_96025.csv", fileEncoding = "cp949")
d2010 <- read.csv("D://시계열프로젝트//csv//2010_분기자료(1990~2016)_20250508_69411.csv", fileEncoding = "cp949")
d2011 <- read.csv("D://시계열프로젝트//csv//2011_분기자료(1990~2016)_20250508_69411.csv", fileEncoding = "cp949")
d2012 <- read.csv("D://시계열프로젝트//csv//2012_분기자료(1990~2016)_20250508_69411.csv", fileEncoding = "cp949")
d2013 <- read.csv("D://시계열프로젝트//csv//2013_분기자료(1990~2016)_20250508_69411.csv", fileEncoding = "cp949")
d2014 <- read.csv("D://시계열프로젝트//csv//2014_분기자료(1990~2016)_20250508_69411.csv", fileEncoding = "cp949")
d2015 <- read.csv("D://시계열프로젝트//csv//2015_분기자료(1990~2016)_20250508_51219.csv", fileEncoding = "cp949")
d2016 <- read.csv("D://시계열프로젝트//csv//2016_분기자료(1990~2016)_20250508_51219.csv", fileEncoding = "cp949")
d2019 <- read.csv("D://시계열프로젝트//csv//2019_분기자료(2019~) - 비농림어가_1인이상_20250515_13234.csv", fileEncoding = "cp949")
d2020 <- read.csv("D://시계열프로젝트//csv//2020_분기자료(2019~) - 비농림어가_1인이상_20250515_13234.csv", fileEncoding = "cp949")
d2021 <- read.csv("D://시계열프로젝트//csv//2021_분기자료(2019~) - 비농림어가_1인이상_20250515_52986.csv", fileEncoding = "cp949")
d2022 <- read.csv("D://시계열프로젝트//csv//2022_분기자료(2019~) - 비농림어가_1인이상_20250515_52986.csv", fileEncoding = "cp949")
d2023 <- read.csv("D://시계열프로젝트//csv//2023_분기자료(2019~) - 비농림어가_1인이상_20250515_25951.csv", fileEncoding = "cp949")
d2024 <- read.csv("D://시계열프로젝트//csv//2023_분기자료(2019~) - 비농림어가_1인이상_20250515_25951.csv", fileEncoding = "cp949")

# 함수를 적용하도록 필요한 열의 이름들을 통일 
# 1990 - 2016을 통일: '조사월.분기.', '가구주.연령','가구원2_연령', ..., '가구원8_연령' , '소득', '학원및보습교육.성인미포함.', '성인학원교육'
names(d2010)[names(d2010) == '조사월'] <- '조사월.분기.'
names(d2011)[names(d2011) == '조사월'] <- '조사월.분기.'
names(d2012)[names(d2012) == '조사월'] <- '조사월.분기.'
names(d2013)[names(d2013) == '조사월'] <- '조사월.분기.'
names(d2014)[names(d2014) == '조사월'] <- '조사월.분기.'; names(d2014)[names(d2014) == '학원.및.보습교육.성인미포함.'] <- '학원및보습교육.성인미포함.'
names(d2015)[names(d2015) == '조사분기코드'] <- '조사월.분기.'; names(d2015)[names(d2015) == '가구주_연령'] <- '가구주.연령'; names(d2015)[names(d2015) == '가계지출_소비지출_교육_학원보습.성인미포함.교육비'] <- '학원및보습교육.성인미포함.'; names(d2015)[names(d2015) == '가계지출_소비지출_교육_성인학원비'] <- '성인학원교육'
names(d2016)[names(d2016) == '조사분기코드'] <- '조사월.분기.'; names(d2016)[names(d2016) == '가구주_연령'] <- '가구주.연령'; names(d2016)[names(d2016) == '가계지출_소비지출_교육_학원보습.성인미포함.교육비'] <- '학원및보습교육.성인미포함.'; names(d2016)[names(d2016) == '가계지출_소비지출_교육_성인학원비'] <- '성인학원교육'


age <-  c("가구주.연령", "가구원2_연령", "가구원3_연령", "가구원4_연령", "가구원5_연령",
          "가구원6_연령", "가구원7_연령", "가구원8_연령")
sort(unique(unlist(d2001[age]))) 

# 1998년 조사까지는 15세 미만에 대해서 1, 4, 10를 조사  13, 14세는 간헐적으로 조사 
# --> 다시 확인해보니, 가구주에 대해서는 조사가 구체적으로 이루어졌지만, 가구원의 나이는 미성년자는 1, 4, 10, 17세로만 표기 
# 초등학교 입학은 만나이로는 6세 --> 6세 이상을 기준으로
# 1998년까지는 10, 17세의 자녀를 선택, 1999년부터는 6세 이상의 자녀를 선택 
# 학생 나이의 상한은 만 18세 이하로 지정 
# 만 6세이상, 만 18세 이하 / 만 15세 이상, 만 18세 이하 - 두개의 기준으로 학생수를 계산 
# 추가로 가구당 교육비를 소득으로 나눠서 소득 대비 교육비 지출을 계산 


# 양식을 통일시킨 1990-2016 자료의 정리 
data_modify <- function(input) {
  input_name <- paste0('d', input)
  output_name <- paste0('data', input)
  df <- get(input_name)
  
  # 가구당 학생수를 계산 
  df <- df %>%
    rowwise() %>%
    mutate(
      학생수_초등포함 = sum(c_across(c(`가구주.연령`, `가구원2_연령`, `가구원3_연령`, `가구원4_연령`,
                                `가구원5_연령`, `가구원6_연령`, `가구원7_연령`, `가구원8_연령`)) %in% 6:18, na.rm = TRUE),
      학생수_고등 = sum(c_across(c(`가구주.연령`, `가구원2_연령`, `가구원3_연령`, `가구원4_연령`,
                              `가구원5_연령`, `가구원6_연령`, `가구원7_연령`, `가구원8_연령`)) %in% 15:18, na.rm = TRUE)
    ) %>%
    ungroup()
  # 초등포함, 초등제외한 학생수 당 교육비, 소득대비 교육비 지출액(인원당으로 계산하지않음)을 계산
  df <- df %>%
    mutate(
      `1인당_교육비_초등` = ifelse(학생수_초등포함 > 0, `학원및보습교육.성인미포함.` / 학생수_초등포함, NA),
      `1인당_교육비_고등` = ifelse(학생수_고등 > 0, `학원및보습교육.성인미포함.` / 학생수_고등, NA),
      `소득대비교육비` = ifelse(소득 > 0, `학원및보습교육.성인미포함.` / 소득, NA)
    )
  
  # 각 분기별로 가구 평균값을 계산 
  df_new <- df %>%
    filter(가구원수 != 1) %>% group_by(`조사월.분기.`) %>% 
    summarise(student = mean(`학원및보습교육.성인미포함.`, na.rm = TRUE),
              adult = mean(`성인학원교육`, na.rm = TRUE),
              perstudent_elementary = mean(`1인당_교육비_초등`, na.rm = TRUE),
              perstudent_high = mean(`1인당_교육비_고등`, na.rm = TRUE),
              cost_per_income = mean(`소득대비교육비`, na.rm = TRUE)) %>% 
    mutate(year = input, quarter = floor(`조사월.분기.` / 10)) %>%
    select(-`조사월.분기.`) %>% select(year, quarter, student, adult, perstudent_elementary, perstudent_high, cost_per_income)
  
  assign(output_name, df_new, envir = .GlobalEnv)
}

years <- 1990:2016
lapply(years, data_modify)

data_list <- lapply(years, function(y) get(paste0("data", y)))
data <- bind_rows(data_list)

data

write.csv(data, file = "D://시계열프로젝트//csv//1990-2016data.csv", row.names = FALSE)


### 2019-2024를 정리 
## 2019-2024는 이전 자료와는 다르게 자기들끼리 통일되어있기에, 함수의 열 이름을 수정해서 적용 
# 2019-2024: '조사연월'(6자리), '가구주_연령', ..., '가구원9_연령', '소득 '가계지출_소비지출_교육_학생학원교육비', '가계지출_소비지출_교육_성인학원비'

data_modify_ver2 <- function(input) {
  input_name <- paste0('d', input)
  output_name <- paste0('data', input)
  df <- get(input_name)
  
  # 가구당 학생수를 계산 
  df <- df %>%
    rowwise() %>%
    mutate(
      학생수_초등포함 = sum(c_across(c(`가구주_연령`, `가구원2_연령`, `가구원3_연령`, `가구원4_연령`,
                                `가구원5_연령`, `가구원6_연령`, `가구원7_연령`, `가구원8_연령`, `가구원9_연령`)) %in% 6:18, na.rm = TRUE),
      학생수_고등 = sum(c_across(c(`가구주_연령`, `가구원2_연령`, `가구원3_연령`, `가구원4_연령`,
                              `가구원5_연령`, `가구원6_연령`, `가구원7_연령`, `가구원8_연령`, `가구원9_연령`)) %in% 15:18, na.rm = TRUE)
    ) %>%
    ungroup()
  # 초등포함, 초등제외한 학생수 당 교육비, 소득대비 교육비 지출액(인원당으로 계산하지않음)을 계산
  df <- df %>%
    mutate(
      `1인당_교육비_초등` = ifelse(학생수_초등포함 > 0, `가계지출_소비지출_교육_학생학원교육비` / 학생수_초등포함, NA),
      `1인당_교육비_고등` = ifelse(학생수_고등 > 0, `가계지출_소비지출_교육_학생학원교육비` / 학생수_고등, NA),
      `소득대비교육비` = ifelse(소득 > 0, `가계지출_소비지출_교육_학생학원교육비` / 소득, NA)
    )
  
  # 각 분기별로 가구 평균값을 계산 
  df_new <- df %>%
    filter(가구원수 != 1) %>% group_by(`조사연월`) %>% 
    summarise(student = mean(`가계지출_소비지출_교육_학생학원교육비`, na.rm = TRUE),
              adult = mean(`가계지출_소비지출_교육_성인학원비`, na.rm = TRUE),
              perstudent_elementary = mean(`1인당_교육비_초등`, na.rm = TRUE),
              perstudent_high = mean(`1인당_교육비_고등`, na.rm = TRUE),
              cost_per_income = mean(`소득대비교육비`, na.rm = TRUE)) %>% 
    mutate(year = input, quarter = substr(`조사연월`, 5, 5)) %>%
    select(-`조사연월`) %>% select(year, quarter, student, adult, perstudent_elementary, perstudent_high, cost_per_income)
  
  assign(output_name, df_new, envir = .GlobalEnv)
}


years2 <- 2019:2024
lapply(years2, data_modify_ver2)

data_list2 <- lapply(years2, function(y) get(paste0("data", y)))
data2 <- bind_rows(data_list2)

data2

write.csv(data2, file = "D://시계열프로젝트//csv//2019-2024data.csv", row.names = FALSE)


