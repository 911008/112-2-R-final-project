# 安裝並加載dplyr套件 -----
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
library(dplyr)

# 移除月欄位並垂直合併相同年別的資料 -----
high <- high %>%
  select(-月) %>%
  group_by(年) %>%
  summarize(旅客人數 = sum(旅客人數, na.rm = TRUE)) %>%
  ungroup()

# 修改欄位名稱並調整年別 -----
high <- high %>%
  rename(年別 = 年, 高鐵 = 旅客人數) %>%
  mutate(年別 = 年別 + 1911)

# 保留2015-2022年的資料 -----
high <- high %>%
  filter(年別 >= 2015 & 年別 <= 2022)

# 創建HSR數據框 -----
HSR <- high %>%
  select(年別, 高鐵)

# 檢視HSR數據框的前三行 -----
glimpse(HSR)

# 創立merge的新檔案並水平合併 -----
merge <- train %>%
  left_join(HSR, by = "年別")

# 依年別排列 -----
merge <- merge %>%
  arrange(年別)

# 創立台鐵新欄位 -----
merge <- merge %>%
  mutate(台鐵 = 自強號 + 莒光號 + 區間列車 + 普通車)

# 創立比值(台鐵/高鐵)新欄位 -----
merge <- merge %>%
  mutate(`比值(台鐵/高鐵)` = 台鐵 / 高鐵)

# 檢視merge數據框的前三行 -----
glimpse(merge)

# 安裝並加載ggplot2套件 -----
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)

# 創建折線圖 -----
ggplot(data = merge, aes(x = 年別)) +
  geom_line(aes(y = 自強號, color = "自強號")) +
  geom_line(aes(y = 莒光號, color = "莒光號")) +
  geom_line(aes(y = 區間列車, color = "區間列車")) +
  geom_line(aes(y = 普通車, color = "普通車")) +
  geom_line(aes(y = 高鐵, color = "高鐵")) +
  geom_line(aes(y = 台鐵, color = "台鐵")) +
  labs(title = "年別 vs 各列車客流量",
       x = "年別",
       y = "客流量",
       color = "列車種類") +
  theme_minimal()


# 讀取GDP檔案 -----
GDP <- read.csv("GDP.csv")

# 將年度欄位改為年別 -----
GDP$年別 <- as.numeric(substring(GDP$年度, 1, 4))
GDP$年度 <- NULL

# 只擷取2015到2022年的資料 -----
GDP <- GDP[GDP$年別 >= 2015 & GDP$年別 <= 2022, ]

# 平行轉移到新創檔案data -----
data <- data.frame(年別 = GDP$年別, 國內生產毛額 = GDP$國內生產毛額.億元)

# 以年別為依據，水平合併data和merge至名為final的新創檔案 -----
final <- merge(data, merge, by = "年別", all = TRUE)

  scale_color_manual(values = c("國內生產毛額" = "blue", "台鐵" = "red", "高鐵" = "green")) +
 

    library(ggplot2)
  
  # 將final中2015-2022的國內生產毛額以折線圖表示 -----
  ggplot(final, aes(x = 年別, y = 國內生產毛額)) +
    geom_line() +
    labs(title = "國內生產毛額趨勢", x = "年別", y = "國內生產毛額(億元)")

  # 計算國內生產毛額、台鐵、高鐵的年成長率 -----
  end <- final %>%
    mutate(國內生產毛額成長率 = (國內生產毛額 - lag(國內生產毛額, default = first(國內生產毛額))) / lag(國內生產毛額, default = first(國內生產毛額)),
           台鐵成長率 = (台鐵 - lag(台鐵, default = first(台鐵))) / lag(台鐵, default = first(台鐵)),
           高鐵成長率 = (高鐵 - lag(高鐵, default = first(高鐵))) / lag(高鐵, default = first(高鐵)))
  
  # 匯入至名為end的新創檔案 -----
  write.csv(end, "end.csv", row.names = FALSE)
  
  # 將end中2015-2022年的國內生產毛額成長率、台鐵成長率、高鐵成長率的三個欄位的數值繪製成折線圖 -----
  ggplot(end, aes(x = 年別, y = 國內生產毛額成長率)) +
    geom_line(color = "blue") +
    geom_line(aes(y = 台鐵成長率), color = "red") +
    geom_line(aes(y = 高鐵成長率), color = "green") +
    labs(title = "成長率趨勢", x = "年別", y = "成長率") +
    scale_y_continuous(labels = scales::percent)
  
  