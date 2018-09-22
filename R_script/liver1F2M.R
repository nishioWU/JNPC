# 肝疾患による死亡率をプロットしてみる


# パッケージの準備
require(tidyverse)
# 最初の１回のみインストールが必要
# install.packages("plotly")
require(plotly)

# データを読み込む
liver_orig <- read_csv("../data/liver_pop_R.csv")
# 男性だけのデータ
liver_M <- liver_orig %>%
  select(code5, municipality,
         rate = rate_M,
         population = population_M,
         number = number_M,
         probability = probability_M)
liver_M <- liver_M %>%
  mutate(sex = "M")
# 女性だけのデータ
liver_F <- liver_orig %>%
  select(code5, municipality,
         rate = rate_F,
         population = population_F,
         number = number_F,
         probability = probability_F)
liver_F <- liver_F %>%
  mutate(sex = "F")
liver <- bind_rows(liver_M, liver_F)
liver <- liver %>%
  mutate(check = (probability < 0.003))

# 散布図を描く
# 外れ値ならマーク
liver_2 <- liver %>% mutate(check = case_when(
  check == TRUE ~ "TOO HIGH",
  check == FALSE ~ "POSSIBLE")
)
# 男女に分ける
liver_M2 <- liver_2 %>%
  filter(sex == "M")
liver_F2 <- liver_2 %>%
  filter(sex == "F")

plt_M <- plot_ly(liver_M2, x = ~population, y = ~rate, mode = "markers", color = ~check, text = ~paste(municipality,"\n調整ずみ死亡率：", rate, "\n人口：", population, "\n実際の死亡者数：", number), hoverinfo = "text", marker = list(size = 5, opacity = 0.6), type = "scatter") %>%
  layout(title = "【男性】肝疾患による死亡率と自治体人口")

# ファイルを保存
# 別のフォルダに出力すると、いろいろ厄介だと判明したので、working directory に
htmlwidgets::saveWidget(config(plt_M, showLink = FALSE, collaborate = FALSE, displayModeBar = FALSE), "liver2M_0922.html")
print(plt_M)

plt_F <- plot_ly(liver_F2, x = ~population, y = ~rate, mode = "markers", color = ~check, text = ~paste(municipality,"\n調整ずみ死亡率：", rate, "\n人口：", population, "\n実際の死亡者数：", number), hoverinfo = "text", marker = list(size = 5, opacity = 0.6), type = "scatter") %>%
  layout(title = "【女性】肝疾患による死亡率と自治体人口")
print(plt_F)

htmlwidgets::saveWidget(config(plt_F, showLink = FALSE, collaborate = FALSE, displayModeBar = FALSE), "liver1F_0922.html")

