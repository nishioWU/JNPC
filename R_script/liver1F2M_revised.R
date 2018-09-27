# 肝疾患による死亡率をプロットしてみる
# 確率の計算もRでやるように書き直し

# パッケージの準備
require(tidyverse)
# 最初の１回のみインストールが必要
# install.packages("plotly")
require(plotly)

# データを読み込む
liver_orig <- read_csv("../data/liver_pop_R.csv")
# 男性だけのデータ
liver_M <- liver_orig %>%
  select(code5, prefecture, municipality,
         rate = rate_M,
         population = population_M,
         number = number_M,
         probability = probability_M)
liver_M <- liver_M %>%
  mutate(sex = "M")

# この１行を追加。都道府県（市区町村名が空欄）だけを取り出してダブりのないよう合計。仮にNA値があれば取り除く
total_M <- liver_M %>%
  filter(is.na(municipality)) %>%
  summarize(total_population_M = sum(population), total_number_M = sum(number, na.rm = TRUE), avg_rate_M = total_number_M / total_population_M)

# 以下も追加
# 全国平均の死亡率だと仮定した場合、その自治体の人口で、同等以上に高い値が観測される確率を算定。二項分布を想定した
# 自治体の年齢構成が分からないので、人口×死亡率で死者数を概算。ここが年齢調整値にならないので、参考程度だが、大外れはしていない感じ
liver_M <- liver_M %>%
  mutate(
    estimated_number_R = population * rate * total_M$avg_rate_M / 100,
    probability_R = 1 - pbinom(q = estimated_number_R, size = population, prob = total_M$avg_rate_M)
  )

# 女性だけのデータ
liver_F <- liver_orig %>%
  select(code5, prefecture, municipality,
         rate = rate_F,
         population = population_F,
         number = number_F,
         probability = probability_F)
liver_F <- liver_F %>%
  mutate(sex = "F")

# この１行を追加。都道府県だけを取り出してダブりのないよう合計。仮にNA値があれば取り除く
total_F <- liver_F %>%
  filter(is.na(municipality)) %>%
  summarize(total_population_F = sum(population), total_number_F = sum(number, na.rm = TRUE), avg_rate_F = total_number_F / total_population_F)

# 以下も追加
liver_F <- liver_F %>%
  mutate(
    estimated_number_R = population * rate * total_F$avg_rate_F / 100,
    probability_R = 1 - pbinom(q = estimated_number_R, size = population, prob = total_F$avg_rate_F)
  )

liver <- bind_rows(liver_M, liver_F)

# 都道府県全体のデータの場合を追加
liver <- liver %>%
  mutate(municipality = replace_na(municipality, ""))

# 千三つ相当をマークする。Rで計算したカラムを使うように切り替えた
liver <- liver %>%
  mutate(check = (probability_R < 0.003))

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

# 都道府県名を追記
plt_M <- plot_ly(liver_M2, x = ~population, y = ~rate, mode = "markers", color = ~check, text = ~paste0(prefecture, municipality,"\n調整ずみ死亡率：", rate, "\n人口：", population, "\n実際の死亡者数：", number), hoverinfo = "text", marker = list(size = 5, opacity = 0.6), type = "scatter") %>%
  layout(title = "【男性】肝疾患による死亡率と自治体人口")

# ファイルを保存
# 別のフォルダに出力すると、いろいろ厄介だと判明したので、working directory に
htmlwidgets::saveWidget(config(plt_M, showLink = FALSE, collaborate = FALSE, displayModeBar = FALSE), "liver2M_revised.html")
print(plt_M)

plt_F <- plot_ly(liver_F2, x = ~population, y = ~rate, mode = "markers", color = ~check, text = ~paste0(prefecture, municipality,"\n調整ずみ死亡率：", rate, "\n人口：", population, "\n実際の死亡者数：", number), hoverinfo = "text", marker = list(size = 5, opacity = 0.6), type = "scatter") %>%
  layout(title = "【女性】肝疾患による死亡率と自治体人口")
print(plt_F)

htmlwidgets::saveWidget(config(plt_F, showLink = FALSE, collaborate = FALSE, displayModeBar = FALSE), "liver1F_revised.html")
print(plt_F)
