# 肝疾患による死亡率をプロットしてみる


# パッケージの準備
require(tidyverse)
# 初回のみ、次の１行を生かして実行を
# install.packages("plotly")
require(plotly)


# データを読み込む
liver_orig <- read_csv("../data/liver_pop_R.csv")

# データの加工
liver <- liver_orig %>%
  select(code5,
         prefecture,
         municipality,
         rate_M,
         rate_F,
         population_M,
         population_F,
         number_M,
         number_F,
         probability_M,
         probability_F)
liver <- liver %>%
  mutate(population = population_M + population_F)

# 「千三つ」つまり3σ相当(ただしここでは片側だが)を洗い出して、外れ値ならマーク
liver <- liver %>%
  mutate(MALE = (probability_M < 0.003))
liver <- liver %>%
  mutate(FEMALE = (probability_F < 0.003))
liver <- liver %>%
  mutate(check = case_when(
     MALE &  FEMALE ~ "BOTH HIGH",
     MALE & !FEMALE ~ "MALE HIGH",
    !MALE &  FEMALE ~ "FEMALE HIGH",
    !MALE & !FEMALE ~ "POSSIBLE")
  )

# 都道府県か市区町村かのフラグ
liver <- liver %>%
  mutate(is_prefecture = case_when(
     is.na(municipality) ~ "都道府県",
    !is.na(municipality) ~ "市区町村")
  )
# 都道府県の場合は全体の平均だと表示を補う
liver <- liver %>%
  mutate(municipality = replace_na(municipality, "全体の平均"))

# 散布図を描く。ただし、ggplot2ではなく、plotlyにした。マウスホーバーで自治体名やデータがポップアップするようにしたいので
plt_MF <- plot_ly(
  data = liver,
  alpha = 0.4,
  x = ~rate_M,
  y = ~rate_F,
  type = "scatter",
  mode = "markers",
  symbol = ~is_prefecture,
  symbols = c("pentagon", "cross"),
# ポップアップを表示する準備
  text = ~paste0(
    prefecture,
    municipality,
    "\n男性死亡率：",
    rate_M,
    "\n女性死亡率：",
    rate_F,
    "\n人口総計：",
    population,
    "\nうち男性：",
    population_M,
    "\nうち女性：",
    population_F
  ),
  hoverinfo = 'text',
  color = ~check
) %>%
  add_trace(x = c(0, 500), y = c(100, 100), mode = "lines", line = list(color = "#888888", width = 2, dash = "dot"), inherit = FALSE, showlegend = FALSE) %>%
  add_trace(x = c(100, 100), y = c(0, 500), mode = "lines", line = list(dash = "dot", color = "#888888", width = 2),  inherit = FALSE, showlegend = FALSE) %>%
  layout(title = "■肝疾患による男女の死亡率")
print(plt_MF)

# ファイルを保存。working directoryに
htmlwidgets::saveWidget(config(plt_MF, showLink = FALSE, collaborate = FALSE, displayModeBar = FALSE), "liver3)MF_R0922.html")
