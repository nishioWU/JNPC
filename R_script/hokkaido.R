# 北海道の世帯あたり人数

# パッケージの準備
require(tidyverse)
require(plotly)

# 人口データを読み込む
hokkaido_orig <- read_csv("../data/hokkaido.csv")

# 世帯の平均人数を計算する
hokkaido <-
hokkaido_orig %>%
  mutate(persons_per_HH = population_total / houshold)

# ダブルカウントを防ぐ
hokkaido <- hokkaido %>%
  filter(!str_detect(municipality, "の計"))

# 人口密度のデータを読み込む
hokkaido_density <- read_csv("../data/hokkaido_pop_density.csv")
hokkaido_density$density <- as.numeric(hokkaido_density$density)
hokkaido_density$population_2017 <- as.numeric(hokkaido_density$population_2017)
# 自治体名をキー列にして結合する
hokkaido_joined <-
  hokkaido %>% left_join(hokkaido_density, by = "municipality")

# 札幌市の区のアタマに市名がついていなかったらつける
sapporo_city <- str_detect(hokkaido_joined$municipality, pattern = "^(?!札幌市).*区$")
hokkaido_joined$municipality[sapporo_city] <- paste0("札幌市", hokkaido_joined$municipality[sapporo_city])

category <- c("^札幌市", "市$", "[町村]$")
hokkaido_joined <-
  hokkaido_joined %>%
    mutate(category =
      case_when(
        str_detect(municipality, pattern = category[1]) ~ "札幌市",
        str_detect(municipality, pattern = category[2]) ~ "その他の市",
        str_detect(municipality, pattern = category[3]) ~ "町村"
      )
    )
hokkaido_joined$category <-
  as_factor(hokkaido_joined$category)
hokkaido_joined$category <-
  fct_relevel(hokkaido_joined$category, c("札幌市", "その他の市", "町村"))
# View(hokkaido_joined)

plt_hokkaido <- plot_ly(
  data = hokkaido_joined,
  x = ~density,
  y = ~persons_per_HH,
  alpha = 0.5,
  type = "scatter",
  mode = "markers",
  symbol = ~category,
  symbols = c("pentagon", "circle", "cross"),
  text = ~paste0(
    municipality,
    "\n人口：",
    population_total,
    "\n人口密度：",
    density,
    "\n世帯人数：",
    round(persons_per_HH, digits = 2)
  ),

  hoverinfo = "text",
  color = ~category
  ) %>%
  layout(
    title = "北海道の市区町村の世帯人数と人口密度",
    xaxis = list(title = "人口密度（対数軸）", type = "log"),
    yaxis = list(title = "世帯あたり人数")
  )

print(plt_hokkaido)
# ファイルを保存
# 別のフォルダに出力すると、いろいろ厄介だと判明したので、working directory に
htmlwidgets::saveWidget(config(plt_hokkaido, showLink = FALSE, collaborate = FALSE, displayModeBar = FALSE), "hokkaido_R.html")
