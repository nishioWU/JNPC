# 喫茶店データをRで扱ってみる
# ２つのデータを、都道府県名をキーに結合する例

# 初回のみ、以下２行の#を外して実行を
# install.packages("tidyverse")
# install.packages("scales")
# scalesは対数軸を使うためにインストール

require(tidyverse)
require(scales)

# ３チェーンのデータを読む。ブロック名つき
dataset <- read_csv("../data/cafe_area.csv")
# ダイアログで選ぶならこちら
# dataset <- read_csv(file.choose())
# S-JISなら、以下を補って文字化けを防ぐ , locale = locale(encoding = 'CP932')

# データの先頭部分を見る
head(dataset)
# colnames(dataset) <- c('code', 'area', 'prefecture', 'population', 'houshold','starbucks', 'doutor', 'komeda')
# エリアを分かりやすい順番に。適宜まとめる
dataset$area <- as.factor(dataset$area)
levels(dataset$area)
dataset$area <- fct_collapse(dataset$area,
  '北海道・東北' = c('北海道', '東北'),
  '甲信越・北陸' = c('甲信越', '北陸'),
  '九州・沖縄'   = c('九州', '沖縄')
)
dataset$area <- fct_inorder(dataset$area)
levels(dataset$area)
# 合計は取り除く
dataset <- dataset %>% filter(prefecture != '合計')

# 散布図を描く
# 人口×スタバ
plt <- ggplot(data = dataset, mapping = aes(x = population, y = starbucks, color = area)) +
  geom_point() + scale_x_log10(labels = trans_format("log10", math_format(10^.x))) + scale_y_log10() +
  labs(title = '■都道府県の人口とスタバの店舗数')
plt

# ３チェーン
plt <- ggplot(data = dataset, mapping = aes(x = population)) +
  geom_point(aes(y = starbucks), color = "#00740A") +
  labs(title = '■S-K-D店舗数') + theme(axis.title.y = element_blank())
plt <- plt + geom_point(aes(y = komeda), color = "#F27500")
plt <- plt + geom_point(aes(y = doutor), color = "#FFE14D")
plt

# X軸を対数軸にしてみる
plt <- ggplot(data = dataset, mapping = aes(x = population)) +
  geom_point(aes(y = starbucks), color = "#00740A") + scale_x_log10(labels = trans_format("log10", math_format(10^.x))) + coord_cartesian(xlim = c(10^5, 10^6) ,ylim = c(0, 100)) +
  labs(title = '■S-K-D店舗数 log-axis') + theme(axis.title.y = element_blank())
plt <- plt + geom_point(aes(y = komeda), color = "#F27500")
plt <- plt + geom_point(aes(y = doutor), color = "#FFE14D")
plt

# Y軸も修正
plt <- ggplot(data = dataset, mapping = aes(x = population)) +
  geom_point(aes(y = starbucks), color = "#00740A") + scale_x_log10(labels = trans_format("log10", math_format(10^.x))) + scale_y_log10() +
  labs(title = '■S-K-D店舗数 log-axis') + theme(axis.title.y = element_blank())
plt <- plt + geom_point(aes(y = komeda), color = "#F27500")
plt <- plt + geom_point(aes(y = doutor), color = "#FFE14D")
plt

# スタバ×ドトール
plt <- ggplot(data = dataset, mapping = aes(x = doutor, y = starbucks, color = area)) +
  geom_point() +
  labs(title = '■スタバとドトールの店舗数')
# plt <- plt + xlim(10, 300) + ylim(10, 300)
plt

# スタバ×コメダ
plt <- ggplot(data = dataset, mapping = aes(x = komeda, y = starbucks, color = area)) +
  geom_point() +
  labs(title = '■スタバとコメダの店舗数')
# plt <- plt + xlim(10, 300) + ylim(10, 300)
plt

# コメダ×ドトール
plt <- ggplot(data = dataset, mapping = aes(x = komeda, y = doutor, color = area)) +
  geom_point() +
  labs(title = '■コメダとドトールの店舗数')
# plt <- plt + xlim(10, 300) + ylim(10, 300)
plt

# 人口10万人あたりにしてみる
dataset2 <- dataset %>% mutate(starbucks_per_pop = starbucks / population * 100000, komeda_per_pop = komeda / population * 100000, doutor_per_pop = doutor / population * 100000)
# 念のため確認
head(dataset2)

# 散布図を描く
# スタバ×コメダ
# ○の大きさは人口に対応
plt <- ggplot(data = dataset2, mapping = aes(x = komeda_per_pop, y = starbucks_per_pop, color = area)) +
  geom_point(aes(size = population)) +
  coord_fixed(ratio = 1L) + scale_x_continuous(breaks = seq(0,3)) + scale_y_continuous(breaks = seq(0,3)) +
  labs(title = '■スタバな県vs.コメダな県', x = '人口10万人あたりのコメダ店舗数', y = 'スタバ店舗数')
plt <- plt + geom_text(aes(label = prefecture),
  angle = 20,
  color = 'black', size = 3) +
  theme(legend.position = 'none')
plt

# 経済センサスのデータを読み込む
keizai_census <- read_csv("../data/keizai_census.csv")
# keizai_census <- read_csv(file.choose())
head(keizai_census)

# 県名で照合してJOIN
cafe_joined <- dataset2 %>% left_join(keizai_census, by = 'prefecture')

# 見てみる。県ごとの喫茶店数が加わった
View(cafe_joined)

# これも人口比に直してみる
cafe_joined <- cafe_joined %>% mutate(cafe_per_pop = n_of_cafe / population)

# 散布図を描く。○の大きさは、人口あたりの喫茶店数に対応。でも見にくい
plt <- ggplot(data = cafe_joined, mapping = aes(x = komeda_per_pop, y = doutor_per_pop, color = area)) +
  geom_point(aes(size = cafe_per_pop)) +
  coord_fixed(ratio = 1L) + scale_x_continuous(breaks = seq(0,3)) + scale_y_continuous(breaks = seq(0,3)) +
  labs(title = '■黄色かオレンジか', x = '人口10万人あたりのコメダ店舗数', y = 'ドトール店舗数') +
  geom_text(aes(label = prefecture),
  angle = 20, color = 'black', size = 4) +
  theme(legend.position = 'none')
plt

# ファイルに保存
ggsave("../outcome/cafe_0922.pdf", width = 420, height = 297, units = "mm")
