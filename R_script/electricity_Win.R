# 消費電力量と天候の関係はどうなっているか

# 使うパッケージの準備
require(tidyverse)
require(lubridate)


# 分析期間中の祝日一覧。海の日は2011年にはなかった
holidays = as.Date(c("2011-07-18", "2017-07-17", "2017-08-11", "2018-07-16", "2018-08-11"), tz = "Asia/Tokyo")

# 気象庁データを読み込む。７月と８月の天候データ
climate17 <- read_csv("../data/climate17.csv"
# S-JISで読み込むなら以下のコメント行の#を削って
  #, locale = locale(encoding = 'CP932')
)
climate18 <- read_csv("../data/climate18.csv")
climate11 <- read_csv("../data/climate11.csv")

# 一括処理したいので、３つのファイルを縦に連結する
climate <- bind_rows(climate11, climate17, climate18)

# View(climate)
# 文字列として読み込まれたので、日付時刻型に変換しておく
climate$date_hour <- climate$date_hour %>% as.POSIXct(tz = "Asia/Tokyo")
# str(climate)

# precipitationは降水量だが、0と0.0がある。前者は降水なし（no_precipitation_dummyの値が1）。後者は数字に現れない降水（ダミー変数の値が0）。TRUEかFALSEかのlogical型に変換しておく
climate$no_precipitation_dummy <- (climate$no_precipitation_dummy == 1)

# 曜日と、営業日かどうかの情報を追加
climate <- climate %>% mutate(day_of_the_week = wday(date_hour))
climate <- climate %>% mutate(daytime = (hour(date_hour) >= 7 & hour(date_hour) <= 19))
climate <- climate %>% mutate(days_off = (day_of_the_week %in% c(1, 7)) | date(date_hour) %in% holidays)
# 1が日曜日、7が土曜日のこと
# str(climate)

# TEPCOデータを読み込む。７月と８月の需要データ。単位は万kw
electricity17 <- read_csv("../data/electricity17.csv"
# S-JISで読み込むなら以下のコメント行の#を削って
  #, locale = locale(encoding = 'CP932')
)
electricity11 <- read_csv("../data/electricity11.csv")
electricity18 <- read_csv("../data/electricity18.csv")
electricity <- bind_rows(electricity11, electricity17, electricity18)
# View(electricity)
electricity$DATE <- electricity$DATE %>% as.Date(tz = "Asia/Tokyo")
# tail(electricity)
# 迷うところだが、天候（とくに降水量）と消費電力量を突き合わせるにあたり、１時間ずらすかどうか。ここでは、ずらしていない
electricity <- electricity %>% mutate(date_hour = as.POSIXct(paste(DATE, TIME), tz = "Asia/Tokyo")) #+ hms("01:00:00"))
# ７月と８月分だけに絞る
electricity <- electricity %>% filter(month(date_hour) == 7 | month(date_hour) == 8)

# hour_date列をキーにしてJOIN
joined_table <- electricity %>% inner_join(climate, by = "date_hour")
joined_table <- joined_table %>% mutate(year = year(date_hour))
# str(joined_table)

# 年をカテゴリー変数に型変換した
joined_table$year <- factor(joined_table$year, levels = c("2018", "2017", "2011"))

# 平日（休日の否定）で日中だけを取り出す
plot_data <- joined_table %>% filter(daytime, !days_off)

# 散布図を描く
# 最初の１行はMacでggplot2を使うときの文字化けを防ぐもの
# old = theme_set(theme_gray(base_family="HiraKakuProN-W3")) この１行はWindowsでは不要
# theme_set(theme_gray(base_family="")) 設定を元に戻すときに使う
plt <- ggplot(data = plot_data, mapping = aes(x = temperature, y = power_consumption, color = year, shape = no_precipitation_dummy, group = year))
plt <- plt + ggtitle("■気温と電力需要：７～８月の平日昼間について調べてみる")
plt <- plt + scale_x_continuous(name = "東京の気温（℃）", breaks = seq(15, 40, 5))
plt <- plt + scale_y_continuous(name = "東京電力の電力需要（万kw）", breaks = (seq(2000, 5000, 1000)))
plt <- plt + theme(legend.position = c(1, 0), legend.justification = c(1, -.1)) +
guides(color = guide_legend(order = 1), shape = guide_legend(order = 2)) +
 scale_shape_discrete(labels = c("あり", "なし")) +
 labs(color = "年", shape = "降水")
plt <- plt + geom_point(alpha = .5)
plt <- plt + geom_smooth(method = "lm", se = FALSE)
plt <- plt + coord_cartesian(xlim = c(17, 38), ylim = c(2400, 6000))
# plt <- plt + coord_cartesian(xlim = c(15, 40), ylim = c(2000, 6000))
plt <- plt + labs(caption = "出典：気象庁、東京電力")
print(plt)

# 回帰直線の係数を調べる。Excelと違うのはなぜ？
lm_data_2018 <- plot_data %>% filter(year == 2018)
lm(power_consumption ~ temperature, data = lm_data_2018)
lm_data_2017 <- plot_data %>% filter(year == 2017)
lm(power_consumption ~ temperature, data = lm_data_2017)
lm_data_2011 <- plot_data %>% filter(year == 2011)
lm(power_consumption ~ temperature, data = lm_data_2011)
# 祝日をちゃんと除外して計算したからだった。33行目の後段をコメントアウトすると、同じ結果になる

# 画像をファイルに保存

# Windowsの場合はこれでもOK
ggsave('../outcome/electricity_0922_win1.pdf', plt, width = 297, height = 210, units = 'mm')

# Macでも文字化けしないようCairoを使う。A４判サイズをインチで指定
# cairo_pdf("../outcome/electricity_0922_win2.pdf", width = 11.69, height = 8.27, family = "Japan1GothicBBB")
# print(plt)
# dev.off()

# Macの場合、以下を生かしてメニューから保存するときれい
# quartz()
# print(plt)
