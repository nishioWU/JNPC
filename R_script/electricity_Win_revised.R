# 消費電力量と天候の関係はどうなっているか
# 西暦年をいちいち指定しなくても動作するようにリファクタリング。2016年とか15年のデータがフォルダにあれば、それも一括して読み込んで結合する。凡例での年の並び順も、手作業でなく自動化
# 休日も、各年分を与えなくて済むようにした
# 回帰直線の係数も、各年の一覧を出せるようにした
# このパッケージは休日判定用
# install.packages("Nippon")

# 使うパッケージの準備
require(tidyverse)
require(lubridate)

# フォルダ内にある気象庁データを読み込む。climateに数字が0~6桁ついているCSVを想定
climate_files = fs::dir_ls("../data", regexp = "climate\\d{0,6}") # fsの関数
# climate_files
climate <- map_dfr(climate_files, read_csv) # purrrの関数
# View(climate)
# 文字列として読み込まれたので、日付時刻型に変換しておく
climate$date_hour <- climate$date_hour %>% as.POSIXct(tz = "Asia/Tokyo")
# str(climate)
# View(climate)
# precipitationは降水量だが、0と0.0がある。前者は降水なし（no_precipitation_dummyの値が1）。後者は数字に現れない降水（ダミー変数の値が0）。TRUEかFALSEかのlogical型に変換しておく
climate$no_precipitation_dummy <- (climate$no_precipitation_dummy == 1)

# 曜日と、営業日かどうかの情報を追加
climate <- climate %>% mutate(day_of_the_week = wday(date_hour))
climate <- climate %>% mutate(daytime = (hour(date_hour) >= 7 & hour(date_hour) <= 19))
climate <- climate %>% mutate(days_off = (day_of_the_week %in% c(1, 7) | Nippon::is.jholiday(date(date_hour))))
# 1が日曜日、7が土曜日のこと
# str(climate)

# TEPCOデータも同様に読み込む。７月と８月の需要データ。単位は万kw
electricity_files = fs::dir_ls("../data", regexp = "electricity\\d{0,6}") # fsの関数
electricity <- map_dfr(electricity_files, read_csv) # purrrの関数
# View(electricity)
electricity$DATE <- electricity$DATE %>% as.Date(tz = "Asia/Tokyo")
# tail(electricity)
# 迷うところだが、天候（とくに降水量）と消費電力量を突き合わせるにあたり、１時間ずらすかどうか。ここでは、ずらしていない
# タイムゾーンをここにも追加して、表示も揃うようにした
electricity <- electricity %>% mutate(date_hour = as.POSIXct(paste(DATE, TIME), tz = "Asia/Tokyo")) #+ hms("01:00:00"))
# ７月と８月分だけに絞る
electricity <- electricity %>% filter(month(date_hour) == 7 | month(date_hour) == 8)

# hour_date列をキーにしてJOIN
joined_table <- electricity %>% inner_join(climate, by = "date_hour")
joined_table <- joined_table %>% mutate(year = year(date_hour))
# str(joined_table)
# View(joined_table)
# 年を取り出して昇順にする
year_levels <- joined_table %>%
  select(year) %>%
  distinct() %>%
  arrange() %>% # これは昇順
  pull() %>%  # １列だけのtibbleをベクトルに変換
  rev() # 新しい順に

# 数値なので文字列型に変換。でないと、並び順指定に使えないようだ
year_levels <- as.character(year_levels)
# この順番に並び順を指定。色分けしやすいよう、factorに変換する
joined_table$year <- factor(joined_table$year, levels = fct_relevel(year_levels))

# 平日（休日の否定）で日中だけを取り出す
plot_data <- joined_table %>% filter(daytime, !days_off)

# str(plot_data)
# 散布図を描く
# 最初の１行はMacでggplot2を使うときの文字化けを防ぐもの
# old = theme_set(theme_gray(base_family="HiraKakuProN-W3")) この１行はWindowsでは不要
# theme_set(theme_gray(base_family="")) 設定を元に戻すときに使う
plt <- ggplot(data = plot_data, mapping = aes(x = temperature, y = power_consumption, color = year, shape = no_precipitation_dummy, group = year))
plt <- plt + ggtitle("■気温と電力需要：７～８月の平日昼間について調べてみる")
plt <- plt + scale_x_continuous(name = "東京の気温（℃）", breaks = seq(15, 40, 5))
plt <- plt + scale_y_continuous(name = "東京電力の電力需要（万kw）", breaks = (seq(2000, 5000, 1000)))
plt <- plt + theme(legend.position = c(1, 0), legend.justification = c(1, -.1)) +
# 凡例の順番を指定
  guides(color = guide_legend(order = 1), shape = guide_legend(order = 2)) +
  scale_shape_discrete(labels = c("あり", "なし")) +
  labs(color = "年", shape = "降水")
plt <- plt + geom_point(alpha = .5)
plt <- plt + geom_smooth(method = "lm", se = FALSE)
plt <- plt + coord_cartesian(xlim = c(17, 38), ylim = c(2400, 6000))
# plt <- plt + coord_cartesian(xlim = c(15, 40), ylim = c(2000, 6000))
plt <- plt + labs(caption = "出典：気象庁、東京電力")
print(plt)

# 下と同じことだが、手動で指定しないで、回帰直線の係数を表示できるようにした
# 年を一緒に見せたいのだが、こんな苦しい方法でよいのか……
lm_output <-
  plot_data %>%
    group_by(year) %>%
    arrange(year) %>%
    nest() %>%
      transmute(year = year, lm_coefficients = map(data, function(data_inside){
        broom::tidy(lm(power_consumption ~ temperature, data = data_inside))
      }
    )) %>%
    unnest()
# lm_output
View(lm_output)

# これだと、年が降順にならない
# lm_output <-
#   plot_data %>%
#   nest(-year) %>%
#     transmute(year = year, lm_coefficients = map(data, function(data_inside){
#       broom::tidy(lm(power_consumption ~ temperature, data = data_inside))
#     }
#     )) %>%
#   unnest()
# lm_output
# View(lm_output)

# 回帰直線の係数を調べる。Excelと違うのはなぜ？
# lm_data_2018 <- plot_data %>% filter(year == 2018)
# lm(power_consumption ~ temperature, data = lm_data_2018)
# lm_data_2017 <- plot_data %>% filter(year == 2017)
# lm(power_consumption ~ temperature, data = lm_data_2017)
# lm_data_2011 <- plot_data %>% filter(year == 2011)
# lm(power_consumption ~ temperature, data = lm_data_2011)
# 祝日をちゃんと除外して計算したからだった。祝日を考慮しないで月～金曜日だけを抜き出すと、Excelと同じ結果になった

# 画像をファイルに保存

# Windowsの場合はこれでもOK
ggsave('../outcome/electricity_Win_revised', plt, width = 297, height = 210, units = 'mm')

# Macでも文字化けしないようCairoを使う。A４判サイズをインチで指定
# cairo_pdf("../outcome/electricity_Mac_revised.pdf", width = 11.69, height = 8.27, family = "Japan1GothicBBB")
# print(plt)
# dev.off()

# Macの場合、以下を生かしてメニューから保存するときれい
# quartz()
# print(plt)
