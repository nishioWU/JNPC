# 石田基広先生のgithubから.RProfileを複写するコード
# ただし、作る場所をユーザーのホームではなく、working directoryに変更している
# プロジェクトを開いていれば、このフォルダのはず。getwd()で確かめられる
# このプロジェクトのフォルダに.Rprofileがあったら、タイムスタンプをファイル名に足してバックアップ。その上で、先生の設定をダウンロードする
getwd()

if (file.exists(".Rprofile")) {
  oldRprofile  <- paste("dot.Rprofile.", format(Sys.time(), "%Y_%m-%d_%H-%M"), ".txt", sep = "")
  file.copy(from = ".Rprofile",  to = oldRprofile)
}

download.file("http://rmecab.jp/R/dot.Rprofile.txt", dest = ".Rprofile")

source (".Rprofile")

cat("日本語フォントの文字化けを極力防ぐ設定ファイル「.Rprofile」を作成しました。\nRを一度終了して、起動しなおして下さい。")

# ".Rprofile"というファイルを作った。デフォルトでは見えない不可視ファイル
