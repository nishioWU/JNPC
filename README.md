# JNPC
### 参加ありがとうございます

2018年9月の自主ゼミ用の資料です。右上の｢Clone or download｣から｢Download ZIP｣を選んで下さい。データフォルダのパスワードは事務局からご案内している通りです。
川上さん・田中さんから頂戴した、RとRStudioのインストールガイド（昨年の講座のもの）は、Windows用とMac用に分かれています。RやRStudioバージョンの数字は変わっていますので、最新版に読み替えて、新しいものを入れて下さい。Rは3.5.1、RStudioは1.1.456です。
ご参加いただき、どうもありがとうございます。

---

### Rの初期設定について

「R_script」フォルダにRのスクリプト一式が入っています。データはExcel用と同じdataフォルダから読み込みます。初めてRStudioを起動したときのみ、以下の手順で、このフォルダ用の初期設定を走らせて下さい。すでにRをお使いの方の基本環境には悪さをせずに、このフォルダ内に限って、挙動を揃えます。

1. 「R_script.Rproj」ファイルをダブルクリックして下さい。プロジェクトが設定されます。コードやデータを読み込むときの「現在地」になります
1. 「RStuidio」の右上角に、このプロジェクト名が表示されていればOKです。コードを実行したときに、データの読み込みに失敗するようなら、ここを見て確認して下さい
1. 最初の１度だけ、「make_dot_RProfile_in_WD.R」ファイルを実行して下さい。RStudioの左上、３番目のボタンでファイル選択画面が開くので、そこで選んで下さい。開いたら、真ん中上（左上のエディターペインの右上あたり）の「Source」ボタンを押す
1. Rをいったん終了し、再起動して下さい

２回目以降は、RStudioのアイコンをクリックするだけで大丈夫です。「R_script.Rproj」ファイルのダブルクリックでも、RStudioが立ち上がります。先ほどの「make_dot_RProfile_in_WD.R」がまだ開いていたら、閉じてしまって構いません。

ここまで来たら、コードのファイルを開いて実行してみて下さい。今度は「Ctrl + Enter」で、１ブロックずつがよいでしょう。パッケージがない、というエラーが出たら、右下のペインの「Packages」→「Install」でそのパッケージを足して下さい。または、`install.packages("パッケージ名")`で。それでも動かないときは、ご連絡下さい。
何をしているのか、コードの要所にコメントを入れていますので、参考になさって下さい。

-  喫茶店は、ggplot2を使ったプロットの例です。丸の大きさを変えるバブルプロットにしているものもあります。Excelと同じことをやっている訳ではありません。「scales」パッケージを使っています。Windows用とMac用があります
-  電力は、Excelでやったのとおなじことをなぞっています。Rならこうなる、ということです。ただし、期間中の祝日も除外しているので、回帰直線の係数がExcelとは異なっています。祝日を除外しないで実行したら、同じになります。やはりWindows用とMac用があります
-  肝疾患は、plotly パッケージを使っています。HTMLファイルを出力しますが、これのみ、outcomeではなく、R_scriptフォルダに出力されます。WindowsとMacの共用です。カーソルで点を指すと、自治体名や数値が分かります。右側の凡例をクリックすると表示がオン・オフされます。カーソルで範囲指定すると拡大表示します。ダブルクリックで元に戻ります
- 「R_no_Tsukaikata.Rmd」は、コードと説明が一体になっています。「Rの使い方（Rmd版をknitした）」の元ファイルです。knitボタンでHTMLを押すと、同じものができます。コードと実行結果に説明を加えてレポートしたいときに、使うとよい形式です
---

## ご案内

9月22日の自主ゼミのご案内です。
今年度２回目は、データ分析の入門編「散布図を描いてみよう」です。２つの要素、たとえば天気と電力需要とか、人口と喫茶店数とか－－が、どんな関係にあるのかを探ります。

1.  データを入手する
1.  それを加工して散布図を描く
1.  関係を探る（＝相関係数をＰＣに計算させる、回帰分析をしてみる）

という流れを予定しています。そのためのツールとして使える「R」と「RStudio」もご紹介します。「散布図＋回帰直線」を一応の目標としています。
なお、1.は簡単に済ませますので、あらかじめ以下の２つを宿題として考えてみて下さい。考えて、ほんの少し試してみるだけで結構です。データの準備は不用です。


### 宿題

-  「スタバ」「ドトール」「コメダ」それぞれの都道府県別の店舗数を知りたい
    ※スタバが鳥取に出店してニュースになりましたが、「スタバはあるけどコメダがない県」が存在します
- 東京の７月と８月の気温を、１時間刻みで知りたい。今年の分だけでなく、去年の分も調べたい


### お願い

当日は、「Excel」など表計算ソフトが入ったパソコンをお持ち下さい。Excelでなくて「Open Office」でも構いませんが、講師はExcelを使いますので、画面表示や操作法は異なります。

また、末尾に書いた２種類のソフト「R」「RStudio」をあらかじめインストールしておいて下さい。インストールに自信のない方は、当日30分早くご来場下さい。
セキュリティ上の理由でインストールができない方は、R + RStudioのクラウド版を使って下さい。ただし、こちらはデータの置き場所や日本語表示などに制約があります。練習用としてだけおすすめします。

１回目の講座でも紹介のあった「Googleスプレッドシート」も活用します。１回目に参加されていない方や、使えるかどうか分からない、という方は、下記のリンクで試してみて下さい。新しいスプレッドシートを作れるようならＯＫです。

### インストール

RとRStudioのインストールについては、川上さん、田中さんに作っていただいた昨年のPDFをご覧下さい。Windows用とMac用に分かれています。Windows用は10ページまで、今回はMeCabは不要です。
また、こちらのサイトも参考になります
https://qiita.com/daifuku_mochi2/items/ad0b398e6affd0688c97

#### ◆統計処理用のプログラム言語「R」

ダウンロードはこちらから。
https://cran.r-project.org/

Rのインストールが済んでから、次の「RStudio」に進んで下さい。

#### ◆Rを使いやすくする必須ソフト「RStsudio」

ダウンロードはこちら。公式ページの「Dowload」をクリックすると製品一覧が表示されます。「RStudio Desktop」とサーバー版があるので、「無料（FREE）」のデスクトップ版を選んで下さい。
https://www.rstudio.com/products/rstudio/download/#download

#### ◆R + RStsudioのクラウド版

インストールが制限されている方のみこちらを。誰でも使えますが、アカウントを作る必要があります。
https://rstudio.cloud

#### ◆Googleスプレッドシート

使えるかどうか、以下のリンクで確認を
[sheets.google.com](https://sheets.google.com)

使えない場合のみ、こちらの手順で設定を
https://support.google.com/docs/answer/6000292

#### ◆Copytables
ブラウザにChromeをお使いの方は、拡張機能の「Copytables」を追加すると、作業が楽になります。可能であれば、程度ですので、無理にインストールしなくても構いません。  
