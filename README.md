# JNPC
## 勉強会のための資料置き場

2018年9月の自主ゼミ用です。資料を準備中。まだ途中で、たびたび更新しています。  
だいたい固まったら、お知らせします。  

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

-  「スタバ」「ドトール」「コメダ」それぞれの都道府県別の店舗数を知りたい。  
    ※スタバが鳥取に出店してニュースになりましたが、「スタバはあるけどコメダがない県」が存在します  
- 東京の７月と８月の気温を、１時間刻みで知りたい。今年の分だけでなく、去年の分も調べたい。


### お願い

当日は、「Excel」など表計算ソフトが入ったパソコンをお持ち下さい。Excelでなくて「Open Office」でも構いませんが、講師はExcelを使いますので、画面表示や操作法は異なります。  

また、末尾に書いた２種類のソフト「R」「RStudio」をあらかじめインストールしておいて下さい。インストールに自信のない方は、当日30分早くご来場下さい。  
セキュリティ上の理由でインストールができない方は、R + RStudioのクラウド版を使って下さい。ただし、こちらはデータの置き場所や日本語表示などに制約があります。練習用としてだけおすすめします。  

１回目の講座でも紹介のあった「Googleスプレッドシート」も活用します。１回目に参加されていない方や、使えるかどうか分からない、という方は、下記のリンクで試してみて下さい。新しいスプレッドシートを作れるようならＯＫです。  

### インストール

ダウンロードとインストールは、こちらが参考になります  
https://qiita.com/daifuku_mochi2/items/ad0b398e6affd0688c97

#### ◆統計処理用のプログラム言語「R」

ダウンロード先はこちら。  
https://cran.r-project.org/
Rのインストールが済んでから、次の「RStudio」に進んで下さい。  

#### ◆Rを使いやすくする必須ソフト「RStsudio」
ダウンロード先はこちら。公式ページの「Dowload」をクリックすると製品一覧が表示されます。「RStudio Desktop」とサーバー版があるので、「無料（FREE）」のデスクトップ版を選んで下さい。  
https://www.rstudio.com/products/rstudio/download/#download

#### ◆R + RStsudioのクラウド版
インストールが制限されている方のみこちらを。誰でも使えますが、アカウントを作る必要があります。  
https://rstudio.cloud

#### ◆Googleスプレッドシート
使えるかどうか、以下のリンクで確認を  
sheets.google.com

使えない場合のみ、こちらの手順で設定を  
https://support.google.com/docs/answer/6000292

#### ◆Copytables
ブラウザにChromeをお使いの方は、拡張機能の「Copytables」を追加すると、作業が楽になります。こちらは、可能であれば、程度です。  
