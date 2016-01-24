[Node.js v4.2.0 LTS][Node.js v4.2.0] がリリースされてから3ヶ月以上が経過し，それどころか，[v5.5.0][Node.js v5.5.0] までリリースされているにも関わらず，[AWS OpsWorks][OpsWorks] が out-of-box でサポートしている最新のバージョンは，未だに [v0.12.9 LTS][Node.js v0.12.9] のみである．どうやら，[コミュニティの cookbook を使うことを推奨しているらしい][OpsWorks Support]．

![AWS OpsWorks: Add Node.js Layer](/static/img/articles/nodejs-v4-v5-on-aws-opsworks/aws-opsworks-add-nodejs-layer.png)

もちろん，我々は OpsWorks のサポートを悠長に待つわけにはいかない．[v5.x では 1，2 週間ごとに新しいバージョンがリリースされる][Node.js v5]のだ．
そこで，本稿では，Node.js v5.x を OpsWorks で使う方法を示す．同様の方法によって， v4.x も含めたあらゆるバージョンをインストールできるはずだ．

Custom Cookbook
----------------------------------------------------------------------

幸い，OpsWorks では，[Custom Cookbook][Custom Cookbook] を使うことによって，デプロイ手順を自由にカスタマイズできる．そのため，任意のバージョンの Node.js をインストールするには，cookbook を自分で書いて，それを使えば良いだけである．

とはいえ，Node.js をインストールする cookbook をスクラッチから書くのは少々面倒である[^1]．そこで，既存の cookbook である [redguide/nodejs][redguide/nodejs] を使う．

nodejs cookbook は，Node.js のバイナリをフェッチ，インストールする recipe を提供してくれる．この cookbook の recipe を使うには，あなたの cookbook の `metadata.rb` に，次の 1 行を追加する必要がある．

metadata.rb:
```ruby
depends 'nodejs'
```

そして，OpsWorks でフックする recipe（例えば，`nodejs_setup.rb` としよう）にて，nodejs のデフォルト recipe を実行する．

recipes/nodejs\_setup.rb:
```ruby
include_recipe 'nodejs'
```

この状態で `nodejs_setup` を実行すると，v0.10.26 がインストールされてしまう．従って，異なるバージョンの Node.js をインストールするには，attributes を適切に設定する必要がある．

Attributes
----------------------------------------------------------------------

attributes は，以下のように設定すれば良い．ここでは，v5.5.0 をインストールする場合の値を示している．もちろん，他のバージョンをインストールしたい場合は，バージョン番号とチェックサムを変更すること[^2]．

attributes/default.rb
```ruby
### Node.js ###

default['nodejs']['engine'] = 'node'
default['nodejs']['version'] = '5.5.0'
default['nodejs']['install_method'] = 'binary'

# See `http://nodejs.org/dist/v5.5.0/SHASUMS256.txt` or
# calculate by `curl -L -s http://nodejs.org/dist/v5.5.0/node-v5.5.0.tar.gz | shasum -a 256`
override['nodejs']['binary']['checksum'] = 'd69b18cc20699a35434858fb853997616762280610a510ec4b4ff1a94798b432'
```

OpsWorks
----------------------------------------------------------------------

あとは，[Custom Cookbook を OpsWorks にインストール][Install Custom Cookbook]し，Custom Layer を作って `nodejs_setup` をフックすれば良い．Node.js のインストール以外の recipe は，OpsWorks であらかじめ用意されているものを用いる．

最終的な Custom Chef Recipes は次のようになる．ここで，Custom Cookbook の名前は `nodejs_sample` であると仮定している．

![AWS OpsWorks: Custom Chef Recipes](/static/img/articles/nodejs-v4-v5-on-aws-opsworks/aws-opsworks-nodejs-custom-chef-recipes.png)

おわりに
----------------------------------------------------------------------

以上の方法によって，（執筆時点で）最新の安定版である v5.5.0 を OpsWorks で利用できる．大した労力はいらないはずだ．OpsWorks の設定方法が分からない場合は，[公式のドキュメント][OpsWorks Document]を参照してほしい．

参照サイト
----------------------------------------------------------------------

- [Node v4.2.0 (LTS) | Node.js][Node.js v4.2.0]
- [Node v5.5.0 (Stable) | Node.js][Node.js v5.5.0]
- [AWS OpsWorks （DevOps アプリケーションの管理・自動化） | AWS][OpsWorks]
- [Node v0.12.9 (LTS) | Node.js][Node.js v0.12.9]
- [AWS Developer Forums: Nodejs layer support of nodejs v4 ...][OpsWorks Support]
- [What You Should Know about Node.js v5 and More | Node.js][Node.js v5]
- [クックブックとレシピ - AWS OpsWorks][Custom Cookbook]
- [redguide/nodejs: Chef cookbook for nodejs][redguide/nodejs]
- [カスタムクックブックのインストール - AWS OpsWorks][Install Custom Cookbook]
- [AWS OpsWorks とは何ですか? - AWS OpsWorks][OpsWorks Document]
- [Upgrade to chef 12 produces checksum error · Issue #61 · redguide/nodejs][Chef 12 Checksum Error]

[^1]: スクラッチから書く気があるなら，この記事を読んでいないはずだ．
[^2]: checksum を指定しないと，[Chef::Exceptions::ChecksumMismatch エラーが出ることがある][Chef 12 Checksum Error]．

[Node.js v4.2.0]: https://nodejs.org/en/blog/release/v4.2.0/
[Node.js v5.5.0]: https://nodejs.org/en/blog/release/v5.5.0/
[OpsWorks]: https://aws.amazon.com/opsworks/
[Node.js v0.12.9]: https://nodejs.org/en/blog/release/v0.12.9/
[OpsWorks Support]: https://forums.aws.amazon.com/thread.jspa?threadID=215979
[Node.js v5]: https://nodejs.org/en/blog/community/node-v5/
[Custom Cookbook]: http://docs.aws.amazon.com/ja_jp/opsworks/latest/userguide/workingcookbook.html
[redguide/nodejs]: https://github.com/redguide/nodejs
[Install Custom Cookbook]: http://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook-installingcustom-enable.html
[OpsWorks Document]: http://docs.aws.amazon.com/opsworks/latest/userguide/welcome.html
[Chef 12 Checksum Error]: https://github.com/redguide/nodejs/issues/61
