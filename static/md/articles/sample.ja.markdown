このページは何？
======================================================================

このページは，サンプル記事である．ソースコード，数式表示などの例が含まれている．

筆者は普段， [Markdown](http://daringfireball.net/projects/markdown/)（特に，[GitHub Flavored Markdown](https://help.github.com/articles/github-flavored-markdown) の一種）で記事を書く．従って，いつでも参照できるように，例は対応する markdown を伴っている．

markdown ファイルは，[pandoc](http://pandoc.org/) に [githubMarkdownExtensions](http://hackage.haskell.org/package/pandoc-1.13.2.1/docs/Text-Pandoc-Options.html#v:githubMarkdownExtensions) リーダーオプションを用いて，HTML に変換される．


基本的なライティング
======================================================================

テキストにスタイルを付ける
----------------------------------------------------------------------

```markdown
**太字**, *イタリック*, ~~取り消し線~~
```

**太字**, *イタリック*, ~~取り消し線~~

引用ブロック
----------------------------------------------------------------------

```markdown
ルビコン川を渡る際，ガイウス・ユリウス・カエサルは言った：
> 賽は投げられた．
```

ルビコン川を渡る際，ガイウス・ユリウス・カエサルは言った：
> 賽は投げられた．


リスト
======================================================================

順序なしリスト
----------------------------------------------------------------------

```markdown
- 項目
- 項目
- 項目
```

- 項目
- 項目
- 項目

順序付きリスト
----------------------------------------------------------------------

```markdown
1. 項目1
2. 項目2
3. 項目3
```

1. 項目1
2. 項目2
3. 項目3

ネストしたリスト
----------------------------------------------------------------------

```markdown
- 項目
    1. ネストした項目1
    2. ネストした項目2
- 項目
    - ネストした項目
    - ネストした項目
        - もっとネストした項目
```

- 項目
    1. ネストした項目1
    2. ネストした項目2
- 項目
    - ネストした項目
    - ネストした項目
        - もっとネストした項目


テーブル
======================================================================

[GitHub Flavored Markdown](https://help.github.com/articles/github-flavored-markdown) は，以下のような，テーブルを作る構文を提供している．

```markdown
| テーブル  |      は       |  クール |
|----------|:-------------:|-------:|
|  列1は   |     左寄せ     | $1600 |
|  列2は   |    中央寄せ    |   $12 |
|  列3は   |     右寄せ     |    $1 |
```

この構文はより簡潔で読み易いが，テーブルのクラスを指定することができない．従って，テーブルのスタイルを直接修正しない限り，[Bootstrap のテーブルクラス](http://getbootstrap.com/css/#tables) を指定するために，`<table>` タグを用いる必要がある．

```html
<table class="table">
  <thead>
    <tr>
      <th>テーブル</th>
      <th style="text-align: center">は</th>
      <th style="text-align: right">クール</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>列1は</td>
      <td align="center">左寄せ</td>
      <td align="right">$1600</td>
    </tr>
    <tr>
      <td>列2は</td>
      <td align="center">中央寄せ</td>
      <td align="right">$12</td>
    </tr>
    <tr>
      <td>列3は</td>
      <td align="center">右寄せ</td>
      <td align="right">$1</td>
    </tr>
  </tbody>
</table>
```

<table class="table">
  <thead>
    <tr>
      <th>テーブル</th>
      <th style="text-align: center">は</th>
      <th style="text-align: right">クール</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>列1は</td>
      <td align="center">左寄せ</td>
      <td align="right">$1600</td>
    </tr>
    <tr>
      <td>列2は</td>
      <td align="center">中央寄せ</td>
      <td align="right">$12</td>
    </tr>
    <tr>
      <td>列3は</td>
      <td align="center">右寄せ</td>
      <td align="right">$1</td>
    </tr>
  </tbody>
</table>

ボーダー付きテーブル
----------------------------------------------------------------------

ボーダー付きテーブルを作るには，`.table-bordered` クラスを追加するだけでよい．他のスタイルのテーブルについては，[Bootstrap のドキュメント](http://getbootstrap.com/css/#tables)を参照せよ．

<table class="table table-bordered">
  <thead>
    <tr>
      <th>テーブル</th>
      <th style="text-align: center">は</th>
      <th style="text-align: right">かっこいい</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>列1は</td>
      <td align="center">左寄せ</td>
      <td align="right">$1600</td>
    </tr>
    <tr>
      <td>列2は</td>
      <td align="center">中央寄せ</td>
      <td align="right">$12</td>
    </tr>
    <tr>
      <td>列3は</td>
      <td align="center">右寄せ</td>
      <td align="right">$1</td>
    </tr>
  </tbody>
</table>


ソースコード
======================================================================

GitHub Flavored Markdown は，[フェンスコードブロック](https://help.github.com/articles/github-flavored-markdown#fenced-code-blocks)をサポートしている．フェンスコードブロックは，以下に示すように，コードブロックのクラスを指定できる．
ハイライトは，[highlight.js](https://highlightjs.org/)の github スタイルによって行われる．

Clojure:

<pre>
```clojure
(defn hello []
  (println "Hello, world!"))
```
</pre>

```clojure
(defn hello []
  (println "Hello, world!"))
```

Haskell:

<pre>
```haskell
main :: IO ()
main = putStrLn "Hello, world!"
```
</pre>

```haskell
main :: IO ()
main = putStrLn "Hello, world!"
```

JavaScript:

<pre>
```javascript
console.log('Hello, world!');
```
</pre>

```javascript
console.log('Hello, world!');
```


数式
======================================================================

数式は，[MathJax](http://www.mathjax.org/) エンジンによって美しく表示される．MathJax エンジンは，TeX/LaTeX，MathML，および AsciiMath 形式をサポートしている．

ローレンツ方程式
----------------------------------------------------------------------

$$
\begin{aligned}
\dot{x}\ & = \sigma(y-x) \\\\
\dot{y}\ & = \rho x - y - xz \\\\
\dot{z}\ & = -\beta z + xy
\end{aligned}
$$

コーシー＝シュワルツの不等式
----------------------------------------------------------------------

$$
\left( \sum_{k=1}^n a_k b_k \right)^2 \leq \left( \sum_{k=1}^n a_k^2 \right) \left( \sum_{k=1}^n b_k^2 \right)
$$

クロス積の式
----------------------------------------------------------------------

$$
\mathbf{V}_1 \times \mathbf{V}_2 =  \begin{vmatrix}
\mathbf{i} & \mathbf{j} & \mathbf{k} \\\\
\frac{\partial X}{\partial u} &  \frac{\partial Y}{\partial u} & 0 \\\\
\frac{\partial X}{\partial v} &  \frac{\partial Y}{\partial v} & 0
\end{vmatrix}
$$

コインを\(n\)個投げたとき，\(k\)個表になる確率
----------------------------------------------------------------------

$$ P(E)   = {n \choose k} p^k (1-p)^{ n-k} $$

ラマヌジャンの恒等式
----------------------------------------------------------------------

$$
\frac{1}{\Bigl(\sqrt{\phi \sqrt{5}}-\phi\Bigr) e^{\frac25 \pi}} =
1+\frac{e^{-2\pi}} {1+\frac{e^{-4\pi}} {1+\frac{e^{-6\pi}}
{1+\frac{e^{-8\pi}} {1+\ldots} } } }
$$

ロジャース・ラマヌジャン恒等式
----------------------------------------------------------------------

$$
1 + \frac{q^2}{(1-q)}+\frac{q^6}{(1-q)(1-q^2)}+\cdots =
\prod_{j=0}^{\infty}\frac{1}{(1-q^{5j+2})(1-q^{5j+3})},
\quad\quad \text{for $|q|<1$}.
$$

マクスウェルの方程式
----------------------------------------------------------------------

$$
\begin{aligned}
\nabla \times \vec{\mathbf{B}} -\, \frac1c\, \frac{\partial\vec{\mathbf{E}}}{\partial t}\ & = \frac{4\pi}{c}\vec{\mathbf{j}} \\\\
\nabla \cdot \vec{\mathbf{E}}\ & = 4 \pi \rho \\\\
\nabla \times \vec{\mathbf{E}}\, +\, \frac1c\, \frac{\partial\vec{\mathbf{B}}}{\partial t}\ & = \vec{\mathbf{0}} \\\\
\nabla \cdot \vec{\mathbf{B}}\ & = 0
\end{aligned}
$$
