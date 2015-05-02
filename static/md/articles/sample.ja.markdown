What's this page?
======================================================================

This is a sample article which contains example materials, including source code, mathematics, etc.

The author usually writes articles in [Markdown](http://daringfireball.net/projects/markdown/) (or more specifically, a kind of [GitHub Flavored Markdown](https://help.github.com/articles/github-flavored-markdown)), so the materials may accompany the corresponding markdowns for reference.

Markdown files are converted to HTML using [pandoc](http://pandoc.org/) with [githubMarkdownExtensions](http://hackage.haskell.org/package/pandoc-1.13.2.1/docs/Text-Pandoc-Options.html#v:githubMarkdownExtensions) reader option.


Basic writing
======================================================================

Styling text
----------------------------------------------------------------------

```markdown
**bold**, *italic*, ~~strike through~~
```

**bold**, *italic*, ~~strike through~~


Blockquotes
----------------------------------------------------------------------

```markdown
Upon crossing the Rubicon, Gaius Julius Caesar said:
> The die is cast.
```

Upon crossing the Rubicon, Gaius Julius Caesar said:
> The die is cast.

Lists
======================================================================

Unordered lists
----------------------------------------------------------------------

```markdown
- Item
- Item
- Item
```

- Item
- Item
- Item


Ordered lists
----------------------------------------------------------------------

```markdown
1. Item 1
2. Item 2
3. Item 3
```

1. Item 1
2. Item 2
3. Item 3

Nested lists
----------------------------------------------------------------------

```markdown
- Item
    1.  Nested item 1
    2.  Nested item 2
- Item
    - Nested item
    - Nested item
        - More nested item
```

- Item
    1.  Nested item 1
    2.  Nested item 2
- Item
    - Nested item
    - Nested item
        - More nested item


Tables
======================================================================

[GitHub Flavored Markdown](https://help.github.com/articles/github-flavored-markdown) offers syntax to create tables as below.

```markdown
| Tables   |      Are      |  Cool |
|----------|:-------------:|------:|
| col 1 is |  left-aligned | $1600 |
| col 2 is |    centered   |   $12 |
| col 3 is | right-aligned |    $1 |
```

Although it is more concise and readable, it cannot specify classes for tables. So, I have to use `<table>` tags to specify [Pure Table classes](http://purecss.io/tables/) unless someone or I implement a pegdown's extension like [special attribute feature of PHP Markdown Extra](https://michelf.ca/projects/php-markdown/extra/#spe-attr).


Bordered tables
----------------------------------------------------------------------

```html
<table class="pure-table pure-table-bordered">
  <thead>
    <tr>
      <th>Tables</th>
      <th align="center">Are</th>
      <th align="right">Cool</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>col 1 is</td>
      <td align="center">left-aligned</td>
      <td align="right">$1600</td>
    </tr>
    <tr>
      <td>col 2 is</td>
      <td align="center">centered</td>
      <td align="right">$12</td>
    </tr>
    <tr>
      <td>col 3 is</td>
      <td align="center">right-aligned</td>
      <td align="right">$1</td>
    </tr>
  </tbody>
</table>
```

<table class="pure-table pure-table-bordered">
  <thead>
    <tr>
      <th>Tables</th>
      <th align="center">Are</th>
      <th align="right">Cool</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>col 1 is</td>
      <td align="center">left-aligned</td>
      <td align="right">$1600</td>
    </tr>
    <tr>
      <td>col 2 is</td>
      <td align="center">centered</td>
      <td align="right">$12</td>
    </tr>
    <tr>
      <td>col 3 is</td>
      <td align="center">right-aligned</td>
      <td align="right">$1</td>
    </tr>
  </tbody>
</table>


Source code
======================================================================

Pegdown supports [Fenced Code Blocks](https://help.github.com/articles/github-flavored-markdown#fenced-code-blocks), which can specify a class for code blocks shown in the example below. Syntax highlighting is added by [highlight.js](https://highlightjs.org/) with github style.

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


Mathematics
======================================================================

Mathematics is beautifully displayed by [MathJax](http://www.mathjax.org/) engine, which supports TeX/LaTeX, MathML, and AsciiMath for syntax.

<h2 class="math-header">The Lorenz Equations</h2>

$$
\begin{aligned}
\dot{x}\ & = \sigma(y-x) \\\\
\dot{y}\ & = \rho x - y - xz \\\\
\dot{z}\ & = -\beta z + xy
\end{aligned}
$$

<h2 class="math-header">The Cauchy-Schwarz Inequality</h2>

$$
\left( \sum_{k=1}^n a_k b_k \right)^2 \leq \left( \sum_{k=1}^n a_k^2 \right) \left( \sum_{k=1}^n b_k^2 \right)
$$

<h2 class="math-header">A Cross Product Formula</h2>

$$
\mathbf{V}_1 \times \mathbf{V}_2 =  \begin{vmatrix}
\mathbf{i} & \mathbf{j} & \mathbf{k} \\\\
\frac{\partial X}{\partial u} &  \frac{\partial Y}{\partial u} & 0 \\\\
\frac{\partial X}{\partial v} &  \frac{\partial Y}{\partial v} & 0
\end{vmatrix}
$$

<h2 class="math-header">The probability of getting \(k\) heads when flipping \(n\) coins is</h2>

$$ P(E)   = {n \choose k} p^k (1-p)^{ n-k} $$

<h2 class="math-header">An Identity of Ramanujan</h2>

$$
\frac{1}{\Bigl(\sqrt{\phi \sqrt{5}}-\phi\Bigr) e^{\frac25 \pi}} =
1+\frac{e^{-2\pi}} {1+\frac{e^{-4\pi}} {1+\frac{e^{-6\pi}}
{1+\frac{e^{-8\pi}} {1+\ldots} } } }
$$

<h2 class="math-header">A Rogers-Ramanujan Identity</h2>

$$
1 + \frac{q^2}{(1-q)}+\frac{q^6}{(1-q)(1-q^2)}+\cdots =
\prod_{j=0}^{\infty}\frac{1}{(1-q^{5j+2})(1-q^{5j+3})},
\quad\quad \text{for $|q|<1$}.
$$

<h2 class="math-header">Maxwell’s Equations</h2>

$$
\begin{aligned}
\nabla \times \vec{\mathbf{B}} -\, \frac1c\, \frac{\partial\vec{\mathbf{E}}}{\partial t}\ & = \frac{4\pi}{c}\vec{\mathbf{j}} \\\\
\nabla \cdot \vec{\mathbf{E}}\ & = 4 \pi \rho \\\\
\nabla \times \vec{\mathbf{E}}\, +\, \frac1c\, \frac{\partial\vec{\mathbf{B}}}{\partial t}\ & = \vec{\mathbf{0}} \\\\
\nabla \cdot \vec{\mathbf{B}}\ & = 0
\end{aligned}
$$
