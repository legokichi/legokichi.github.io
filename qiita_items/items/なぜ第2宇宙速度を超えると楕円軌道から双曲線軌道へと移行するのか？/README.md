# 衛星の運動と円錐曲線

軌道上である速度を超えると、なぜ楕円軌道から双曲線軌道になるのか？

KSPプレイヤーならば誰しも一度は疑問に思ったことがあるはずだ。
KSP沼にハマって `宇宙工学シリーズ` の `人工衛星と宇宙探査機` を読んでいたらそのことへの記述が載っていたため、ここに行間を補いつつ説明する。


軌道面にある衛星の運動を考える。
地球の中心を原点とした極座標系 $(r, \theta)$ を考える。
$r$ 方向と $\theta$ 方向の単位ベクトルをそれぞれ $\boldsymbol{e}\_{r}, \boldsymbol{e}\_{\theta}$ とする。
座標 $(r,\theta)$ における速度 $\boldsymbol{v}$ を求めたい。

```math
\frac{d}{dt} \boldsymbol{e}_r
= \dot{\theta} \boldsymbol{e}_\theta
,\quad
\frac{d}{dt} \boldsymbol{e}_\theta
= - \dot{\theta} \boldsymbol{e}_r
```

であるから

```math
\begin{align}
\boldsymbol{v} = \frac{d}{dt} (r\boldsymbol{e}_r)
& = \dot{r} \boldsymbol{e}_r + r \frac{d}{dt} \boldsymbol{e}_r \\
& = \dot{r} \boldsymbol{e}_r + r \dot{\theta} \boldsymbol{e}_\theta \\
\end{align}
```

なので、加速度 $\boldsymbol{a}$ は

```math
\begin{align}
\boldsymbol{a} = \dot{\boldsymbol{v}}
& = \frac{d}{dt} \left( \dot{r} \boldsymbol{e}_r \right)
  + \frac{d}{dt} \left( r \dot{\theta} \boldsymbol{e}_\theta \right) \\
& = \left( \ddot{r} \boldsymbol{e}_r + \dot{r} \dot{\theta} \boldsymbol{e}_\theta \right) 
  + \left( \dot{r} \dot{\theta} \boldsymbol{e}_\theta + r \frac{d}{dt} \left( \dot{\theta} \boldsymbol{e}_\theta \right) \right) \\
& = \left( \ddot{r} \boldsymbol{e}_r + \dot{r} \dot{\theta} \boldsymbol{e}_\theta \right)
  + \left( \dot{r} \dot{\theta} \boldsymbol{e}_\theta + r \left( \ddot{\theta} \boldsymbol{e}_\theta + \dot{\theta} \frac{d}{dt} \boldsymbol{e}_\theta \right) \right) \\
& = \left( \ddot{r} \boldsymbol{e}_r + \dot{r} \dot{\theta} \boldsymbol{e}_\theta \right)
  + \left( \dot{r} \dot{\theta} \boldsymbol{e}_\theta + r \left( \ddot{\theta} \boldsymbol{e}_\theta - \dot{\theta}^2 \boldsymbol{e}_r \right) \right) \\
& = \ddot{r} \boldsymbol{e}_r
  + \dot{r} \dot{\theta} \boldsymbol{e}_\theta
  + \dot{r} \dot{\theta} \boldsymbol{e}_\theta 
  + r \ddot{\theta} \boldsymbol{e}_\theta
  - r \dot{\theta}^2 \boldsymbol{e}_r \\
& = \left( \ddot{r} - r \dot{\theta}^2 \right) \boldsymbol{e}_r
  + \left( 2 \dot{r} \dot{\theta} + r \ddot{\theta} \right) \boldsymbol{e}_\theta
\end{align}
```

となる。
衛星には万有引力が作用するので運動方程式

```math
m \boldsymbol{a}
= - \frac{mMG}{r^2} \boldsymbol{e}_r
```

が得られる。

この式を $r$ 方向と $\theta$ 方向について表現すると

```math
\ddot{r} - r \dot{\theta}^2 = - \frac{MG}{r^2} \tag{1}
```
```math
2 \dot{r} \dot{\theta} + r \ddot{\theta} = 0 \tag{2}
```

$r$ 方向の式1を $r$ で積分すると

> ```math
> \ddot{r} = \left( \frac{d}{dt}r \right) \left( \frac{d}{dr}\dot{r} \right)
> ```
> なので

```math
\frac{\dot{r}^2}{2} + \frac{h^2}{2r^2}
= \frac{MG}{r} + E \tag{3}
```

となる。ここで

```math
{v_r}^2 = \dot{r}^2 + r^2\dot{\theta}^2
```

と式3をあわせて変形すると

```math
\frac{{v_r}^2}{2} - \frac{GM}{r} = E \tag{4}
```

となる。この式は左辺第1項は運動エネルギー、第2項は重力ポテンシャルエネルギーを示すエネルギー保存則である。


$\theta$ 方向の式2を $t$ で積分すると

```math
r^2 \dot{\theta} = h \tag{5}
```

> ```math
> \begin{align}
> \frac{1}{r}\frac{d}{dt} \left( r^2 \dot{\theta} \right)
> & = \frac{2 r \dot{r} \dot{\theta} + r^2 \ddot{\theta}}{r} \\
> & = 2 \dot{r} \dot{\theta} + r \ddot{\theta} \\
> \end{align}
> ```
> であるため。

ここで積分定数 $h$ は単位質量あたりの角運動量であり、この式は角運動量保存則を示す。

衛星の軌道を求めるには式1,2から $t$ を消去すればよい。
式5より

$$
dt = \frac{r^2}{h}d\theta
$$

を使って式4に代入し、 $dr/d\theta$ について解けば


```math
\frac{dr}{d\theta}
= r \sqrt{
    \frac{2E}{h^2}r^2
  + \frac{2GM}{h^2}r
  - 1
} \tag{6}
```

という微分方程式になる。これを $C$ を積分定数として変数分離法で解くと

```math
\sin^{-1} = \frac{
\frac{GMr}{h} - h
}{
r \sqrt{2E + \frac{(GM)^2}{h^2} }
} = \theta + C \tag{7}
```

となる。

> ```math
> g(y) \frac{dy}{dx} = f(x)
> ```
> のような形になる微分方程式を変数分離形という。

近地点で $\theta=0$ となるようにすると $C=-\pi/2$ となるので、 $r$　について解けば


```math
\begin{align}
r
& = \frac{
  \frac{h^2}{GM}
}{
  1 + \sqrt{\frac{2Eh^2}{(GM)^2} + 1} \cos \theta
} \\
& = \frac{l}{1 + e \cos \theta}
\end{align} \tag{8}
```

という円錐曲線の方程式になる。

円錐曲線は離心率 $e$ の値によって

|$e=0$|円|
|:----|----|
|$0<e<1$|楕円|
|$e=1$|放物線|
|$1<e$|双曲線|

のように変化する。

このとき $e=1$ となる速度が、楕円軌道から放物線そして双曲線軌道へと変化する脱出速度、第2宇宙速度である。

## 所感

途中で眠くなったので式変形追えていない。WolframAlphaに投げよう

## 参考


* [宇宙工学シリーズ 人工衛星と宇宙探査機｜コロナ社](http://www.coronasha.co.jp/np/isbn/9784339012231/)
