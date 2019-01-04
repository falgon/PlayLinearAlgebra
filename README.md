# PlayLinearAlgebra

[![Build Status](https://travis-ci.org/falgon/PlayLinearAlgebra.svg?branch=master)](https://travis-ci.org/falgon/PlayLinearAlgebra)
[![BSD License](http://img.shields.io/badge/license-BSD-blue.svg?style=flat)](LICENSE)

My playground about linear algebra.

## Build

```sh
$ stack build
```

## Apps

Includes a least squares plotter.
It works by solving by LU decomposition and solving by pseudo (Moore-Penrose) inverse matrix respectively.
The method of least squares is as follows.

Let <img src="https://latex.codecogs.com/gif.latex?\inline&space;\boldsymbol{y}=X\beta&plus;\boldsymbol{u}" title="\boldsymbol{y}=X\beta+\boldsymbol{u}" /> be a multiple linear regression model with <img src="https://latex.codecogs.com/gif.latex?\inline&space;p" title="p" /> independent variables where <img src="https://latex.codecogs.com/gif.latex?\inline&space;X\in\mathbb{R}^{m\times&space;n},&space;\boldsymbol{\beta}\in\mathbb{R}^{n\times&space;1},\boldsymbol{y}\in\mathbb{R}^{m\times&space;1}" title="X\in\mathbb{R}^{m\times n}, \boldsymbol{\beta}\in\mathbb{R}^{n\times 1},\boldsymbol{y}\in\mathbb{R}^{m\times 1}" />.
<img src="https://latex.codecogs.com/gif.latex?\inline&space;\boldsymbol{u}" title="\boldsymbol{u}" /> is a vector <img src="https://latex.codecogs.com/gif.latex?\inline&space;\boldsymbol{u}=(u_1,u_2,\cdots,u_m)^T\in\mathbb{R}^{m\times&space;1}" title="\boldsymbol{u}=(u_1,u_2,\cdots,u_m)^T\in\mathbb{R}^{m\times 1}" /> of the probability error according to i.i.d and <img src="https://latex.codecogs.com/gif.latex?\inline&space;\mathrm{N}(0,\sigma^2)" title="\mathrm{N}(0,\sigma^2)" />. In this case, the least squares method is defined by the following equation.

<img src="https://latex.codecogs.com/gif.latex?{\rm&space;OLS}(X,\boldsymbol{y}):=\mathrm{arg}\min_{\boldsymbol{\beta}}\sum_{i=1}^mr(\boldsymbol{\beta})^2_i" title="{\rm OLS}(X',\boldsymbol{y}):=\mathrm{arg}\min_{\boldsymbol{\beta}}\sum_{i=1}^mr(\boldsymbol{\beta})^2_i" />

### `lineqByLU`,`lineqByPseudo`

```sh
$ stack exec lineqByLU
Usage: lineqByLU <dta file path> <output image path> <degree number>
$ stack exec lineqByLU -- range.dta out.png 9 # https://math.arizona.edu/~dsl/brange.htm
```

When executed as above, the following figure will be output.

![least squares with LU decomposition](./assets/lineqByLUout.png)

The same result can be obtained by executing lineqByPseudo in the same way.

### `lineqRegular`

`lineqRegular` performs L2 regularization according to arbitrary parameters.

<img src="https://latex.codecogs.com/gif.latex?\epsilon(\boldsymbol{a})_\lambda=\sum^m_{i=1}(y_i-f_n(x_i))^2&plus;\lambda&space;R(\boldsymbol{a})" title="\epsilon(\boldsymbol{a})_\lambda=\sum^m_{i=1}(y_i-f_n(x_i))^2+\lambda R(\boldsymbol{a})" />

When <img src="https://latex.codecogs.com/gif.latex?\inline&space;R(\boldsymbol{a})" title="R(\boldsymbol{a})" /> is assumed to be the <img src="https://latex.codecogs.com/gif.latex?\inline&space;L^2" title="L^2" /> norm

<img src="https://latex.codecogs.com/gif.latex?\begin{array}{ccc}&space;\epsilon(\boldsymbol{a})_\lambda&=&\sum^m_{i=1}(y_i-f_n(x_i))^2&plus;\lambda\sum^n_{j=1}a^2_j&space;\\&space;&=&(\boldsymbol{y}-X\boldsymbol{a})^T(\boldsymbol{y}-X\boldsymbol{a})&plus;\lambda\boldsymbol{a}^T\boldsymbol{a}&space;\end{array}" title="\begin{array}{ccc} \epsilon(\boldsymbol{a})_\lambda&=&\sum^m_{i=1}(y_i-f_n(x_i))^2+\lambda\sum^n_{j=1}a^2_j \\ &=&(\boldsymbol{y}-X\boldsymbol{a})^T(\boldsymbol{y}-X\boldsymbol{a})+\lambda\boldsymbol{a}^T\boldsymbol{a} \end{array}" />

```sh
$ stack exec lineqRegular
Usage: lineqRegular <dta file path> <output image path> <parameter>
$ stack exec lineqRegular -- rats.dta out.png 1 # https://math.arizona.edu/~dsl/brats.htm
```

When executed as above, the following figure will be output.

![least squares with L2 regularization](./assets/lineqRegularout.png)

### `mkAnimFromDta`

`mkAnimFromDta` outputs git animation that
can see transitions of fitting by model from 1 to the specified dimension.
In order to do this imagemagick must be set up under the execution environment.

```sh
$ stack exec mkAnimFromDta
Usage: mkAnimFromDta <dta file path> <output image path> <degree number>
$ stack exec mkAnimFromDta -- range.dta out.gif 10
```

When executed as above, the following figure will be output.

![least squares with L2 regularization](./assets/mkAnimFromDtaout.gif)

### `mkAnimFromSamples`

`mkAnimFromSamples` outputs a gif animation that can see the fitting transitions for
<img src="https://latex.codecogs.com/gif.latex?\inline&space;¥sin(x_i)&plus;e" />
with a degree specified from 1.
Note that <img src="https://latex.codecogs.com/gif.latex?\inline&space;e" /> 
is noise that follows the normal distribution of 
<img src="https://latex.codecogs.com/gif.latex?\inline&space;N(0,0.2)" />.

```sh
$ stack exec mkAnimFromSamples
Usage: mkAnimFromSamples <output image path> <degree number>
$ stack exec mkAnimFromSamples -- out.gif 8
```

When executed as above, the following figure will be output.

![least squares to sin function](./assets/mkAnimFromSamplesout.gif)


### Related article

There is a [related article (WIP)](https://falgon.github.io/roki.log/posts/2019/%201%E6%9C%88/03/leastSquares/) on my blog (Japanese).
