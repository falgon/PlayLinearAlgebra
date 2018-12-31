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
It works by solving by LU decomposition and solving by pseudo inverse matrix respectively.

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
<img src="https://latex.codecogs.com/gif.latex?\inline \sin(x_i)+e" />
with a degree specified from 1.
Note that <img src="https://latex.codecogs.com/gif.latex?\inline e" /> 
is noise that follows the normal distribution of 
<img src="https://latex.codecogs.com/gif.latex?\inline N(0,0.2)" />.

```sh
$ stack exec mkAnimFromSamples
Usage: mkAnimFromSamples <output image path> <degree number>
$ stack exec mkAnimFromSamples -- out.gif 8
```

When executed as above, the following figure will be output.

![least squares to sin function](./assets/mkAnimFromSamplesout.gif)
