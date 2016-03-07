## readQP - A tool for reading optimization model files in R

There are currently a number of R packages available for solving optimization problems, either using proprietary solvers or with built-in solvers. In addition, many of those packages provide tools for reading a pre-specified problem from a file, but they are limited to linear and mixed-integer problems.

ReadQP aims to build on top of that functionality by providing the ability to also handle quadratic programming problems by wrapping the `Rglpk_read_file` function from [Rglpk](https://cran.r-project.org/web/packages/Rglpk/index.html) and adding additional steps to parse a quadratic objective into an in-memory representation that is compatible with [ROI](https://cran.r-project.org/web/packages/ROI/index.html).


## TO DO:
  - ~~Handle all variations of the model goal (e.g. max, min, maximize, maximum, upper and lower case etc.)~~
  - ~~Handle all variations of the constraint statement (Subject To, s.t., such that, ST., etc.)~~
  - Nested quadratic components in the objective

## Installation
To install `readQP` from GitHub, you will need the `devtools` package installed. Instructions for installing devtools can be found [here](https://github.com/hadley/devtools).

Once you have `devtools` installed, simply run the following command from your R console to install `readQP`:
```R
library(devtools)
devtools::install_github('cafreeman/readQP')
```

To verify the installation, you can run the test suite with the following command:
```R
library(testthat)
library(readQP)
testthat::test_package('readQP')
```

## Usage
`readQP` currently exposes one main function, `readModelFile`, which takes a file path and a file type as arguments and returns an in-memory representation of the model.

To see a basic example, execute the following commands:
```R
sampleFile <- readQP:::getSampleData("simple_qp")
qpModel <- readModelFile(sampleFile)
str(qpModel)
```

The resulting `qpModel` object will contain all the various components of the model, including the quadratic component which can be found at `qpModel$objective$Q`.
