## readQP - An tool for reading optimization model files in R

There are currently a number of R packages available for solving optimization problems, either using proprietary solvers or with built-in solvers. In addition, many of those packages provide tools for reading a pre-specified problem from a file, but they are limited to linear and mixed-integer problems.

ReadQP aims to build on top of that functionality by providing the ability to also handle quadratic programming problems by wrapping the `Rglpk_read_file` function from [Rglpk](https://cran.r-project.org/web/packages/Rglpk/index.html) and adding additional steps to parse a quadratic objective into an in-memory representation that is compatible with [ROI](https://cran.r-project.org/web/packages/ROI/index.html).


## TO DO:
  - ~~Handle all variations of the model goal (e.g. max, min, maximize, maximum, upper and lower case etc.)~~
  - ~~Handle all variations of the constraint statement (Subject To, s.t., such that, ST., etc.)~~
  - Nested quadratic components in the objective
