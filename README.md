# Implementation in R of the Markowitz Criterion 

https://github.com/luana1909/Markowitiz/blob/main/README.md


This project consists of the implementation of the method in R, its tests, the CRAN package and a web application written in R Shiny.

If you just want to run the method by reading the data contained in a excelfile, use the application hosted at shinyapps.io.
[Open rwisp on shinyapps.io] ( https://luanadeazevedo.shinyapps.io/criteriosdedecisao/)

The web application repository is 
[github.com/luana1909/ Markowitiz] (https://github.com/luana1909/Markowitiz)

## Reference

JOKUNG-NGUÉNA, Octave. Microéconomie de l’incertain: risques et décisions. [s.l.] : Dunod, 1998. 

Abstract: The Markowitz criterion is a multicriteria decision-making method that stands out in risk and uncertainty analysis in contexts where probabilities are known. This approach represents an evolution of Pascal's criterion by incorporating the dimension of variability. In this framework, the expected value reflects the anticipated return, while the standard deviation serves as a measure of risk. The 'markowitz' package provides a practical and accessible tool for implementing this method, enabling researchers and professionals to perform analyses without complex calculations. Thus, the package facilitates the application of the Markowitz criterion.

## For Use

### Install option 1 - from github
```
library("devtools");
install_github("luana1909/Markowitiz");
library("Markowitz")
...
```

### Install option 2 - from CRAN
```
install.packages("markowitz ")
library("markowitz ")
...
```

### Calculation option 1 - from vars
```
criterios <- data.frame(criterio = c('a1', 'a2', 'a3'),
peso = c(0.25, 0.5, 0.25))
alternativas <- data.frame(alternativas = c('outdoor', 'televisao', 'jornal'),
a1 = c(12, 36, -3),
a2 = c(-6, 12, 60),
a3 = c(24, 48, 30))
result<-markowitzcalc(criterios, alternativas)
result<-markowitzcalc(criterios, alternativas, lambda_selec = 2)
