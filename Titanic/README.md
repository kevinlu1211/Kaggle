### Files:
- There are currently two files
  1. titanic.R is where I use the algorithms from the library (instead of caret) so that I have a better understanding of how the library works; while doing this I will also read up on the machine learning methods used and try to deepen my understanding of these methods
  2. caretTitanic.R is where I use the caret library to access different machine learning methods so that I can get a high level understanding of how the library works; whereby later I will be able to use these libraries in my titanic.R file to get a more thorough understanding

### Currently working on:

- Using the Caret library to do spot checking of different algorithms in caretTitanic.R
  1. Gradient Boosted Machines using [gmb](https://cran.r-project.org/web/packages/gbm/gbm.pdf)
  2. LASSO/Ridge Regression/Elastic Net using [glmnet](https://cran.r-project.org/web/packages/glmnet/glmnet.pdf)
  3. Decision Trees using [rpart](https://cran.r-project.org/web/packages/rpart/vignettes/longintro.pdf)
  4. Random Forests using [randomForest](https://cran.r-project.org/web/packages/randomForest/randomForest.pdf)
  5. Logistic Regression via glm() function in R
  6. SVMs via e1071

---

### Todos:
- ~~Read/Implement Decision Trees using [rpart](https://cran.r-project.org/web/packages/rpart/vignettes/longintro.pdf)~~ 20/08/16
- ~~Read/Implement Random Forests using [randomForest](https://cran.r-project.org/web/packages/randomForest/randomForest.pdf)~~ 23/08/16
- ~~Read/Implement Logistic Regression using glm()~~ 27/08/16
- ~~Read/Implement SVMs from [e1071](https://cran.r-project.org/web/packages/e1071/e1071.pdf
)~~ 29/08/16
- Use Neural Networks from [here](http://www.parallelr.com/r-deep-neural-network-from-scratch/)
- Use emsembles from [here](http://machinelearningmastery.com/machine-learning-ensembles-with-r/)

### Readings:
- Read [this](http://freakonometrics.hypotheses.org/19874) to get an basic understanding of boosting algorithms