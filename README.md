# Package "regressr"


## Overview/Synopsis
 Create “regressr”, an R library dedicated to helping user build regression models, providing text output to ease interpretation of the model’s results, and optimizing model specifications. The purpose of this library is to make the process of running regression modeling and coefficient interpretation easier for those unfamiliar with regressions.

## Use
TODO: Specific example of what happens in your project (e.g. what's going on under the hood)

## Data Used
TODO: Where you got your data, description about the data

## Installation
 1. First, you will need to install thee package "devtool" in order to install pacakges on github:
```
install.package("devtool")
```
 2. Load the devtool package:    
```
    library(devtool)
```
 3. Install the regressr package using "author/pacakge":
```
    install_github("GeorgetownMcCourt/regressr")
```
 4. Load the regressr package to use it:
 ```
    library(regressr)
 ```

## Usage
TODO: Write usage instructions
Note: If you're providing a new package or software, provide examples of how to use your code (example: https://github.com/CommerceDataService/eu.us.opendata). If you're providing an analytical output, describe what goes on each file or how to run it.
#### summarizer
```
summarizer(df)
```
#### possibleModels
```
possibleModels(df)
```
#### interpreter
```
interpreter(modelType, dependentVar, independentVar, df, logDepen = F, logIndepen = NULL, squareIndepend = NULL)
```

#### optimizer
```
optimizer(df, dependentVar, independentVar = colnames(df)[colnames(df) != dependentVar], include = NULL,
          model = c("OLS", "binary probit", "binary logit", "ordered probit", "ordered logit", "multinomial logit", "multinomial probit"),
          quadratic = NULL, cubic = NULL,
          time.series = FALSE, time.var = NULL, save.csv = FALSE)
```

## Progress Log
 Write history in bullet form of what progress has been made and when.
 * Project mock code, 4/18/2017
 * Function 1 - summarizer
 * Function 2 - possibleModels
 * Function 3 - interpreter
 * Function 4 - optimizer
  * base function created, 4/20/2017
  * added more user input options. 4/23/2017




## Credits
Author: Justin Goss, Yixuan Huang, Nghia-Piotr Le


## License
MIT License

Copyright (c) 2017 PiotrLeTrong

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
