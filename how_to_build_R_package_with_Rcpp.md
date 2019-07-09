* First we claim

  - current_dir = Your current directory
  - pkg_name = The name of your package

* On an active R console, run
```
require(Rcpp)
require(RcppArmadillo)

RcppArmadillo.package.skeleton(pkg_name)
```

* change dir to package directory, and 
  -  put .cpp file under dir ./src
  -  put .R file under dir ./R
  start a new R session and run  
```
compileAttributes(verbose=TRUE)
```
  Then run
```
library(tools)
package_native_routine_registration_skeleton("~/Downloads/regboost")
```

* Switch back to the mother folder of your package,
run the following code from terminal

 ```
 R CMD build pkg_name
 R CMD INSTALL pkg_name_1.0.tar.gz
 ```
 
 Then open a new R session, run
 ```
 require(pkg_name)
 ```
 Chicken dinner! 
