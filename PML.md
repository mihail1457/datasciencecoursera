---
title: "Leaflet_Rest"
author: "Mihail Petkov"
date: "7/9/2020"
output:
  html_document: default
  pdf_document: default
---




## Background

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset).


## Data

Data
The training data for this project are available here:

<a href="https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv">https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv</a>

The test data are available here:

<a href="https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv">https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv</a>

The data for this project come from this source: http://groupware.les.inf.puc-rio.br/har. If you use the document you create for this class for any purpose please cite them as they have been very generous in allowing their data to be used for this kind of assignment.

## Data Importing and Cleaning


```r
library(caret)
```

```
## Warning: package 'caret' was built under R version 3.6.1
```

```
## Loading required package: lattice
```

```
## Loading required package: ggplot2
```

```
## Warning: package 'ggplot2' was built under R version 3.6.1
```

```
## Error: package or namespace load failed for 'ggplot2' in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]):
##  there is no package called 'scales'
```

```
## Error: package 'ggplot2' could not be loaded
```

```r
library(e1071)
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.6.1
```

```
## Error: package or namespace load failed for 'ggplot2' in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]):
##  there is no package called 'scales'
```

```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.6.1
```

```
## Error: package or namespace load failed for 'dplyr' in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
##  there is no package called 'glue'
```

```r
library(randomForest)
```

```
## Warning: package 'randomForest' was built under R version 3.6.1
```

```
## randomForest 4.6-14
```

```
## Type rfNews() to see new features/changes/bug fixes.
```

```r
library(tictoc)

set.seed(60)
# Download and unzip the data
fileurl1 = 'https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv'
fileurl2 = 'https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv'

if (!file.exists('./PML/trainSample')){
  download.file(fileurl1,'./PML/trainSample.csv')
  dateDownloaded1 <- date()
}

if (!file.exists('./PML/testSample')){
  download.file(fileurl2,'./PML/testSample.csv')
  dateDownloaded2 <- date()
}



trainSample <- read.csv('./PML/trainSample.csv', na.strings=c("NA", "#DIV/0!", ""))
testSample <- read.csv('./PML/testSample.csv', na.strings=c("NA", "#DIV/0!", ""))

# Checking the dimensions of the data
colnames(trainSample)
```

```
##   [1] "X"                        "user_name"               
##   [3] "raw_timestamp_part_1"     "raw_timestamp_part_2"    
##   [5] "cvtd_timestamp"           "new_window"              
##   [7] "num_window"               "roll_belt"               
##   [9] "pitch_belt"               "yaw_belt"                
##  [11] "total_accel_belt"         "kurtosis_roll_belt"      
##  [13] "kurtosis_picth_belt"      "kurtosis_yaw_belt"       
##  [15] "skewness_roll_belt"       "skewness_roll_belt.1"    
##  [17] "skewness_yaw_belt"        "max_roll_belt"           
##  [19] "max_picth_belt"           "max_yaw_belt"            
##  [21] "min_roll_belt"            "min_pitch_belt"          
##  [23] "min_yaw_belt"             "amplitude_roll_belt"     
##  [25] "amplitude_pitch_belt"     "amplitude_yaw_belt"      
##  [27] "var_total_accel_belt"     "avg_roll_belt"           
##  [29] "stddev_roll_belt"         "var_roll_belt"           
##  [31] "avg_pitch_belt"           "stddev_pitch_belt"       
##  [33] "var_pitch_belt"           "avg_yaw_belt"            
##  [35] "stddev_yaw_belt"          "var_yaw_belt"            
##  [37] "gyros_belt_x"             "gyros_belt_y"            
##  [39] "gyros_belt_z"             "accel_belt_x"            
##  [41] "accel_belt_y"             "accel_belt_z"            
##  [43] "magnet_belt_x"            "magnet_belt_y"           
##  [45] "magnet_belt_z"            "roll_arm"                
##  [47] "pitch_arm"                "yaw_arm"                 
##  [49] "total_accel_arm"          "var_accel_arm"           
##  [51] "avg_roll_arm"             "stddev_roll_arm"         
##  [53] "var_roll_arm"             "avg_pitch_arm"           
##  [55] "stddev_pitch_arm"         "var_pitch_arm"           
##  [57] "avg_yaw_arm"              "stddev_yaw_arm"          
##  [59] "var_yaw_arm"              "gyros_arm_x"             
##  [61] "gyros_arm_y"              "gyros_arm_z"             
##  [63] "accel_arm_x"              "accel_arm_y"             
##  [65] "accel_arm_z"              "magnet_arm_x"            
##  [67] "magnet_arm_y"             "magnet_arm_z"            
##  [69] "kurtosis_roll_arm"        "kurtosis_picth_arm"      
##  [71] "kurtosis_yaw_arm"         "skewness_roll_arm"       
##  [73] "skewness_pitch_arm"       "skewness_yaw_arm"        
##  [75] "max_roll_arm"             "max_picth_arm"           
##  [77] "max_yaw_arm"              "min_roll_arm"            
##  [79] "min_pitch_arm"            "min_yaw_arm"             
##  [81] "amplitude_roll_arm"       "amplitude_pitch_arm"     
##  [83] "amplitude_yaw_arm"        "roll_dumbbell"           
##  [85] "pitch_dumbbell"           "yaw_dumbbell"            
##  [87] "kurtosis_roll_dumbbell"   "kurtosis_picth_dumbbell" 
##  [89] "kurtosis_yaw_dumbbell"    "skewness_roll_dumbbell"  
##  [91] "skewness_pitch_dumbbell"  "skewness_yaw_dumbbell"   
##  [93] "max_roll_dumbbell"        "max_picth_dumbbell"      
##  [95] "max_yaw_dumbbell"         "min_roll_dumbbell"       
##  [97] "min_pitch_dumbbell"       "min_yaw_dumbbell"        
##  [99] "amplitude_roll_dumbbell"  "amplitude_pitch_dumbbell"
## [101] "amplitude_yaw_dumbbell"   "total_accel_dumbbell"    
## [103] "var_accel_dumbbell"       "avg_roll_dumbbell"       
## [105] "stddev_roll_dumbbell"     "var_roll_dumbbell"       
## [107] "avg_pitch_dumbbell"       "stddev_pitch_dumbbell"   
## [109] "var_pitch_dumbbell"       "avg_yaw_dumbbell"        
## [111] "stddev_yaw_dumbbell"      "var_yaw_dumbbell"        
## [113] "gyros_dumbbell_x"         "gyros_dumbbell_y"        
## [115] "gyros_dumbbell_z"         "accel_dumbbell_x"        
## [117] "accel_dumbbell_y"         "accel_dumbbell_z"        
## [119] "magnet_dumbbell_x"        "magnet_dumbbell_y"       
## [121] "magnet_dumbbell_z"        "roll_forearm"            
## [123] "pitch_forearm"            "yaw_forearm"             
## [125] "kurtosis_roll_forearm"    "kurtosis_picth_forearm"  
## [127] "kurtosis_yaw_forearm"     "skewness_roll_forearm"   
## [129] "skewness_pitch_forearm"   "skewness_yaw_forearm"    
## [131] "max_roll_forearm"         "max_picth_forearm"       
## [133] "max_yaw_forearm"          "min_roll_forearm"        
## [135] "min_pitch_forearm"        "min_yaw_forearm"         
## [137] "amplitude_roll_forearm"   "amplitude_pitch_forearm" 
## [139] "amplitude_yaw_forearm"    "total_accel_forearm"     
## [141] "var_accel_forearm"        "avg_roll_forearm"        
## [143] "stddev_roll_forearm"      "var_roll_forearm"        
## [145] "avg_pitch_forearm"        "stddev_pitch_forearm"    
## [147] "var_pitch_forearm"        "avg_yaw_forearm"         
## [149] "stddev_yaw_forearm"       "var_yaw_forearm"         
## [151] "gyros_forearm_x"          "gyros_forearm_y"         
## [153] "gyros_forearm_z"          "accel_forearm_x"         
## [155] "accel_forearm_y"          "accel_forearm_z"         
## [157] "magnet_forearm_x"         "magnet_forearm_y"        
## [159] "magnet_forearm_z"         "classe"
```

```r
colnames(testSample)
```

```
##   [1] "X"                        "user_name"               
##   [3] "raw_timestamp_part_1"     "raw_timestamp_part_2"    
##   [5] "cvtd_timestamp"           "new_window"              
##   [7] "num_window"               "roll_belt"               
##   [9] "pitch_belt"               "yaw_belt"                
##  [11] "total_accel_belt"         "kurtosis_roll_belt"      
##  [13] "kurtosis_picth_belt"      "kurtosis_yaw_belt"       
##  [15] "skewness_roll_belt"       "skewness_roll_belt.1"    
##  [17] "skewness_yaw_belt"        "max_roll_belt"           
##  [19] "max_picth_belt"           "max_yaw_belt"            
##  [21] "min_roll_belt"            "min_pitch_belt"          
##  [23] "min_yaw_belt"             "amplitude_roll_belt"     
##  [25] "amplitude_pitch_belt"     "amplitude_yaw_belt"      
##  [27] "var_total_accel_belt"     "avg_roll_belt"           
##  [29] "stddev_roll_belt"         "var_roll_belt"           
##  [31] "avg_pitch_belt"           "stddev_pitch_belt"       
##  [33] "var_pitch_belt"           "avg_yaw_belt"            
##  [35] "stddev_yaw_belt"          "var_yaw_belt"            
##  [37] "gyros_belt_x"             "gyros_belt_y"            
##  [39] "gyros_belt_z"             "accel_belt_x"            
##  [41] "accel_belt_y"             "accel_belt_z"            
##  [43] "magnet_belt_x"            "magnet_belt_y"           
##  [45] "magnet_belt_z"            "roll_arm"                
##  [47] "pitch_arm"                "yaw_arm"                 
##  [49] "total_accel_arm"          "var_accel_arm"           
##  [51] "avg_roll_arm"             "stddev_roll_arm"         
##  [53] "var_roll_arm"             "avg_pitch_arm"           
##  [55] "stddev_pitch_arm"         "var_pitch_arm"           
##  [57] "avg_yaw_arm"              "stddev_yaw_arm"          
##  [59] "var_yaw_arm"              "gyros_arm_x"             
##  [61] "gyros_arm_y"              "gyros_arm_z"             
##  [63] "accel_arm_x"              "accel_arm_y"             
##  [65] "accel_arm_z"              "magnet_arm_x"            
##  [67] "magnet_arm_y"             "magnet_arm_z"            
##  [69] "kurtosis_roll_arm"        "kurtosis_picth_arm"      
##  [71] "kurtosis_yaw_arm"         "skewness_roll_arm"       
##  [73] "skewness_pitch_arm"       "skewness_yaw_arm"        
##  [75] "max_roll_arm"             "max_picth_arm"           
##  [77] "max_yaw_arm"              "min_roll_arm"            
##  [79] "min_pitch_arm"            "min_yaw_arm"             
##  [81] "amplitude_roll_arm"       "amplitude_pitch_arm"     
##  [83] "amplitude_yaw_arm"        "roll_dumbbell"           
##  [85] "pitch_dumbbell"           "yaw_dumbbell"            
##  [87] "kurtosis_roll_dumbbell"   "kurtosis_picth_dumbbell" 
##  [89] "kurtosis_yaw_dumbbell"    "skewness_roll_dumbbell"  
##  [91] "skewness_pitch_dumbbell"  "skewness_yaw_dumbbell"   
##  [93] "max_roll_dumbbell"        "max_picth_dumbbell"      
##  [95] "max_yaw_dumbbell"         "min_roll_dumbbell"       
##  [97] "min_pitch_dumbbell"       "min_yaw_dumbbell"        
##  [99] "amplitude_roll_dumbbell"  "amplitude_pitch_dumbbell"
## [101] "amplitude_yaw_dumbbell"   "total_accel_dumbbell"    
## [103] "var_accel_dumbbell"       "avg_roll_dumbbell"       
## [105] "stddev_roll_dumbbell"     "var_roll_dumbbell"       
## [107] "avg_pitch_dumbbell"       "stddev_pitch_dumbbell"   
## [109] "var_pitch_dumbbell"       "avg_yaw_dumbbell"        
## [111] "stddev_yaw_dumbbell"      "var_yaw_dumbbell"        
## [113] "gyros_dumbbell_x"         "gyros_dumbbell_y"        
## [115] "gyros_dumbbell_z"         "accel_dumbbell_x"        
## [117] "accel_dumbbell_y"         "accel_dumbbell_z"        
## [119] "magnet_dumbbell_x"        "magnet_dumbbell_y"       
## [121] "magnet_dumbbell_z"        "roll_forearm"            
## [123] "pitch_forearm"            "yaw_forearm"             
## [125] "kurtosis_roll_forearm"    "kurtosis_picth_forearm"  
## [127] "kurtosis_yaw_forearm"     "skewness_roll_forearm"   
## [129] "skewness_pitch_forearm"   "skewness_yaw_forearm"    
## [131] "max_roll_forearm"         "max_picth_forearm"       
## [133] "max_yaw_forearm"          "min_roll_forearm"        
## [135] "min_pitch_forearm"        "min_yaw_forearm"         
## [137] "amplitude_roll_forearm"   "amplitude_pitch_forearm" 
## [139] "amplitude_yaw_forearm"    "total_accel_forearm"     
## [141] "var_accel_forearm"        "avg_roll_forearm"        
## [143] "stddev_roll_forearm"      "var_roll_forearm"        
## [145] "avg_pitch_forearm"        "stddev_pitch_forearm"    
## [147] "var_pitch_forearm"        "avg_yaw_forearm"         
## [149] "stddev_yaw_forearm"       "var_yaw_forearm"         
## [151] "gyros_forearm_x"          "gyros_forearm_y"         
## [153] "gyros_forearm_z"          "accel_forearm_x"         
## [155] "accel_forearm_y"          "accel_forearm_z"         
## [157] "magnet_forearm_x"         "magnet_forearm_y"        
## [159] "magnet_forearm_z"         "problem_id"
```

```r
dim(trainSample)
```

```
## [1] 19622   160
```

```r
dim(testSample)
```

```
## [1]  20 160
```

```r
trainNULLS <- sapply(trainSample, function(y) sum(length(which(is.na(y)))))
testNULLS <- sapply(testSample, function(y) sum(length(which(is.na(y)))))

# There are many columns with all of the values being NAs, so deleting these would be wise
# I took a threshold of below 40% of the samples can be NAs
threshold_40_per <- dim(trainSample)[1]*0.4

trainNULLS <- as.data.frame(trainNULLS)
indexes <- which(trainNULLS<=threshold_40_per)

cleanTrain <- trainSample[, indexes]
cleanTest <- testSample[,indexes]

# Next we need to take away all the variables that have
# near-zero variance, one can do this with the function from caret
# We only need to take care of the variance in the train sample
# and we are to assume the testing sample corresponds to this
nzv <- nearZeroVar(cleanTrain)
```

```
## Error in nearZeroVar(cleanTrain): could not find function "nearZeroVar"
```

```r
filteredTrain <- na.omit(cleanTrain[,-nzv])
filteredTest <- na.omit(cleanTest[,-nzv])
filteredTrain$classe <- as.factor(cleanTest$classe)
```

```
## Error in `$<-.data.frame`(`*tmp*`, classe, value = structure(integer(0), .Label = character(0), class = "factor")): replacement has 0 rows, data has 19622
```

```r
filteredTest$classe <- as.factor(cleanTest$classe)
```

```
## Error in `$<-.data.frame`(`*tmp*`, classe, value = structure(integer(0), .Label = character(0), class = "factor")): replacement has 0 rows, data has 20
```

```r
summary(filteredTrain)
```

```
##        X            user_name    raw_timestamp_part_1 raw_timestamp_part_2
##  Min.   :    1   adelmo  :3892   Min.   :1.322e+09    Min.   :   294      
##  1st Qu.: 4906   carlitos:3112   1st Qu.:1.323e+09    1st Qu.:252912      
##  Median : 9812   charles :3536   Median :1.323e+09    Median :496380      
##  Mean   : 9812   eurico  :3070   Mean   :1.323e+09    Mean   :500656      
##  3rd Qu.:14717   jeremy  :3402   3rd Qu.:1.323e+09    3rd Qu.:751891      
##  Max.   :19622   pedro   :2610   Max.   :1.323e+09    Max.   :998801      
##                                                                           
##           cvtd_timestamp    num_window      roll_belt     
##  28/11/2011 14:14: 1498   Min.   :  1.0   Min.   :-28.90  
##  05/12/2011 11:24: 1497   1st Qu.:222.0   1st Qu.:  1.10  
##  30/11/2011 17:11: 1440   Median :424.0   Median :113.00  
##  05/12/2011 11:25: 1425   Mean   :430.6   Mean   : 64.41  
##  02/12/2011 14:57: 1380   3rd Qu.:644.0   3rd Qu.:123.00  
##  02/12/2011 13:34: 1375   Max.   :864.0   Max.   :162.00  
##  (Other)         :11007                                   
##    pitch_belt          yaw_belt       total_accel_belt  gyros_belt_x      
##  Min.   :-55.8000   Min.   :-180.00   Min.   : 0.00    Min.   :-1.040000  
##  1st Qu.:  1.7600   1st Qu.: -88.30   1st Qu.: 3.00    1st Qu.:-0.030000  
##  Median :  5.2800   Median : -13.00   Median :17.00    Median : 0.030000  
##  Mean   :  0.3053   Mean   : -11.21   Mean   :11.31    Mean   :-0.005592  
##  3rd Qu.: 14.9000   3rd Qu.:  12.90   3rd Qu.:18.00    3rd Qu.: 0.110000  
##  Max.   : 60.3000   Max.   : 179.00   Max.   :29.00    Max.   : 2.220000  
##                                                                           
##   gyros_belt_y       gyros_belt_z      accel_belt_x       accel_belt_y   
##  Min.   :-0.64000   Min.   :-1.4600   Min.   :-120.000   Min.   :-69.00  
##  1st Qu.: 0.00000   1st Qu.:-0.2000   1st Qu.: -21.000   1st Qu.:  3.00  
##  Median : 0.02000   Median :-0.1000   Median : -15.000   Median : 35.00  
##  Mean   : 0.03959   Mean   :-0.1305   Mean   :  -5.595   Mean   : 30.15  
##  3rd Qu.: 0.11000   3rd Qu.:-0.0200   3rd Qu.:  -5.000   3rd Qu.: 61.00  
##  Max.   : 0.64000   Max.   : 1.6200   Max.   :  85.000   Max.   :164.00  
##                                                                          
##   accel_belt_z     magnet_belt_x   magnet_belt_y   magnet_belt_z   
##  Min.   :-275.00   Min.   :-52.0   Min.   :354.0   Min.   :-623.0  
##  1st Qu.:-162.00   1st Qu.:  9.0   1st Qu.:581.0   1st Qu.:-375.0  
##  Median :-152.00   Median : 35.0   Median :601.0   Median :-320.0  
##  Mean   : -72.59   Mean   : 55.6   Mean   :593.7   Mean   :-345.5  
##  3rd Qu.:  27.00   3rd Qu.: 59.0   3rd Qu.:610.0   3rd Qu.:-306.0  
##  Max.   : 105.00   Max.   :485.0   Max.   :673.0   Max.   : 293.0  
##                                                                    
##     roll_arm         pitch_arm          yaw_arm          total_accel_arm
##  Min.   :-180.00   Min.   :-88.800   Min.   :-180.0000   Min.   : 1.00  
##  1st Qu.: -31.77   1st Qu.:-25.900   1st Qu.: -43.1000   1st Qu.:17.00  
##  Median :   0.00   Median :  0.000   Median :   0.0000   Median :27.00  
##  Mean   :  17.83   Mean   : -4.612   Mean   :  -0.6188   Mean   :25.51  
##  3rd Qu.:  77.30   3rd Qu.: 11.200   3rd Qu.:  45.8750   3rd Qu.:33.00  
##  Max.   : 180.00   Max.   : 88.500   Max.   : 180.0000   Max.   :66.00  
##                                                                         
##   gyros_arm_x        gyros_arm_y       gyros_arm_z       accel_arm_x     
##  Min.   :-6.37000   Min.   :-3.4400   Min.   :-2.3300   Min.   :-404.00  
##  1st Qu.:-1.33000   1st Qu.:-0.8000   1st Qu.:-0.0700   1st Qu.:-242.00  
##  Median : 0.08000   Median :-0.2400   Median : 0.2300   Median : -44.00  
##  Mean   : 0.04277   Mean   :-0.2571   Mean   : 0.2695   Mean   : -60.24  
##  3rd Qu.: 1.57000   3rd Qu.: 0.1400   3rd Qu.: 0.7200   3rd Qu.:  84.00  
##  Max.   : 4.87000   Max.   : 2.8400   Max.   : 3.0200   Max.   : 437.00  
##                                                                          
##   accel_arm_y      accel_arm_z       magnet_arm_x     magnet_arm_y   
##  Min.   :-318.0   Min.   :-636.00   Min.   :-584.0   Min.   :-392.0  
##  1st Qu.: -54.0   1st Qu.:-143.00   1st Qu.:-300.0   1st Qu.:  -9.0  
##  Median :  14.0   Median : -47.00   Median : 289.0   Median : 202.0  
##  Mean   :  32.6   Mean   : -71.25   Mean   : 191.7   Mean   : 156.6  
##  3rd Qu.: 139.0   3rd Qu.:  23.00   3rd Qu.: 637.0   3rd Qu.: 323.0  
##  Max.   : 308.0   Max.   : 292.00   Max.   : 782.0   Max.   : 583.0  
##                                                                      
##   magnet_arm_z    roll_dumbbell     pitch_dumbbell     yaw_dumbbell     
##  Min.   :-597.0   Min.   :-153.71   Min.   :-149.59   Min.   :-150.871  
##  1st Qu.: 131.2   1st Qu.: -18.49   1st Qu.: -40.89   1st Qu.: -77.644  
##  Median : 444.0   Median :  48.17   Median : -20.96   Median :  -3.324  
##  Mean   : 306.5   Mean   :  23.84   Mean   : -10.78   Mean   :   1.674  
##  3rd Qu.: 545.0   3rd Qu.:  67.61   3rd Qu.:  17.50   3rd Qu.:  79.643  
##  Max.   : 694.0   Max.   : 153.55   Max.   : 149.40   Max.   : 154.952  
##                                                                         
##  total_accel_dumbbell gyros_dumbbell_x    gyros_dumbbell_y  
##  Min.   : 0.00        Min.   :-204.0000   Min.   :-2.10000  
##  1st Qu.: 4.00        1st Qu.:  -0.0300   1st Qu.:-0.14000  
##  Median :10.00        Median :   0.1300   Median : 0.03000  
##  Mean   :13.72        Mean   :   0.1611   Mean   : 0.04606  
##  3rd Qu.:19.00        3rd Qu.:   0.3500   3rd Qu.: 0.21000  
##  Max.   :58.00        Max.   :   2.2200   Max.   :52.00000  
##                                                             
##  gyros_dumbbell_z  accel_dumbbell_x  accel_dumbbell_y  accel_dumbbell_z 
##  Min.   : -2.380   Min.   :-419.00   Min.   :-189.00   Min.   :-334.00  
##  1st Qu.: -0.310   1st Qu.: -50.00   1st Qu.:  -8.00   1st Qu.:-142.00  
##  Median : -0.130   Median :  -8.00   Median :  41.50   Median :  -1.00  
##  Mean   : -0.129   Mean   : -28.62   Mean   :  52.63   Mean   : -38.32  
##  3rd Qu.:  0.030   3rd Qu.:  11.00   3rd Qu.: 111.00   3rd Qu.:  38.00  
##  Max.   :317.000   Max.   : 235.00   Max.   : 315.00   Max.   : 318.00  
##                                                                         
##  magnet_dumbbell_x magnet_dumbbell_y magnet_dumbbell_z  roll_forearm      
##  Min.   :-643.0    Min.   :-3600     Min.   :-262.00   Min.   :-180.0000  
##  1st Qu.:-535.0    1st Qu.:  231     1st Qu.: -45.00   1st Qu.:  -0.7375  
##  Median :-479.0    Median :  311     Median :  13.00   Median :  21.7000  
##  Mean   :-328.5    Mean   :  221     Mean   :  46.05   Mean   :  33.8265  
##  3rd Qu.:-304.0    3rd Qu.:  390     3rd Qu.:  95.00   3rd Qu.: 140.0000  
##  Max.   : 592.0    Max.   :  633     Max.   : 452.00   Max.   : 180.0000  
##                                                                           
##  pitch_forearm     yaw_forearm      total_accel_forearm gyros_forearm_x  
##  Min.   :-72.50   Min.   :-180.00   Min.   :  0.00      Min.   :-22.000  
##  1st Qu.:  0.00   1st Qu.: -68.60   1st Qu.: 29.00      1st Qu.: -0.220  
##  Median :  9.24   Median :   0.00   Median : 36.00      Median :  0.050  
##  Mean   : 10.71   Mean   :  19.21   Mean   : 34.72      Mean   :  0.158  
##  3rd Qu.: 28.40   3rd Qu.: 110.00   3rd Qu.: 41.00      3rd Qu.:  0.560  
##  Max.   : 89.80   Max.   : 180.00   Max.   :108.00      Max.   :  3.970  
##                                                                          
##  gyros_forearm_y     gyros_forearm_z    accel_forearm_x   accel_forearm_y 
##  Min.   : -7.02000   Min.   : -8.0900   Min.   :-498.00   Min.   :-632.0  
##  1st Qu.: -1.46000   1st Qu.: -0.1800   1st Qu.:-178.00   1st Qu.:  57.0  
##  Median :  0.03000   Median :  0.0800   Median : -57.00   Median : 201.0  
##  Mean   :  0.07517   Mean   :  0.1512   Mean   : -61.65   Mean   : 163.7  
##  3rd Qu.:  1.62000   3rd Qu.:  0.4900   3rd Qu.:  76.00   3rd Qu.: 312.0  
##  Max.   :311.00000   Max.   :231.0000   Max.   : 477.00   Max.   : 923.0  
##                                                                           
##  accel_forearm_z   magnet_forearm_x  magnet_forearm_y magnet_forearm_z
##  Min.   :-446.00   Min.   :-1280.0   Min.   :-896.0   Min.   :-973.0  
##  1st Qu.:-182.00   1st Qu.: -616.0   1st Qu.:   2.0   1st Qu.: 191.0  
##  Median : -39.00   Median : -378.0   Median : 591.0   Median : 511.0  
##  Mean   : -55.29   Mean   : -312.6   Mean   : 380.1   Mean   : 393.6  
##  3rd Qu.:  26.00   3rd Qu.:  -73.0   3rd Qu.: 737.0   3rd Qu.: 653.0  
##  Max.   : 291.00   Max.   :  672.0   Max.   :1480.0   Max.   :1090.0  
##                                                                       
##  classe  
##  A:5580  
##  B:3797  
##  C:3422  
##  D:3216  
##  E:3607  
##          
## 
```

```r
# Next, normalizing and centering the variables would
# be good, since there seems to be large differences between the minimums
# and maximums of the variables
# The variable index is excluded since it's only a sequence along row numbers

preProcTrain <- preProcess(filteredTrain[,6:58], method=c("center", "scale"))
```

```
## Error in preProcess(filteredTrain[, 6:58], method = c("center", "scale")): could not find function "preProcess"
```

```r
normTrain <- predict(preProcTrain, filteredTrain[,6:58])
```

```
## Error in UseMethod("predict"): no applicable method for 'predict' applied to an object of class "preProcess"
```

```r
preProcTest <- preProcess(filteredTest[,6:58], method=c("center", "scale"))
```

```
## Error in preProcess(filteredTest[, 6:58], method = c("center", "scale")): could not find function "preProcess"
```

```r
normTest <- predict(preProcTest, filteredTest[,6:58])
```

```
## Error in UseMethod("predict"): no applicable method for 'predict' applied to an object of class "preProcess"
```

```r
# See the correlation between the numeric predictors
highlyCor <- findCorrelation(cor(normTrain), cutoff=.75, verbose=FALSE)
```

```
## Error in findCorrelation(cor(normTrain), cutoff = 0.75, verbose = FALSE): could not find function "findCorrelation"
```

```r
# And we want to remove these to not clutter the analysis
normTrain <- normTrain[,-highlyCor]
normTest <- normTest[, -highlyCor]

# We add back the classe (outcome) predictor
normTrain$classe <- as.factor(filteredTrain$classe)
normTest$classe <- as.factor(filteredTest$problem_id)



# Create a validation test partition / Data Slicing
CV <- createDataPartition(normTrain$classe, p=0.70, list=FALSE)
```

```
## Error in createDataPartition(normTrain$classe, p = 0.7, list = FALSE): could not find function "createDataPartition"
```

```r
normTrainT <- normTrain[CV,]
normValid <- normTrain[-CV,]
```


## Applying predictors:: Random Forest

```
## Error in confusionMatrix(predictRF, normValid$classe): could not find function "confusionMatrix"
```

```
## $positive
## NULL
## 
## $table
##           Reference
## Prediction    A    B    C    D    E
##          A 1674    1    0    0    0
##          B    0 1138    1    0    0
##          C    0    0 1024    7    0
##          D    0    0    1  957    0
##          E    0    0    0    0 1082
## 
## $overall
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##      0.9983008      0.9978506      0.9968773      0.9991849      0.2844520 
## AccuracyPValue  McnemarPValue 
##      0.0000000            NaN 
## 
## $byClass
##          Sensitivity Specificity Pos Pred Value Neg Pred Value Precision
## Class: A   1.0000000   0.9997625      0.9994030      1.0000000 0.9994030
## Class: B   0.9991220   0.9997893      0.9991220      0.9997893 0.9991220
## Class: C   0.9980507   0.9985594      0.9932105      0.9995880 0.9932105
## Class: D   0.9927386   0.9997968      0.9989562      0.9985793 0.9989562
## Class: E   1.0000000   1.0000000      1.0000000      1.0000000 1.0000000
##             Recall        F1 Prevalence Detection Rate
## Class: A 1.0000000 0.9997014  0.2844520      0.2844520
## Class: B 0.9991220 0.9991220  0.1935429      0.1933730
## Class: C 0.9980507 0.9956247  0.1743415      0.1740017
## Class: D 0.9927386 0.9958377  0.1638063      0.1626168
## Class: E 1.0000000 1.0000000  0.1838573      0.1838573
##          Detection Prevalence Balanced Accuracy
## Class: A            0.2846219         0.9998813
## Class: B            0.1935429         0.9994557
## Class: C            0.1751912         0.9983050
## Class: D            0.1627867         0.9962677
## Class: E            0.1838573         1.0000000
## 
## $mode
## [1] "sens_spec"
## 
## $dots
## list()
## 
## attr(,"class")
## [1] "confusionMatrix"
```

```
## Random Forest: 15.75 sec elapsed
```

```
## Error in varImp(RandomF): could not find function "varImp"
```

```
## Error in qplot(num_window, magnet_dumbbell_z, col = RF_Right, data = normValid, : could not find function "qplot"
```

```
##     left daughter right daughter           split var split point status
## 1               2              3          num_window -1.55355064      1
## 2               4              5   magnet_dumbbell_z -0.68270311      1
## 3               6              7       magnet_belt_y -1.09798364      1
## 4               8              9          num_window -1.64430955      1
## 5              10             11    magnet_forearm_x  0.54927576      1
## 6              12             13    magnet_forearm_y  0.91167549      1
## 7              14             15       roll_dumbbell  0.50303406      1
## 8               0              0                <NA>  0.00000000     -1
## 9               0              0                <NA>  0.00000000     -1
## 10             16             17    magnet_forearm_y  0.45228785      1
## 11              0              0                <NA>  0.00000000     -1
## 12             18             19   magnet_dumbbell_z  0.88198762      1
## 13             20             21       magnet_belt_x  1.92272540      1
## 14             22             23       magnet_belt_x -0.18854769      1
## 15             24             25       roll_dumbbell  0.72223602      1
## 16             26             27          num_window -1.63624209      1
## 17             28             29        yaw_dumbbell  1.54622514      1
## 18             30             31     accel_forearm_z  1.31355439      1
## 19             32             33    gyros_dumbbell_y -1.82143302      1
## 20              0              0                <NA>  0.00000000     -1
## 21              0              0                <NA>  0.00000000     -1
## 22             34             35   magnet_dumbbell_z -0.88989960      1
## 23             36             37     accel_forearm_z -0.99863535      1
## 24             38             39   magnet_dumbbell_z -0.11827129      1
## 25             40             41   magnet_dumbbell_z -0.32546778      1
## 26             42             43     accel_forearm_z -0.40252393      1
## 27              0              0                <NA>  0.00000000     -1
## 28              0              0                <NA>  0.00000000     -1
## 29             44             45     accel_forearm_x -0.45044864      1
## 30              0              0                <NA>  0.00000000     -1
## 31              0              0                <NA>  0.00000000     -1
## 32              0              0                <NA>  0.00000000     -1
## 33             46             47     accel_forearm_x  1.13598555      1
## 34             48             49            roll_arm -0.44375013      1
## 35             50             51      pitch_dumbbell  0.23787118      1
## 36             52             53 total_accel_forearm -0.31974279      1
## 37             54             55            roll_arm  0.16935840      1
## 38             56             57     accel_forearm_z  0.34171214      1
## 39             58             59          num_window -0.72461930      1
## 40             60             61            roll_arm -1.55449383      1
## 41             62             63        yaw_dumbbell -0.38295544      1
## 42              0              0                <NA>  0.00000000     -1
## 43              0              0                <NA>  0.00000000     -1
## 44              0              0                <NA>  0.00000000     -1
## 45              0              0                <NA>  0.00000000     -1
## 46              0              0                <NA>  0.00000000     -1
## 47             64             65          num_window -0.68428200      1
## 48             66             67   magnet_dumbbell_z -0.96134666      1
## 49             68             69            roll_arm  0.24977734      1
## 50             70             71     accel_forearm_x  1.32425348      1
## 51             72             73       roll_dumbbell -0.11583021      1
## 52             74             75    magnet_forearm_x  0.86055220      1
## 53             76             77     accel_forearm_x  1.43499933      1
## 54             78             79          num_window  0.02767119      1
## 55             80             81            roll_arm  0.91443647      1
## 56             82             83        magnet_arm_z -1.64868088      1
## 57             84             85      pitch_dumbbell -0.24941688      1
## 58              0              0                <NA>  0.00000000     -1
## 59             86             87       magnet_belt_y -0.03298674      1
## 60              0              0                <NA>  0.00000000     -1
## 61             88             89       magnet_belt_x -0.59366282      1
## 62             90             91     accel_forearm_z  1.17265533      1
## 63             92             93     accel_forearm_x  0.70130812      1
## 64              0              0                <NA>  0.00000000     -1
## 65              0              0                <NA>  0.00000000     -1
## 66             94             95    gyros_dumbbell_y -0.84600766      1
## 67             96             97     accel_forearm_x  0.71515135      1
## 68             98             99     gyros_forearm_z  0.43531636      1
## 69            100            101         gyros_arm_z -1.09275898      1
## 70            102            103          num_window -0.92428889      1
## 71            104            105    magnet_forearm_y  0.59167470      1
## 72            106            107     accel_forearm_x  1.09722451      1
## 73            108            109       roll_dumbbell  0.30554877      1
## 74            110            111       magnet_belt_x  0.20877677      1
## 75              0              0                <NA>  0.00000000     -1
## 76            112            113       magnet_belt_y  0.70970846      1
## 77              0              0                <NA>  0.00000000     -1
## 78            114            115          num_window -1.03521644      1
## 79            116            117          num_window  0.54398853      1
## 80            118            119       roll_dumbbell  0.25901140      1
## 81            120            121       magnet_belt_y -0.31324908      1
## 82              0              0                <NA>  0.00000000     -1
## 83            122            123      pitch_dumbbell  0.85900170      1
## 84            124            125        magnet_arm_z -1.52468331      1
## 85            126            127          num_window  0.71542202      1
## 86            128            129          num_window -0.55721954      1
## 87            130            131    magnet_forearm_y -1.50109121      1
## 88            132            133       magnet_belt_y  0.35938053      1
## 89            134            135       roll_dumbbell  1.06777282      1
## 90            136            137       roll_dumbbell  0.83036420      1
## 91            138            139            roll_arm  0.79140236      1
## 92            140            141    magnet_forearm_x  1.21938476      1
## 93            142            143       magnet_belt_y -0.17311791      1
## 94              0              0                <NA>  0.00000000     -1
## 95            144            145       magnet_belt_y  0.35938053      1
## 96            146            147      pitch_dumbbell  1.21510915      1
## 97            148            149     accel_forearm_z -0.74935239      1
## 98            150            151         gyros_arm_y  1.29444798      1
## 99              0              0                <NA>  0.00000000     -1
## 100           152            153    magnet_forearm_x -1.16562683      1
## 101           154            155          num_window -1.46077487      1
## 102           156            157    gyros_dumbbell_y -0.84600766      1
## 103           158            159   magnet_dumbbell_z -0.52551956      1
## 104           160            161     accel_forearm_z -0.94805619      1
## 105           162            163         gyros_arm_z -0.96621533      1
## 106           164            165         gyros_arm_y  1.31206621      1
## 107           166            167          num_window -1.44060622      1
## 108           168            169     accel_forearm_z -0.72045002      1
## 109           170            171    gyros_dumbbell_y  0.77696897      1
## 110           172            173     accel_forearm_z -1.12147042      1
## 111             0              0                <NA>  0.00000000     -1
## 112           174            175    magnet_forearm_x  0.37058003      1
## 113           176            177     accel_forearm_x -0.44491135      1
## 114             0              0                <NA>  0.00000000     -1
## 115             0              0                <NA>  0.00000000     -1
## 116           178            179    magnet_forearm_y -0.02476854      1
## 117           180            181          num_window  0.73357380      1
## 118           182            183     gyros_forearm_z -0.32559174      1
## 119           184            185     accel_forearm_z  1.60619090      1
## 120           186            187       magnet_belt_x -0.11064093      1
## 121           188            189      pitch_dumbbell  0.49069751      1
## 122           190            191       magnet_belt_x -0.34436120      1
## 123           192            193            roll_arm -1.64453555      1
## 124             0              0                <NA>  0.00000000     -1
## 125           194            195    magnet_forearm_y  0.05277766      1
## 126           196            197       magnet_belt_x  0.73854271      1
## 127           198            199        yaw_dumbbell -0.34655276      1
## 128           200            201    gyros_dumbbell_y -0.15747212      1
## 129           202            203          num_window  1.48183056      1
## 130           204            205    gyros_dumbbell_y -1.09191322      1
## 131           206            207          num_window  0.19103722      1
## 132           208            209 total_accel_forearm  0.97302338      1
## 133           210            211 total_accel_forearm  0.37636207      1
## 134           212            213          num_window  1.18333460      1
## 135           214            215    gyros_dumbbell_y -0.50993675      1
## 136           216            217     accel_forearm_x -0.07945007      1
## 137           218            219         gyros_arm_z -2.07799169      1
## 138           220            221    magnet_forearm_y  1.25032663      1
## 139             0              0                <NA>  0.00000000     -1
## 140           222            223          num_window -0.12964424      1
## 141           224            225       magnet_belt_x -0.46901201      1
## 142           226            227    gyros_dumbbell_y  0.34253583      1
## 143           228            229         gyros_arm_y  0.56622757      1
## 144           230            231     gyros_forearm_z  0.53506086      1
## 145           232            233       roll_dumbbell -0.39410021      1
## 146           234            235        yaw_dumbbell -0.95101691      1
## 147           236            237        gyros_belt_y  0.13308906      1
## 148             0              0                <NA>  0.00000000     -1
## 149             0              0                <NA>  0.00000000     -1
## 150           238            239       magnet_belt_x -0.94424322      1
## 151           240            241   magnet_dumbbell_z -1.02207667      1
## 152             0              0                <NA>  0.00000000     -1
## 153           242            243       magnet_belt_x -0.74947633      1
## 154             0              0                <NA>  0.00000000     -1
## 155           244            245        gyros_belt_y  0.06917947      1
## 156           246            247            roll_arm  1.00516554      1
## 157           248            249          num_window -1.16227891      1
## 158           250            251          num_window -0.33939816      1
## 159           252            253   magnet_dumbbell_z  1.67147770      1
## 160             0              0                <NA>  0.00000000     -1
## 161           254            255        magnet_arm_z  0.15464188      1
## 162           256            257 total_accel_forearm -0.22029924      1
## 163           258            259    magnet_forearm_y  0.72320877      1
## 164           260            261          num_window -1.45674114      1
## 165           262            263         gyros_arm_y  2.86834369      1
## 166             0              0                <NA>  0.00000000     -1
##     prediction
## 1         <NA>
## 2         <NA>
## 3         <NA>
## 4         <NA>
## 5         <NA>
## 6         <NA>
## 7         <NA>
## 8            A
## 9            E
## 10        <NA>
## 11           E
## 12        <NA>
## 13        <NA>
## 14        <NA>
## 15        <NA>
## 16        <NA>
## 17        <NA>
## 18        <NA>
## 19        <NA>
## 20           D
## 21           E
## 22        <NA>
## 23        <NA>
## 24        <NA>
## 25        <NA>
## 26        <NA>
## 27           E
## 28           E
## 29        <NA>
## 30           E
## 31           D
## 32           E
## 33        <NA>
## 34        <NA>
## 35        <NA>
## 36        <NA>
## 37        <NA>
## 38        <NA>
## 39        <NA>
## 40        <NA>
## 41        <NA>
## 42           D
## 43           E
## 44           D
## 45           E
## 46           D
## 47        <NA>
## 48        <NA>
## 49        <NA>
## 50        <NA>
## 51        <NA>
## 52        <NA>
## 53        <NA>
## 54        <NA>
## 55        <NA>
## 56        <NA>
## 57        <NA>
## 58           A
## 59        <NA>
## 60           A
## 61        <NA>
## 62        <NA>
## 63        <NA>
## 64           B
## 65           D
## 66        <NA>
## 67        <NA>
## 68        <NA>
## 69        <NA>
## 70        <NA>
## 71        <NA>
## 72        <NA>
## 73        <NA>
## 74        <NA>
## 75           A
## 76        <NA>
## 77           A
## 78        <NA>
## 79        <NA>
## 80        <NA>
## 81        <NA>
## 82           E
## 83        <NA>
## 84        <NA>
## 85        <NA>
## 86        <NA>
## 87        <NA>
## 88        <NA>
## 89        <NA>
## 90        <NA>
## 91        <NA>
## 92        <NA>
## 93        <NA>
## 94           B
## 95        <NA>
## 96        <NA>
## 97        <NA>
## 98        <NA>
## 99           B
## 100       <NA>
## 101       <NA>
## 102       <NA>
## 103       <NA>
## 104       <NA>
## 105       <NA>
## 106       <NA>
## 107       <NA>
## 108       <NA>
## 109       <NA>
## 110       <NA>
## 111          E
## 112       <NA>
## 113       <NA>
## 114          E
## 115          A
## 116       <NA>
## 117       <NA>
## 118       <NA>
## 119       <NA>
## 120       <NA>
## 121       <NA>
## 122       <NA>
## 123       <NA>
## 124          B
## 125       <NA>
## 126       <NA>
## 127       <NA>
## 128       <NA>
## 129       <NA>
## 130       <NA>
## 131       <NA>
## 132       <NA>
## 133       <NA>
## 134       <NA>
## 135       <NA>
## 136       <NA>
## 137       <NA>
## 138       <NA>
## 139          C
## 140       <NA>
## 141       <NA>
## 142       <NA>
## 143       <NA>
## 144       <NA>
## 145       <NA>
## 146       <NA>
## 147       <NA>
## 148          A
## 149          B
## 150       <NA>
## 151       <NA>
## 152          C
## 153       <NA>
## 154          A
## 155       <NA>
## 156       <NA>
## 157       <NA>
## 158       <NA>
## 159       <NA>
## 160          A
## 161       <NA>
## 162       <NA>
## 163       <NA>
## 164       <NA>
## 165       <NA>
## 166          A
##  [ reached 'max' / getOption("max.print") -- omitted 1641 rows ]
```

## Applying predictors::SVM


```
## Error in confusionMatrix(predictSVM, normValid$classe): could not find function "confusionMatrix"
```

```
## $positive
## NULL
## 
## $table
##           Reference
## Prediction    A    B    C    D    E
##          A 1658   89    4    2    1
##          B    4 1018   21    0    7
##          C   11   31  963  106   13
##          D    0    0   36  855   29
##          E    1    1    2    1 1032
## 
## $overall
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##      0.9389975      0.9227214      0.9325769      0.9449784      0.2844520 
## AccuracyPValue  McnemarPValue 
##      0.0000000            NaN 
## 
## $byClass
##          Sensitivity Specificity Pos Pred Value Neg Pred Value Precision
## Class: A   0.9904421   0.9772026      0.9452680      0.9961268 0.9452680
## Class: B   0.8937665   0.9932575      0.9695238      0.9749741 0.9695238
## Class: C   0.9385965   0.9668656      0.8567616      0.9867675 0.8567616
## Class: D   0.8869295   0.9867913      0.9293478      0.9780463 0.9293478
## Class: E   0.9537893   0.9989590      0.9951784      0.9896865 0.9951784
##             Recall        F1 Prevalence Detection Rate
## Class: A 0.9904421 0.9673279  0.2844520      0.2817332
## Class: B 0.8937665 0.9301051  0.1935429      0.1729822
## Class: C 0.9385965 0.8958140  0.1743415      0.1636364
## Class: D 0.8869295 0.9076433  0.1638063      0.1452846
## Class: E 0.9537893 0.9740444  0.1838573      0.1753611
##          Detection Prevalence Balanced Accuracy
## Class: A            0.2980459         0.9838223
## Class: B            0.1784197         0.9435120
## Class: C            0.1909941         0.9527311
## Class: D            0.1563297         0.9368604
## Class: E            0.1762107         0.9763741
## 
## $mode
## [1] "sens_spec"
## 
## $dots
## list()
## 
## attr(,"class")
## [1] "confusionMatrix"
```

```
## SVM: 32.67 sec elapsed
```

```
## Error in qplot(num_window, magnet_dumbbell_z, col = SVM_Right, data = normValid, : could not find function "qplot"
```

# We can see that Random Forest creates a very accurate prediction of the output class with 0.998 with an out of sample error 0.0003960396. 
# Nevertheless, both algorithms perform very well in predicting the outcome at 0.939 and above (SVM is 0.9397) 
# The running time of the algorithms is much worse with SVM taking 43 seconds, and the Random Forest algorithm taking 24 seconds

# Last thing is to apply the model to produce the results of the test data with the test data
# When I applied the RF model to the test data set, the actual results were much worse at the testing dataset

## RESULTS


```
##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
##  B  A  C  A  D  E  C  B  A  A  B  C  D  C  E  E  E  B  B  D 
## Levels: A B C D E
```

```
##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
##  B  A  C  A  A  E  D  D  A  A  A  C  B  A  E  E  A  B  D  B 
## Levels: A B C D E
```

# The validation test got good results
# But if the algorithm performs better on the test set (that we know the results for)
# It is better to use the SVM model
