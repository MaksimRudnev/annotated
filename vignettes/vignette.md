---
title: "Adding dynamic annotations to console outputs"
author: "Maksim Rudnev"
date: "`r Sys.Date()`"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Adding annotations to console outputs}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---


1. [Create annotations](#create-annotations)
2. [Getting your output annotated](#getting-your-output-annotated)
3. [Technical details](#technical-details)


## 1. Create annotations


One need to create an annotation or use the existing one. If you have an annotation files, check the next section - of how to use it.

Annotation is best created with a real examplary output. So create some call and out it unquoted to the `create_annotation()` function.


```
> library(lavaan)
> create_annotation(cfa('f1 =~ x1 + x2 + x3 + x4', HolzingerSwineford1939))
```

After this, you will be carried through a long interactive Q&A asking you about each line of the output and how it should be annotated. If you opt for conditional annotations, the conditions should be also added. Conditions are pieces of R code that should return *TRUE* or *FALSE*, they may contain an outputted object referred to as `ob`, for example, `nobs(ob)<100` is a proper condition to show an annotations if the model was fitted to sample less than 100.

<details>
  <summary>
    Click to see the full example of interactive Q&A.
  </summary>

```

[1] lavaan 0.6-2 ended normally after 22 iterations
[2] 
[3]   Optimization method                           NLMINB
[4]   Number of free parameters                          8
[5] 
[6]   Number of observations                           301
[7] 
[8]   Estimator                                         ML
[9]   Model Fit Test Statistic                      11.798
[10]   Degrees of freedom                                 2
[11]   P-value (Chi-square)                           0.003
Above is the output you are going to annotate.

---------------------------------------------------------------------------

Would you like to make annotations conditional?
You will be prompted to add conditions for every line of current output. 

1: Yes
2: No

Selection: 1

---------------------------------------------------------------------------

Every condition will be saved as an R script. You can refer to the output object of the initial call as 'ob'.
Now I am going to return it line by line so you can add your annotations.
If you do not want to comment the line, just press 'Enter'.
---------------------------------------------------------------------------

[1] lavaan 0.6-2 ended normally after 22 iterations
Is it conditional? 

1: Yes
2: No

Selection: 1
---------------------------------------------------------------------------

What is condition 1? lavInspect(m, "optim")$converged

---------------------------------------------------------------------------

Annotate (condition 1): The solution was found. Also note the version of the software.

---------------------------------------------------------------------------

What is condition 2? !lavInspect(m, "optim")$converged

---------------------------------------------------------------------------

Annotate (condition 2): Beware! The solution was NOT found for this model.

---------------------------------------------------------------------------

Need to add another (last) condition? 

1: Yes
2: No

Selection: 2
---------------------------------------------------------------------------

[2] 
[3]   Optimization method                           NLMINB
Is it conditional? 

1: Yes
2: No

Selection: 2
---------------------------------------------------------------------------

Annotate: 
[4]   Number of free parameters                          8
Is it conditional? 

1: Yes
2: No

Selection: 2

---------------------------------------------------------------------------

Annotate: 
[5] 
[6]   Number of observations                           301
Is it conditional? 

1: Yes
2: No

Selection: 1

---------------------------------------------------------------------------

What is condition 1? nobs(ob)>500

---------------------------------------------------------------------------

Annotate (condition 1): You have relatively large sample.

---------------------------------------------------------------------------

What is condition 2? nobs(ob)<500

---------------------------------------------------------------------------

Annotate (condition 2): You have relatively small sample. Treat chi-square seriously.

---------------------------------------------------------------------------

Need to add another (last) condition? 

1: Yes
2: No

Selection: 2

---------------------------------------------------------------------------

[7] 
[8]   Estimator                                         ML
Is it conditional? 

1: Yes
2: No

Selection: 2

---------------------------------------------------------------------------

Annotate: 
[9]   Model Fit Test Statistic                      11.798
Is it conditional? 

1: Yes
2: No

Selection: 2

---------------------------------------------------------------------------

Annotate: this is actually Chi-square.

---------------------------------------------------------------------------

[10]   Degrees of freedom                                 2
Is it conditional? 

1: Yes
2: No

Selection: 2

---------------------------------------------------------------------------

Annotate: 

---------------------------------------------------------------------------

[11]   P-value (Chi-square)                           0.003
Is it conditional? 

1: Yes
2: No

Selection: 2

---------------------------------------------------------------------------

Annotate: 

---------------------------------------------------------------------------

It's over. Press Enter and have a look at the annotated. 

---------------------------------------------------------------------------

lavaan 0.6-2 ended normally after 22 iterations 
CONDITION: lavInspect(m, "optim")$converged
ANNOTATION: The solution was found. Also note the version of the software.
CONDITION: !lavInspect(m, "optim")$converged
ANNOTATION: Beware! The solution was NOT found for this model.

 

  Optimization method                           NLMINB 

  Number of free parameters                          8 

 

  Number of observations                           301 
CONDITION: nobs(ob)>500
ANNOTATION: You have relatively large sample.
CONDITION: nobs(ob)<500
ANNOTATION: You have relatively small sample. Tread chi-square seriously.

 

  Estimator                                         ML 

  Model Fit Test Statistic                      11.798 
CONDITION: 
ANNOTATION: this is actually Chi-square.

  Degrees of freedom                                 2 

  P-value (Chi-square)                           0.003 
  
---------------------------------------------------------------------------

How do I save the annotations?

1: + Save as R-script for future use with 'annotate' function.
2:   Save as html as the static example.

Enter one or more numbers separated by spaces, or an empty line to cancel
1: 1 2

---------------------------------------------------------------------------

To which functions this annotation is applicable? separate by spaces e.g., lavaan::cfa cfa:  cfa sem lavaan

---------------------------------------------------------------------------

Give this file a name with .R extension: example.R

---------------------------------------------------------------------------

Entries are saved to example.R. You can manually edit them later.
all done.

```

</details>


After the tutorial finished, two files can be saved: **example.R** (see *Technical details* below) that can be used for dynamic annotation, and **example.html** that shows annotation to this specific example with conditions (as following):


<pre>lavaan 0.6-2 ended normally after 22 iterations<br><span style='color:red;'>CONDITION1: lavInspect(ob, "optim")$converged<br>ANNOTATION: The solution was found. Also note the version of the software.<br>CONDITION2: !lavInspect(ob, "optim")$converged<br>ANNOTATION: Beware! The solution was NOT found for this model.<br></span><br><span style='color:red;'></span>  Optimization method                           NLMINB<br><span style='color:red;'></span>  Number of free parameters                          8<br><span style='color:red;'></span><br><span style='color:red;'></span>  Number of observations                           301<br><span style='color:red;'>CONDITION1: nobs(ob)>500<br>ANNOTATION: You have relatively large sample.<br>CONDITION2: nobs(ob)<500<br>ANNOTATION: You have relatively small sample. Treat chi-square seriously.<br></span><br><span style='color:red;'></span>  Estimator                                         ML<br><span style='color:red;'></span>  Model Fit Test Statistic                      11.798<br><span style='color:red;'>ANNOTATION: this is actually Chi-square.<br></span>  Degrees of freedom                                 2<br><span style='color:red;'></span>  P-value (Chi-square)                           0.003<br><span style='color:red;'></span></pre>



## 2. Getting your output annotated

First, tell R where the libarary with annotations is located.

<pre>
<span style="color:blue">
options(annotated.source="example.R")
</span></pre>


Second, make a call to a function, in a usual way, but embraced by `annotated()`. Note that this would return the usual output of `cfa()`, so you can save it to some object for further examination. For example, later you can check `summary(m)`

<pre>
 <span style="color:blue"> 
m<-annotated(cfa('f1 =~ x1 + x2 + x3 + x4', HolzingerSwineford1939))</span>

lavaan 0.6-2 ended normally after 22 iterations
 <span style="color:red">The solution was found. Also note the version of the software. </span>

  Optimization method                           NLMINB
  Number of free parameters                          8

  Number of observations                           301
 <span style="color:red">You have relatively small sample. Treat chi-square seriously. </span>

  Estimator                                         ML
  Model Fit Test Statistic                      11.798
 <span style="color:red">this is actually Chi-square.</span>
  Degrees of freedom                                 2
  P-value (Chi-square)                           0.003
</pre>

Likewise,  we can use different model to get annotations (not the change in the annotation for the sample size): 
<pre>
<span style="color:blue">m1<-annotated(cfa('f1 =~ x5 + x6 + x7', rbind(HolzingerSwineford1939, HolzingerSwineford1939)  )) </span>

lavaan 0.6-2 ended normally after 25 iterations
 <span style="color:red">The solution was found. Also note the version of the software. </span>
  Optimization method                           NLMINB
  Number of free parameters                          6

  Number of observations                           602
 <span style="color:red">You have relatively large sample.  </span>

  Estimator                                         ML
  Model Fit Test Statistic                       0.000
<span style="color:red">this is actually Chi-square. </span>
  Degrees of freedom                                 0
 </pre>
  
  

## 3. Technical details

`example.R` looks like this - it is a generated R code that creates a list with three elements: `annotation`, `conditions`, and `methods.for`. There are as many elements in each of `annotation` and `conditions` as long the the print output is. One can manually edit it.

```
list(
  annotation=list(
    cond1=c('The solution was found. Also note the version of the software.','','','','',
            'You have relatively large sample.', '','',
            'this is actually Chi-square.', '',''),
    cond2=c('Beware! The solution was NOT found for this model.','','','','',
            'You have relatively small sample. Treat chi-square seriously.','','','','',''),
    cond3=c('','','','','','','','','','','')),
  
  conditions=list(
    cond1=c('lavInspect(ob, "optim")$converged','','','','',
            'nobs(ob)>500','','','','',''),
    cond2=c('!lavInspect(ob, "optim")$converged','','','','',
            'nobs(ob)<500','','','','',''),
    cond3=c('','','','','','','','','','','')),
  
  methods.for=c("cfa", "sem", "lavaan")
  
  )
```


