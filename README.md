## R package `annotated` 

Quickly annotate console outputs of specific functions and let your students learn from it. Annotations may dynamically change depending on the input and fitted objects. For example, if R-squared of the linear model is below .1 you can add a warning message explaining that it may indicate a bad model. Creating conditional annotations is easy and resembles Rmarkdown logic. Use of annotation libraries is extremely straightforward.

Install via `devtools::install_github("maksimrudnev/annotated")`.

## How to use

### For instructors:

1. Use interactive `create_annotation()` to quickly create annotations with an exemplary call to a function. It will save the result to an annotation file.

**Annotations** are short fragments of text to help students (or yourself) to understand the output. They may contain fragments of R code enclosed in backticks, Rmarkdown style, and may use reference to `ob` - the object returned by the target function. For example: ```" by the way, CFI is `fitMeasures(ob)["cfi"]` "```. 

Consider using package `crayon` to format annotations, for example: ```"No, you `crayon::bold("can not")` interpret these coefficients."```.

**Conditions** are optional pieces of R code that should return *TRUE* or *FALSE*, for example, `nobs(ob)<100` for condition 1, and `nobs(ob)>=100` for condition 2.

2. You can edit the annotation file by hand, as it is a source R script.

### For students:

1. Set the location of the annotation library by specifying `annotated.source` option, for example, a folder "collection": `options(annotated.source="collection")`.
2. Run `annotated(some unquoted call to a function)` and enjoy.

See vignette: https://github.com/MaksimRudnev/annotated/blob/master/vignettes/vignette.html

## Pros

Speed and simplicity.
Annotations are printed in a different (red) color, which makes them easier to understand.


## Note

The coloring is based on package `crayon` which works only in RStudio > 1.1. Make sure to update RStudio.

## Limitations

Currently limited to the Console use, and 3 conditions. Not appropriate for non-interactive use.

## Disclaimer 

It is unrelated to `ggplot2`'s `annotate` function.

It is a younger sister of [xplain](http://www.zuckarelli.de/xplain/index.html) package, but it makes creation and use of annotations much faster, and **much** easier.
