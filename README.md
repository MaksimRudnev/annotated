## R package `annotated` 

Quickly annotate console outputs of specific functions and make them conditional of the values of the object. For example, if you can notice that p-values are not very meaningful in linear regression if the sample size used in the specific model was above 1 million. Unconditional annotations can also be useful.

Install via `devtools::install_github("maksimrudnev/annotated")`.

## How to use

### For instructors:

1. Use interactive `create_annotation()` to quickly create annotations with an expemplary call to a function. It will save the result to an annotation file or provide html file woth all the annotations added to this example.
2. You can edit the annotation file by hand, as is is a source R script.

### For students:

1. Set the location of the annotation file, by specifying `annotated.source` option, for example, a folder "collection": `options(annotated.source="collection")`.
2. Run `annotated(some unquoted call to a function)` and enjoy.

See vignette: https://github.com/MaksimRudnev/annotated/blob/master/vignettes/my-vignette.Rmd

## Pros

Speed and simplicity.
Annotations are processed as warnings, so in R studio and in raw R they are printed in different color, which makes them easier to understand.

## Recommendation

Use with R Studio.

It is a relative of [xplain](http://www.zuckarelli.de/xplain/index.html) package, but makes creation of annotations and use of them much **much** easier.

## Limits

Currently limited to console, and 3 conditions.

Unrelated to `ggplot2`'s `annotate` function.
