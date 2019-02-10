#This is a script generated with an R package 'annotated'. It is supposed to be used as a library of annotations within the function 'annotated()'. Created on: 2019-02-10 20:48:35

list(
 annotation = list(
  cond1 = c(
    "",
    "",
    "This is a full call, including the arguments that weren't specified; those arguments were set to their default values.",
    "",
    "These are unstandardized regression coefficients.",
    "",
    "The sample size is below 100 observations. `bold('The estimated parameters can be underpowered!')`",
    ""),
  cond2 = c(
    "",
    "",
    "",
    "",
    "",
    "",
    "The sample size is 100 or higher.",
    ""),
  cond3 = c(
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "")
 ),
 conditions = list(
  cond1 = c(
    "",
    "",
    "",
    "",
    "",
    "",
    "nobs(ob)<100",
    ""),
  cond2 = c(
    "",
    "",
    "",
    "",
    "",
    "",
    "nobs(ob)>99",
    ""),
  cond3 = c(
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "")
 ),
methods.for = "lm"
)
