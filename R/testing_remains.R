#'


# return.call<- function(...,language="EN") as.character(sys.call())[2]
#
#
# return.call(mean(1,2,3,4), language="English")


#
# #Ok
# call='lavaan::cfa("trust =~ ppltrst + pplfair + pplhlp + trstprl", dt)'
#
# #message/warning
# call='lavaan::cfa("trust =~ ppltrst + pplfair", dt)'
#
# #error
# call='lavaan::cfa("trust =~ ppltrst + pplfair + pplhelp + trstprl", dt)'


# annotate <- function(..., language="EN") {
#   require("lavaan", quietly=T)
#
#   # List of annotations
#   if(language=="EN") cat("I use English language. You can always switch to Russian."); ru=F
#
# a <- list(
#   warning.message.lavaan = "Something went wrong in lavaan: \n",
#   ann.title = "\nANNOTATED OUTPUT
#         \nIn  black you can see a usual output, in red - added by the annotator\n\n",
#   ssum.1line="version of lavaan and how many iterations of maximum likelihood were used to find parameter estimates",
#   ssum.2line="how the parameters were found",
#   ssum.3line="this are how many parameters were estimated (are in the model)",
#   ssum.4_5line = "'Used' means what left after deleting all the missings, \n 'Total' is how many observations were in the initial dataset",
#   ssum.6line = "Estimator is an algortihm that looks for the values of parameters, most frequently it is Maximum Likelihood [ML]",
#
#   ssum.7line = "Model Fit Test Statistic is Chi-square of the difference between model-implied and empirical covariance matrix",
#   ssum.7line.chi.0 =  "It is less than 10 which is quite small, but doesn't mean much by itself",
#   ssum.8line = "how many parameters can potentially be added to the model",
#   ssum.9line.df0 = "There are zero degrees of freedom, the model is just-identified, it  exactly fits to the data, chi-square is automatically zero, but it doesn't mean the model is good, it just means that one cannot know a real quality of this model.",
#   ssum9line.p00n500 = "Chi-square is always significant with large samples, e.g. more than 500, which is the case. Check the other fit measures using fitMeasures(yourmodel) or summary(yourmodel)",
#   ssum9line.p00 = "Chi-square is statistically sigificant, that is, smaller than 0.05, it means model does not fit data very well",
#   ssum9line.p05 = "Yay! The model fits data very well, because chi-square is not different from zero."
#   )
#
#
#
#
#
#
#
#
#
#   # Add annotations
#   if(  function.used %in% c("cfa", "lavaan::cfa", "lavaan", "sem") ) {
#
# annotation<- c(   o=output[1],  #lavaan 0.6-2 ended normally after 30 iterations
#                           a[["ssum.1line"]],
#                   o=output[2],
#                   o=output[3],  # Optimization method                           NLMINB
#                          a[["ssum.2line"]],
#                   o=output[4],  # Number of free parameters                          8
#                          a[["ssum.3line"]],
#                   o=output[5],
#                   o=output[6], #                                                 Used       Total
#                   o=output[7], # Number of observations                           966        1000
#                          a[["ssum.4_5line"]],
#                   o=output[8],
#                   o=output[9], # Estimator                                         ML
#                         a[["ssum.6line"]],
#                   o=output[10], #  Model Fit Test Statistic                       4.136"
#                          paste(a[["ssum.7line"]],
#                                ifelse(fitMeasures(mdl)["chisq"]<10, a[["ssum.7line.chi.0"]], "")),
#
#                   o=output[11], #Degrees of freedom                                 2
#                          a[["ssum.8line"]],
#
#
#
#                   o=output[12], #P-value (Chi-square)                           0.126
#                    if(fitMeasures(mdl)[["df"]]>0) {
#                         ifelse(fitMeasures(mdl)["pvalue"]<0.05 && nobs(mdl) > 500,
#                             a[["ssum9line.p00n500"]],
#                             ifelse(fitMeasures(mdl)["pvalue"]<0.05,
#                                    a[["ssum9line.p00"]],
#                                    a[["ssum9line.p05"]]))
#                    } else {
#                      #Minimum Function Value               0.0000000000000
#                      a[["ssum.9line.df0"]]
#                    }
#
# )
#
# # Print output
# message(a[["title"]])
# for(i in 1:length(annotation)) if(names(annotation[i])=="o") cat(annotation[i], "\n") else message(annotation[i])
#
#
# invisible(mdl)
#
#
#
#   } else {
#
#     message(c(" 🤔  I don't know how to deal with function: ", function.used))
#
# }
#
#
#
# }


#methods(class="lavaan")


#


#' Get printed output with s
#'
#'
# get_printed_row_numbers <- function(..., as.text=F) {
#
#   if(!as.text) call <- as.character(sys.call())[2]
#   printed<-capture.output(eval(parse(text=call)))
#   print(printed, quote=F)
#
# }
#
# get_printed_row_numbers(lavaan::cfa("trust =~ ppltrst + pplfair +pplhlp+ trstprl" , dt))
# get_printed_row_numbers('lavaan::cfa("trust =~ ppltrst + pplfair +pplhlp+ trstprl" , dt)')
#
# printed<-capture.output(lavaan::cfa("trust =~ ppltrst + pplfair +pplhlp+ trstprl" , dt))
#
# call. <- 'lavaan::cfa("trust =~ ppltrst + pplfair +pplhlp+ trstprl" , dt)'







# Usage #####





#
#
#
# lavaan:::short.summary(mdl)
#
# summary(mdl, fit.measures=TRUE)
#
# annotate(mean(1,2,3))
#
# <-tryCatch(lavaan::cfa("trust =~ ppltrst + pplfair" + pplhlp, dt), error=function(e) print("There was an error in lavaan"), finally = print("finally"))

