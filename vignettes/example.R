list(
  annotation=list(
    cond1=c('The solution was found. Also note the version of the software.','','','','',
            'You have relatively large sample.', '','',
            'this is actually Chi-square.its value is `round(fitMeasures(ob)["chisq"],1)` with `fitMeasures(ob)["df"]` degrees of freedom.', '',''),
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
