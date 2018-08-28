

# lm(x1 ~ sex + ageyr + grade, HolzingerSwineford1939)
#
#
# create_annotation(lm(x1 ~ sex + ageyr + grade, HolzingerSwineford1939))
#
# options(annotated.source="collection")
# m<-annotated(lm(x1 ~ sex + ageyr+ grade + x2, HolzingerSwineford1939))
#options(annotated.source="tryout1.R")


# Это просто повторение того, что вы ввели, с добавлением имен аргументов.
#
# Это нестандантизованые регрессионные коэффициенты. По умолчанию ни р-квдрат, ни стандартные ошибки коэффициетов, ни их значимости не показываются. Для этого сохраните модель в объект и добавьте к функции summary().
#
# Intercept - это константа, то есть значение зависимой переменной в том случае, если все придикторы равны нулю. Часто это нереалистичное значение.

#' Helper to run inline r code (a lá .Rmd)
#'
#'
#'@param rowtxt Annotation to parse.
#'
parse.annotation.row <- function(rowtxt) {
  b<- as.character(exists("ob", where=parent.frame(), inherits=T))
  ob <- get("ob", parent.frame()) #is.null(get0("ob", envir = pos.to.env(-1L)))

  splt.txt <- strsplit(rowtxt, "`")[[1]]
  if(length(splt.txt)>1) {
    txt.blocks1 <- splt.txt[seq(1,length(splt.txt),2)]
    rscrpts    <- splt.txt[seq(2,length(splt.txt),2)]
    rscrpts    <- sub("r ", "", rscrpts)
    txt.blocks2 <- sapply(rscrpts, function(x) {
      u<-try(eval(parse(text=x)), silent = FALSE)
      if(class(u)=="try-error") paste0("[R error:", x, "]", collapse="") else u
    }
    )
    paste0(sapply(1:max(c(length(txt.blocks1), length(txt.blocks2))),
                  function(x) paste0(ifelse(is.na(txt.blocks1[x]), "", txt.blocks1[x]),
                                     ifelse(is.na(txt.blocks2[x]), "", txt.blocks2[x]) )), collapse="")
  } else {
    rowtxt
  }
}



#parse.annotation.row(rowtxt="bnonono `r 5 6` sdfskjg")
#create_annotation()

