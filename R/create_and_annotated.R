#Sys.setenv(TZ='GMT')
#' For teachers: create annotation
#'
#'@param ... An examplary call that produces a print output to be annotated.
#'
#'@details run create_annotation() and follow the interactive instructions.
#'
#' @export
create_annotation <- function(...) {


  call. <- readline("Enter the call: ")
  cat("\n")
  output <- capture.output(eval(parse(text=call.)))
  print(output, quote=F)
  #output <- output[output!=""]

  void<-readline("Above is the output you are going to annotate.")

  conditional <- menu(c("Yes", "No"), title="Would you like to make annotations conditional?
                      You will be prompted to add conditions for every line of current output.")
  if(conditional==1) {
    void<-readline("Every condition will be saved as an R script. You can refer to the output object of the initial call as 'ob'.")
    conditions <- list(cond1=rep("", length(output)),
                       cond2=rep("", length(output)),
                       cond3=rep("", length(output))
    )
  }


  void<-readline("Now I am going to return it line by line so you can add your annotations.
                 If you do not want to comment the line, just press 'Enter'.")

  annotation <- list(cond1=rep("", length(output)),
                     cond2=rep("", length(output)),
                     cond3=rep("", length(output))
  )
  for(line in 1:length(output)) {
    cat("[", line, "] ", output[line], "\n", sep="")

    if(conditional==1) {
      cond <- menu(c("Yes", "No"), title="Is it conditional?")
      if(cond==1) {

        conditions[["cond1"]][line]      <-readline("What is condition 1? ")
        annotation[["cond1"]][line]      <-readline("Annotate (condition 1): ")


        conditions[["cond2"]][line]      <-readline("What is condition 2? ")
        annotation[["cond2"]][line]      <-readline("Annotate (condition 2): ")


        cond3 <- menu(c("Yes", "No"), title="Need to add another (last) condition?")
        if(cond3==1) {
          conditions[["cond3"]][line]      <-readline("What is the condition? ")
          annotation[["cond3"]][line]      <-readline("Annotate (condition 3): ")
        }

      } else {
        annotation[["cond1"]][line] <- readline("Annotate: ")
      }
    } else {

      annotation[["cond1"]][line] <- readline("Annotate: ")

    }

  }

  void <- readline("It's over. Press Enter and have a look at the annotated. ")

  for(i in 1:length(output))  {
    cat(output[i], "\n")

    if(conditional==1) {

      message(sapply(1:length(conditions), function(x) {

        if(!(conditions[[x]][i]=="" && annotation[[x]][i]=="")) {
          paste0("CONDITION: ",  conditions[[x]][i], "\n",
                 "ANNOTATION: ", annotation[[x]][i], "\n")
        } else {
          ""
        }
      }))

    } else {

      message(annotation[[1]][i])

    }
  }
  # print(conditions)


  choices <- c("Save as R-script for future use with 'annotate' function.",
               "Save an object called 'annotation' to the environment."#,
               #"Save as html as a static example.",
               #"Return markdown code of the static example."
  )

  selected.choice <- select.list(choices, title="How do I save the annotations?",
                                 preselect=choices[2], multiple=T)

  if(any(selected.choice %in% choices[1:2])) {

    methods.for <- readline("To which functions this annotation is applicable? separate by spaces e.g., lavaan::cfa cfa:  ")
    if(methods.for=="") methods.for <- readline("You HAVE TO specify function names these annotations are applicable to (e.g., lavaan::cfa, cfa)? ")

  }

  if(any(selected.choice %in% choices[1])) {


    filename <- readline("Give this file a name with .R extension: ")
    cat( paste0("list(",
                "annotation=list(cond1=", paste0("c(", paste0("'", annotation[[1]], "'", collapse=","),  "),"),
                "cond2=", paste0("c(", paste0("'", annotation[[2]], "'", collapse=","),  "),"),
                "cond3=", paste0("c(", paste0("'", annotation[[3]], "'", collapse=","),  ")),"),


                "conditions=list(cond1=", paste0("c(", paste0("'", conditions[[1]], "'", collapse=","),  "),"),
                "cond2=", paste0("c(", paste0("'", conditions[[2]], "'", collapse=","),  "),"),
                "cond3=", paste0("c(", paste0("'", conditions[[3]], "'", collapse=","),  ")),"),

                "methods.for=",           paste0(strsplit(methods.for, " "), collapse=","),
                ")", collapse="\n"),
         file=filename)
    void<-readline(paste0("Entries are saved to ", filename, ". You can manually edit them later."))
  }

  if (any(selected.choice %in% choices[2])) {


    assign("anno", list(annotation, conditions, methods.for), .GlobalEnv)

  }

  cat("all done.")
}




# Annotated funct #####
#' For students: get annotations
#'
#' @param ... The call to function to get annotations.
#'
#'
#'@export
annotated <- function(...) {

  # Read annotation files

  source.file.dir <- getOption("annotated.source")


  if(dir.exists(source.file.dir) ) {


    annotations <- lapply(list.files(source.file.dir), function(x) source(paste0(source.file.dir, "/", x))[["value"]])

  } else if (file.exists(source.file.dir)) {

    annotations <- list(one=source(source.file.dir)[["value"]])

  } else {
    print(getwd())
    stop("Cant find annotation file/directory. Change option('annotated.source')")
  }

  # Check if the function annotation is available in the annotation files.
  call = as.character(sys.call())[2]
  function.used <- strtrim( call, regexec("\\(", call)[[1]]-1 )
  applicable.annotations.index <- as.logical(lapply(annotations,
                                                    function(z) any (z[["methods.for"]] %in% function.used)  ))


  if(!any(applicable.annotations.index)) {
    stop(paste("The annotation is not available for function", function.used), call.=F)

  } else if (applicable.annotations.index>1) {



    warning(paste0("There is more than one annotation available for the function ", function.used,
                   ". In files", paste(list.files(source.file.dir)[applicable.annotations.index], collapse=","),
                   "Using the first one."))

    annotation <- annotations[applicable.annotations.index][1]

  } else {

    annotation <- annotations[applicable.annotations.index][[1]]
    #assign("annotations", annotations, .GlobalEnv)
  }

  # Handle errors and warnings of the call, and evaluate

  options(warn=1)
  wrn<- capture.output({err<-try({ob <-eval(parse(text=call))}, silent=T)}, type="message")

  if(length(wrn)>0) stop(wrn, call.=F)
  if(class(err)=="try-error") {
    stop(attr(err, "condition")[["message"]], call.=F)

  } else {
    rm(err, wrn)
  }


  output = capture.output(print(ob))

  #print(annotation)
  # Print the annotated output line by line

  for(i in 1:length(output)) {
    cat(output[i])

    # If there are no conditions
    if(annotation[["conditions"]][["cond1"]][i]=="") {

      if(annotation[["annotation"]][["cond1"]][i]!="") {
        message(paste0("\n",annotation[["annotation"]][["cond1"]][i]))
      } else {
        cat("\n")
      }

      # If conditions aren't empty
    } else {

      eval(parse(text=paste("if(", annotation[["conditions"]][["cond1"]][i], ") message('\n",
                            annotation[["annotation"]][["cond1"]][i], "')")))
      eval(parse(text=paste("if(", annotation[["conditions"]][["cond2"]][i], ") message('\n",
                            annotation[["annotation"]][["cond2"]][i], "')")))

      if(annotation[["conditions"]][["cond3"]][i]!="") {

        eval(parse(text=paste("if(", annotation[["conditions"]][["cond3"]][i], ") message(\n'",
                              annotation[["annotation"]][["cond3"]][i], "')")))

      }
    }

  }
  invisible(ob)
}
