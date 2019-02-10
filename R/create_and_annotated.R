#Sys.setenv(TZ='GMT')
#' @title   For instructors: create annotation
#' @description Create conditional annotations, and save them for further use.
#' @param ... An examplary call that produces a print output to be annotated.
#'
#' @details Run create_annotation() and follow inline instructions, or add an unquoted call as an argument.
#'
#' @examples  require(datasets)
#'  if(interactive())
#'    create_annotation(lm('dist ~ speed', cars))
#'
#' @export
create_annotation <- function(...) {

  if(! interactive()) stop("This function is meant to be used in an interactive mode.")
  if(is.na(as.character(sys.call())[2])) {
    call. <- readline("Enter the call: ")
    cat("\n")
  } else {
    call. <- as.character(sys.call())[2]
  }


  output <- capture.output(eval(parse(text=call.)))
  for(line in 1:length(output))  cat("[", line, "] ", output[line], "\n", sep="")
  #output <- output[output!=""]

  void<-readline("Above is the output you are going to annotate.")

  conditional <- menu(c("Yes", "No"), title="Would you like to make annotations conditional?
You will be prompted to add conditions for every line of current output.")
  if(conditional==1) {
    void<-readline("Every condition will be saved as an R script. You can refer to the output object of the initial call as 'ob'.")

  }


  void<-readline("Now I am going to return it line by line so you can add your annotations.
If you do not want to comment the line, just press 'Enter'.")

  conditions <- list(cond1=rep("", length(output)),
                     cond2=rep("", length(output)),
                     cond3=rep("", length(output))
  )
  annotation <- list(cond1=rep("", length(output)),
                     cond2=rep("", length(output)),
                     cond3=rep("", length(output))
  )
  for(line in 1:length(output)) {

    cat("[", line, "] ", output[line], "\n", sep="")

    if(!output[line]=="")   {



    if(conditional==1) {
      cond <- menu(c("Yes", "No"), title="Is it conditional?")
      if(cond==1) {

        conditions[["cond1"]][line]      <-readline("What is condition 1? ")
        annotation[["cond1"]][line]      <-readline("Annotate (if condition 1 is TRUE): ")


        conditions[["cond2"]][line]      <-readline("What is condition 2? ")
        annotation[["cond2"]][line]      <-readline("Annotate (if condition 2 is TRUE): ")


        cond3 <- menu(c("Yes", "No"), title="Need to add another (last) condition?")
        if(cond3==1) {
          conditions[["cond3"]][line]      <-readline("What is condition 3? ")
          annotation[["cond3"]][line]      <-readline("Annotate (if condition 3 is TRUE): ")
        }

      } else {
        annotation[["cond1"]][line] <- readline("Annotate: ")
      }
    } else {

      annotation[["cond1"]][line] <- readline("Annotate: ")

    }

  }}

  void <- readline("It's over. Press Enter to have a look at the annotated output. ")
cat(cli::rule())
  for(i in 1:length(output))  {
    cat(output[i], "\n")

    if(conditional==1) {

      cat(crayon::red(sapply(1:length(conditions), function(x) {

        if(!(conditions[[x]][i]=="" && annotation[[x]][i]=="")) {
          paste0(crayon::underline("CONDITION:"), " ",  conditions[[x]][i], "\n",
                 crayon::underline("ANNOTATION:"), " ", annotation[[x]][i], "\n\n")
        } else {
          ""
        }
      })))

    } else {

      cat(crayon::red(annotation[[1]][i]))

    }
  }
  # print(conditions)


  choices <- c("Save as R-script for future use with 'annotate' function.",
               #"Save an object called 'annotation' to the environment."#,
               "Save as html as the static example."#,
               #"Save as markdown of the static example."
  )

  selected.choice <- select.list(choices,
                                 title = cli::rule("How do I save the annotations?"),
                                 preselect = choices[1], multiple=T)

  if(any(selected.choice %in% choices[1])) {

    methods.for <- readline("To which functions this annotation is applicable? separate by spaces e.g., lm cfa:  ")
    if(methods.for=="") methods.for <- readline("You HAVE TO specify function names these annotations are applicable to (e.g., lm cfa)? ")

  }

  if(any(selected.choice %in% choices[1])) {


    filename <- readline("Give this file a name with .R extension: ")
    cat( paste0("#This is a script generated with an R package 'annotated'. It is supposed to be used as a library of annotations within the function 'annotated()'. Created on: ", Sys.time(), "\n\n",
      "list(\n",
                " annotation = list(\n  cond1 = ",
                paste0("c(\n    ", paste0("\"", annotation[[1]], "\"", collapse=",\n    "),"),\n"),
                "  cond2 = ",
                paste0("c(\n    ", paste0("\"", annotation[[2]], "\"", collapse=",\n    "),  "),\n"),
                "  cond3 = ",
                paste0("c(\n    ", paste0("\"", annotation[[3]], "\"", collapse=",\n    "),  ")\n ),\n"),


                " conditions = list(\n  cond1 = ",
                paste0("c(\n    ", paste0("\"", conditions[[1]], "\"", collapse=",\n    "),  "),\n"),
                "  cond2 = ",
                paste0("c(\n    ", paste0("\"", conditions[[2]], "\"", collapse=",\n    "),  "),\n"),
                "  cond3 = ",
                paste0("c(\n    ", paste0("\"", conditions[[3]], "\"", collapse=",\n    "),  ")\n ),\n"),

                "methods.for = ",           paste0("\"", strsplit(methods.for, " "), "\"", collapse=","),
                "\n)", collapse="\n"),
         file=filename)
    void<-readline(paste0("Entries are saved to ", filename, ". You can manually edit them later."))
  }

  if (any(selected.choice %in% choices[2])) {
    # Create md and html

    html.output <-
    paste0(sapply(1:length(output), function(i)  {
      paste0(output[i], "<br>", "<span style='color:red;'>",

      if(conditional==1) {

        paste0(sapply(1:length(conditions), function(x) {

          if(!(conditions[[x]][i]=="" && annotation[[x]][i]=="")) {
            paste0( ifelse(conditions[[x]][i]=="", "",
                           paste0("CONDITION", x, ":", " ", conditions[[x]][i], "<br>")),
                    "ANNOTATION:", " ", annotation[[x]][i], "<br>")
          } else {
            ""
          }
        }), collapse="")


      } else {

        annotation[[1]][i]

      }, "</span>")
    }), collapse="")
    html.output <- paste0("<pre>", html.output, "</pre>")

    }



  if (any(selected.choice %in% choices[2])) {

    filename <- readline("Give this file a name with .html extension: ")
    cat(html.output, file=filename)

    #assign("anno", list(annotation, conditions, methods.for), .GlobalEnv)

  }

  # if (any(selected.choice %in% choices[3])) {
  #
  #   filename <- readline("Give this file a name with .md extension: ")
  #   cat(md.output, file=filename)
  #
  #   #assign("anno", list(annotation, conditions, methods.for), .GlobalEnv)
  #
  # }

  cat("all done.")
}




# Annotated funct #####
#' @title  For learners: get annotations
#' @description Get annotated output in console.
#'
#' @param ... A call to a function to get annotations.
#'
#'@examples
#'if(interactive()) {
#'  options(annotated.source = "vignettes/lm.annot.R") # set the source file or directory
#'  require(datasets)
#'  lin.mod1 <- annotated(lm('dist ~ speed', cars))
#' }
#'
#' @return
#' The function invisibly returns an evaluated call to a function. For example, if `annotated(lm(a ~ b))` was called, it will return fitted `lm` object, as if annotated weren't there. Annotations are printed in the console at all times, even when the value is being assigned to an object.
#'
#'@export
annotated <- function(...) {


  is.url  <- function(x) regexpr("(http:\\/\\/)|(https:\\/\\/)", x) == 1



  # Read annotation files
if(is.null(getOption("annotated.source"))) {
  stop(paste("The annotation library is not set. Set it using options(annotated.source='[file or directory]')."), call.=F)

} else {
  source.file.dir <- getOption("annotated.source")
}

  if(dir.exists(source.file.dir) ) {

    annotations <- lapply(list.files(source.file.dir, pattern = "\\.R$"),
                          function(x) source(paste0(source.file.dir, "/", x))[["value"]])

  } else if (file.exists(source.file.dir)) {

    annotations <- list(one=source(source.file.dir)[["value"]])

  } else if(is.url(source.file.dir))  {

    downloaded.file <- tempfile()
    download.file(source.file.dir, downloaded.file)

    annotations <- list(one=source(downloaded.file)[["value"]])

  } else {
    print(getwd())
    stop("Can't find annotation file/directory. Change options('annotated.source')")
  }

  # Check if the function annotation is available in the annotation files.
  call = as.character(sys.call())[2]
  function.used <- strtrim( call, regexec("\\(", call)[[1]]-1 )
  applicable.annotations.index <- as.logical(lapply(annotations,
                                                    function(z) any (z[["methods.for"]] %in% function.used)  ))


  if(!any(applicable.annotations.index)) {
    stop(paste("The annotation is not available for function", function.used), call.=F)

  } else if (sum(applicable.annotations.index)>1) {



    message(paste0("There is more than one annotation available for the function ", function.used,
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


  # Print the annotated output line by line

  for(i in 1:length(output)) {
    cat(output[i])

    # If there are no conditions
    if(annotation[["conditions"]][["cond1"]][i]=="") {

      if(annotation[["annotation"]][["cond1"]][i]!="") {
        cat(crayon::red(paste0("\n",
                       parse_annotation_row(
                         annotation[["annotation"]][["cond1"]][i]),
                       "\n")
                ))

      } else {
        cat("\n")
      }

      # If conditions aren't empty
    } else {

      eval(parse(text=paste("if(", annotation[["conditions"]][["cond1"]][i], ") cat(crayon::red(\"\n",
                            parse_annotation_row(
                            annotation[["annotation"]][["cond1"]][i]), "\n\"))"
                            )))
      eval(parse(text=paste("if(", annotation[["conditions"]][["cond2"]][i], ") cat(crayon::red(\"\n",
                            parse_annotation_row(
                              annotation[["annotation"]][["cond2"]][i]), "\n\"))"
                            )))

      if(annotation[["conditions"]][["cond3"]][i]!="") {

        eval(parse(text=paste("if(", annotation[["conditions"]][["cond3"]][i], ") cat(crayon::red(\"\n",
                              parse_annotation_row(
                                annotation[["annotation"]][["cond3"]][i]), "\n\"))"
                              )))

      }
    }

  }
  invisible(ob)
}
