#' @import data.table
#' @import V8
#' @import bit64

rel <- NULL
transitiveclosure <- NULL
isa <- NULL
parser <- NULL
rootconcept <- as.integer64("138875005")
packagename <- "SnoLyze"

# Speed up cardinality with minValue 0
no_att <- NULL # concepts without attributs for 0 cardinality 25% faster
con <- NULL # all unique concepts with attributes

#' @export
launch <- function(sourceRel, sourceTrans = NULL)
{
  if(missing(sourceRel))
  {
    stop("Relationship file is missing")
  }

  env <- loadNamespace(packagename)
  unlockBinding("transitiveclosure", env)
  unlockBinding("rel", env)
  unlockBinding("parser", env)
  unlockBinding("isa", env)
  unlockBinding("no_att", env)
  unlockBinding("con", env)

  rel <- tryCatch(readRel(sourceRel), error = function(e){stop("Relationship file is not RF2")})
  isa <<- typeRel(rel, FALSE)
  setkey(isa, sourceId, destinationId)

  if(!is.null(sourceTrans))
  {
    transitiveclosure <<- tryCatch(readTrans(sourceTrans), error = function(e){stop("Transitiveclosure file needs to have the following headers; subtypeId, supertypeId, pathlength. \n  Leave the second parameter empty to let SnoLyze create one.")})
  }
  else
  {
    transitiveclosure <<- createTC(isa)
  }
  rel <<- typeRel(rel, TRUE)
  parser <<- createParser()
  con <<- unique(rel$sourceId)
  no_att <<- exclusion(any(), con)

  lockBinding("transitiveclosure", env)
  lockBinding("rel", env)
  lockBinding("parser", env)
  lockBinding("isa", env)
  lockBinding("con", env)
  lockBinding("no_att", env)
}

#' @export
execute <- function(query)
{
  if(is.null(transitiveclosure) | is.null(rel))
  {
    stop("SnoLyze is not initialized yet")
  }
  if(validate(query))
  {
    result <- eval(parse(text = getRcode()))
    if(length(result) == 1) # check if concept exists, instead of checking it everytime in self()
    {
      if(result %in% transitiveclosure$subtypeId | result == rootconcept)
      {
        return(result)
      }
      else
      {
        return(emptyVector())
      }
    }
    else
    {
      return(result)
    }
  }
  else
  {
    message("Syntax is not correct")
  }
}

#' @export
getTransitiveClosure <- function()
{
  if(is.null(transitiveclosure))
  {
    stop("SnoLyze is not initialized yet")
  }
  return(transitiveclosure)
}
