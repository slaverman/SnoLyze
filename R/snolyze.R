#' @import data.table
#' @import V8
#' @import bit64

rel <- NULL
transitiveclosure <- NULL
parser <- NULL
rootconcept <- as.integer64("138875005")
packagename <- "SnoLyze"

#' @export
launch <- function(sourceRel, sourceTrans = NULL)
{
  # add check if RF2 file
  env <- loadNamespace(packagename)
  unlockBinding("transitiveclosure", env)
  unlockBinding("rel", env)
  unlockBinding("parser", env)

  rel <- readRel(sourceRel)

  if(!is.null(sourceTrans))
  {
    transitiveclosure <<- readTrans(sourceTrans)
  }
  else
  {
    transitiveclosure <<- createTC(typeRel(rel, FALSE))
  }

  rel <<- typeRel(rel, TRUE)
  parser <<- createParser()

  lockBinding("transitiveclosure", env)
  lockBinding("rel", env)
  lockBinding("parser", env)
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
