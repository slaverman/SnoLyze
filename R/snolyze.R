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
all <- NULL

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
  unlockBinding("all", env)

  rel <- tryCatch(readRel(sourceRel), error = function(e){stop("Relationship file is not RF2")})
  isa <<- typeRel(rel, FALSE)
  setkey(isa, destinationId, sourceId)

  if(!is.null(sourceTrans))
  {
    transitiveclosure <<- tryCatch(readTrans(sourceTrans), error = function(e){stop("Transitiveclosure file needs to have the following headers; subtypeId, supertypeId. \n  Leave the second parameter empty to let SnoLyze create one.")})
  }
  else
  {
    transitiveclosure <<- createTC(copy(isa))
  }
  rel <<- typeRel(rel, TRUE)
  all <<- data.table(c.integer64(rootconcept,unique(isa$sourceId)))
  setnames(all, old = "V1", new = "sctid")
  setkey(all, sctid)
  parser <<- createParser()
  con <<- unique(rel$sourceId)
  no_att <<- exclusion(any(), con)

  lockBinding("transitiveclosure", env)
  lockBinding("rel", env)
  lockBinding("parser", env)
  lockBinding("isa", env)
  lockBinding("con", env)
  lockBinding("no_att", env)
  lockBinding("all", env)
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
    return(eval(parse(text = getRcode())))

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
