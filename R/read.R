# RF2 only
# Read sourceId, destinationId and typeId as character and not as.integer64 to prevent coercion to numeric
# Cast to integer64 after reading in, this prevents coercion to numeric (tested)
readRel <- function(source)
{
  rel_drop <- c("effectiveTime","id","moduleId","modifierId")
  rel <- fread(source, drop = rel_drop, showProgress = FALSE)
  rel$active <- as.logical(rel$active)
  rel$typeId <- as.numeric(rel$typeId)
  setkey(rel, typeId)

  return(rel)
}

# Full; relationshipfile without is-a relationships
# Not full; relationshipfile with is-a relationships only (for transitive closure table)
typeRel <- function(rel, full)
{
  if(full)
  {
    return(rel[active == TRUE & typeId != as.integer64("116680003") & characteristicTypeId != as.integer64("900000000000227009")][,c("sourceId", "typeId", "destinationId", "relationshipGroup")])
  }
  else
  {
    return(rel[active == TRUE & typeId == as.integer64("116680003")][,c("sourceId", "destinationId")])
  }
}

readTrans <- function(source)
{
  # PathLength numeric instead of integer, parentOf(self("19829001")) 75,9 ms as numeric, 97,3 ms as integer
  trans_cols <- list(character = c("subtypeId","supertypeId"))
  trans <- fread(source, colClasses = trans_cols, showProgress = FALSE)
  trans$subtypeId <- as.integer64(trans$subtypeId)
  trans$supertypeId <- as.integer64(trans$supertypeId)
  setkey(trans, supertypeId, subtypeId)
  return(trans)
}
