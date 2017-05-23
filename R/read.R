# RF2 only
# Read sourceId, destinationId and typeId as character and not as.integer64 to prevent coercion to numeric
# Cast to integer64 after reading in, this prevents coercion to numeric (tested)
readRel <- function(source)
{
  rel_cols <- list(logical = "active", character = c("sourceId","destinationId","typeId","characteristicTypeId"), numeric = "relationshipGroup")
  rel_drop <- c("effectiveTime","id","moduleId","modifierId")
  rel <- fread(source, colClasses = rel_cols, drop = rel_drop, showProgress = FALSE)
  rel$sourceId <- as.integer64(rel$sourceId)
  rel$typeId <- as.integer64(rel$typeId)
  rel$destinationId <- as.integer64(rel$destinationId)
  rel$characteristicTypeId <- as.integer64(rel$characteristicTypeId)
  setkey(rel, sourceId, typeId, destinationId, relationshipGroup)
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
  setkey(trans, subtypeId, supertypeId, pathlength)
  return(trans)
}
