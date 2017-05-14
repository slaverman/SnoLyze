createTC <- function(rel)
{
  i <- 1

  dt <- data.table(subtypeId = rel$sourceId, supertypeId = rel$destinationId, pathlength = 1)
  dt$subtypeId <- as.integer64(dt$subtypeId)
  dt$supertypeId <- as.integer64(dt$supertypeId)
  setnames(rel, "sourceId", "supertypeId")
  setkey(rel, supertypeId)
  setkey(dt, supertypeId)

  while(i > 0)
  {
    inner <- rel[dt, nomatch=0,allow.cartesian=TRUE]
    setnames(inner, old = c("supertypeId", "destinationId"), new = c("sourceId","supertypeId"))
    setkey(inner, subtypeId, supertypeId)
    setkey(dt, subtypeId, supertypeId)
    left <- dt[inner]
    a <- left[is.na(pathlength)][,c("subtypeId", "supertypeId", "i.pathlength")]
    a <- a[, i.pathlength := i.pathlength + 1]
    setnames(a, "i.pathlength", "pathlength")
    a <- unique(a)
    dt <- merge(dt, a, by = c("subtypeId","supertypeId","pathlength"), all = TRUE)
    setkey(dt, supertypeId)
    i <- nrow(a)
  }
  setkey(dt, subtypeId, supertypeId, pathlength)
  return(dt)
}
