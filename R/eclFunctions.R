self <- function(sctid) # INPUT MUST BE A STRING OTHERWISE 999480561000087100 WILL BECOME 999480561000087040
{
    self <- as.integer64(sctid)

    if(self %in% all$sctid)
    {
      return(data.table(sctid = self))
    }
    else
    {
      return(emptyTable())
    }
}

emptyTable <- function()
{
  return(data.table(sctid = integer64()))
}

descendantOrSelfOf <- function(sctid)
{
  if(is.null(nrow(sctid))) # *
  {
    return(all)
  }
  else if(nrow(sctid) == 0)
  {
    return(emptyTable())
  }
  else
  {
    return(data.table(sctid = c.integer64(sctid$sctid, transitiveclosure[supertypeId == sctid$sctid]$subtypeId)))
  }
}

descendantOf <- function(sctid)
{
  if(is.null(nrow(sctid)))
  {
    return(anyExceptRoot)
  }
  else if(nrow(sctid) == 0)
  {
    return(emptyTable())
  }
  else
  {
    return(data.table(sctid = transitiveclosure[supertypeId == sctid$sctid]$subtypeId))
  }
}

ancestorOf <- function(sctid)
{
  if(is.null(nrow(sctid)))
  {
    return(nonLeafConcepts)
  }
  else if(nrow(sctid) == 0)
  {
    return(emptyTable())
  }
  else
  {
    return(data.table(sctid = transitiveclosure[subtypeId == sctid$sctid]$supertypeId))
  }
}

ancestorOrSelfOf <- function(sctid)
{
  if(is.null(nrow(sctid)))
  {
    return(all)
  }
  else if(nrow(sctid) == 0)
  {
    return(emptyTable())
  }
  else
  {
    return(data.table(sctid = c.integer64(sctid$sctid, transitiveclosure[subtypeId == sctid$sctid]$supertypeId)))
  }
}

parentOf <- function(sctid)
{
  if(is.null(nrow(sctid)))
  {
    return(nonLeafConcepts)
  }
  else if(nrow(sctid) == 0)
  {
    return(emptyTable())
  }
  else
  {
    return(data.table(sctid = isa[sourceId == sctid$sctid]$destinationId))
  }
}

childOf <- function(sctid)
{
  if(is.null(nrow(sctid)))
  {
    return(anyExceptRoot)
  }
  else if(nrow(sctid) == 0)
  {
    return(emptyTable())
  }
  else
  {
    return(data.table(sctid = isa[destinationId == sctid$sctid]$sourceId))
  }
}


conjunction <- function(a, b, grouped = FALSE)
{
  if(grouped)
  {
    c <- fintersect(a,b)
  }
  else
  {
    c <- fintersect(a[,"sctid"],b[,"sctid"])
  }
  if(nrow(c) == 0)
  {
    return(data.table(sctid = integer64(), relationshipGroup = integer64()))
  }
  else
  {
    return(a[sctid %in% fintersect(a[,"sctid"],b[,"sctid"])$sctid])
  }
}
disjunction <- function(a, b, group = FALSE)
{
  if(group)
  {
    return(funion(a, b, all = TRUE))
  }
  else
  {
    return(funion(a[,"sctid"], b[,"sctid"], all = TRUE))
  }
}
exclusion <- function(a, b)
{
  return(fsetdiff(a[,"sctid"],b[,"sctid"]))
}

conjunctionList <- function(a, grouped = FALSE)
{
  b <- Reduce(fintersect, a)
  if(nrow(b) == 0)
  {
    return(data.table(sctid = integer64(), relationshipGroup = integer64()))
  }
  else
  {
    return(a[[1]][sctid %in% b$sctid])
  }
}

disjunctionList <- function(a)
{
  return(Reduce(funion, a))
}

getAtt <- function(reverseFlag,constraintOperator, eclAttributeName, expressionComparisonOperator, subExpressionConstraint)
{
  att <- copy(rel)
  eclAttribute <- subExpressionConstraint(constraintOperator, eclFocusConcept = eclAttributeName)
  if(length(eclAttribute) == 0 | length(subExpressionConstraint) == 0)
  {
    return(data.table())
  }
  if(is.data.table(eclAttributeName))
  {
      att <- att[typeId %in% eclAttribute$sctid]
  }
  if(is.data.table(subExpressionConstraint))
  {
    if(reverseFlag)
    {
      if(expressionComparisonOperator)
      {
        return(att[sourceId %in% subExpressionConstraint$sctid])
      }
      else
      {
        return(att[!(sourceId %in% subExpressionConstraint$sctid)])
      }
    }
    else
    {
      if(expressionComparisonOperator)
      {
        return(att[destinationId %in% subExpressionConstraint$sctid])
      }
      else
      {
        return(att[!(destinationId %in% subExpressionConstraint$sctid)])
      }
    }
  }
  return(att)
}
cardinalityHandler <- function(grouped, group, min, max, att, reverse = FALSE)
{
  min <- as.numeric(min)# prevent that with "2", => 10 is not missed (min is always an numeric so safe)
  if(group || grouped)
  {
    spec_att <- att[, occurrences:=.N, by= list(sctid, relationshipGroup)]
  }
  else
  {
    spec_att <- att[, occurrences:=.N, by= sctid] # count occurrences of the specified attribute by concept
  }
  if(max == "*")
  {
    spec_att <- spec_att[occurrences >= min]
  }
  else
  {
    max <- as.numeric(max)
    if(min > max)
    {
      return(emptyTable())
    }
    else
    {
      spec_att <- spec_att[occurrences >= min & occurrences <= max]
    }
  }
  if(min == 0)
  {
    if(reverse)
    {
      con <- data.table(sctid = unique(rel$destinationId))
    }
    ex_spec_att <- exclusion(copy(con)[, relationshipGroup := 0], att) # all concepts that have attribute(s), but not the specified attribute
    no_spec_att <- disjunction(ex_spec_att, copy(no_att)[, relationshipGroup := 0]) # evertything that has not the specified attribute
    return(disjunction(no_spec_att, spec_att))
  }
  else
  {
    return(spec_att)
  }
}
