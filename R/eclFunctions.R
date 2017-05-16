self <- function(sctid) # INPUT MUST BE A STRING OTHERWISE 999480561000087100 WILL BECOME 999480561000087040
{
  if(sctid == "*") # *, ANY
  {
    return(any())
  }
  else
  {
    return(as.integer64(sctid))
  }
}

descendantOrSelfOf <- function(sctid)
{
  if(length(sctid) > 1)
  {
    return(any())
  }
  else
  {
    return(c.integer64(sctid,transitiveclosure[supertypeId == sctid]$subtypeId))
  }
}

descendantOf <- function(sctid)
{
  if(length(sctid) > 1)
  {
    return(anyExceptRoot())
  }
  else
  {
    return(transitiveclosure[supertypeId == sctid]$subtypeId)
  }
}

ancestorOf <- function(sctid)
{
  if(length(sctid) > 1)
  {
    return(nonLeafConcepts())
  }
  else
  {
    return(transitiveclosure[subtypeId == sctid]$supertypeId)
  }
}

ancestorOrSelfOf <- function(sctid)
{
  if(length(sctid) > 1)
  {
    return(any())
  }
  else
  {
    return(c.integer64(sctid,transitiveclosure[subtypeId == sctid]$supertypeId))
  }
}

parentOf <- function(sctid)
{
  if(length(sctid) > 1)
  {
    return(nonLeafConcepts())
  }
  else
  {
    return(transitiveclosure[subtypeId == sctid & pathlength == 1]$supertypeId)
  }
}

childOf <- function(sctid)
{
  if(length(sctid) > 1)
  {
    return(anyExceptRoot())
  }
  else
  {
    return(transitiveclosure[supertypeId == sctid & pathlength == 1]$subtypeId)
  }
}

# wildCard functions,for << *, > *, ... faster than selecting unique values in transitiveclosure
any <- function()
{
  return(descendantOrSelfOf(rootconcept))
}
anyExceptRoot <- function()
{
  return(descendantOf(rootconcept))
}
nonLeafConcepts <- function()
{
  return(unique(transitiveclosure$supertypeId))
}
emptyVector <- function()
{
  return(as.integer64(c()))
}

# Cast to data.table, because the setoperations R base are converting integer64 to numerics, so lost of precision, so lost of valid sctid
conjunction <- function(a, b)
{
  return(fintersect(data.table(a), setnames(data.table(b), "b", "a"))[,a])
}

conjunctionList <- function(a)
{
  return(Reduce(fintersect, lapply(a, data.table))[,V1])
}

disjunctionList <- function(a)
{
  return(Reduce(funion, lapply(a, data.table))[,V1])
}

disjunction <- function(a, b)
{
  return(funion(data.table(a), setnames(data.table(b), "b", "a"))[,a])
}
exclusion <- function(a, b)
{
  return(fsetdiff(data.table(a), setnames(data.table(b), "b", "a"))[,a])
}

getAtt <- function(group, reverseFlag,constraintOperator, eclAttributeName, expressionComparisonOperator, subExpressionConstraint)
{
  if(group)
  {
    att <- rel[typeId %in% subExpressionConstraint(constraintOperator, eclFocusConcept = eclAttributeName) & relationshipGroup != 0]
  }
  else
  {
    att <- rel[typeId %in% subExpressionConstraint(constraintOperator, eclFocusConcept = eclAttributeName)]
  }
  if(reverseFlag)
  {
    if(expressionComparisonOperator)
    {
      return(att[sourceId %in% subExpressionConstraint])
    }
    else
    {
      return(att[!(sourceId %in% subExpressionConstraint)])
    }
  }
  else
  {
    if(expressionComparisonOperator)
    {
      return(att[destinationId %in% subExpressionConstraint])
    }
    else
    {
      return(att[!(destinationId %in% subExpressionConstraint)])
    }
  }

}
cardinalityHandler <- function(group, min, max, att, reverseFlag)
{
  min <- as.numeric(min)# prevent that with "2", => 10 is not missed (min is always an numeric so safe)

  if(reverseFlag)
  {
    if(group)
    {
      spec_att <- att[, occurrences:=.N, by= list(destinationId, relationshipGroup)]
    }
    else
    {
      spec_att <- att[, occurrences:=.N, by= destinationId] # count occurrences of the specified attribute by concept
    }
  }
  else
  {
    if(group)
    {
      spec_att <- att[, occurrences:=.N, by= list(sourceId, relationshipGroup)]
    }
    else
    {
      spec_att <- att[, occurrences:=.N, by= sourceId] # count occurrences of the specified attribute by concept
    }
  }
  if(max == "*")
  {
    if(reverseFlag)
    {
      spec_att <- spec_att[occurrences >= min]$destinationId
    }
    else
    {
      spec_att <- spec_att[occurrences >= min]$sourceId
    }
  }
  else
  {
    max <- as.numeric(max)
    if(min > max)
    {
      return(emptyVector()) # is valid according to the grammar, always an emtpy vector
    }
    else
    {
      if(reverseFlag)
      {
        spec_att <- spec_att[occurrences >= min &occurrences <= max]$destinationId
      }
      else
      {
        spec_att <- spec_att[occurrences >= min &occurrences <= max]$sourceId
      }
    }
  }
  if(min == "0")
  {
    if(reverseFlag)
    {
      con <- unique(rel$destinationId) # all unique concepts with attributes
      ex_spec_att <- exclusion(con, unique(att$destinationId)) # all concepts that have attribute(s), but not the specified attribute
    }
    else
    {
      con <- unique(rel$sourceId) # all unique concepts with attributes
      ex_spec_att <- exclusion(con, unique(att$sourceId)) # all concepts that have attribute(s), but not the specified attribute
    }
    no_att <- exclusion(any(), con) # all concepts without attributes
    no_spec_att <- disjunction(ex_spec_att, no_att) # evertything that has not the specified attribute
    return(disjunction(no_spec_att, spec_att))
  }
  else
  {
    return(unique(spec_att))
  }
}
