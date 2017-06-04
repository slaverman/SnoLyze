self <- function(sctid) # INPUT MUST BE A STRING OTHERWISE 999480561000087100 WILL BECOME 999480561000087040
{
    return(as.integer64(sctid))
}

descendantOrSelfOf <- function(sctid)
{
  if(as.character(sctid) == "*")
  {
    return(any())
  }
  else
  {
    descendants <- transitiveclosure[supertypeId == sctid]$subtypeId
    if(length(descendants) == 0) # check if sctid is a concept
    {
      return(emptyVector())
    }
    else
    {
      return(c.integer64(sctid,descendants))
    }
  }
}

descendantOf <- function(sctid)
{
  if(as.character(sctid) == "*")
  {
    return(anyExceptRoot())
  }
  else
  {
    descendants <- transitiveclosure[supertypeId == sctid]$subtypeId
    if(length(descendants) == 0)
    {
      return(emptyVector())
    }
    else
    {
      return(descendants)
    }
  }
}

ancestorOf <- function(sctid)
{
  if(as.character(sctid) == "*")
  {
    return(nonLeafConcepts())
  }
  else
  {
    ancestors <- transitiveclosure[subtypeId == sctid]$supertypeId
    if(length(ancestors) == 0)
    {
      return(emptyVector())
    }
    else
    {
      return(ancestors)
    }
  }
}

ancestorOrSelfOf <- function(sctid)
{
  if(as.character(sctid) == "*")
  {
    return(any())
  }
  else
  {
    ancestors <- transitiveclosure[subtypeId == sctid]$supertypeId
    if(length(ancestors) == 0)
    {
      return(emptyVector())
    }
    else
    {
      return(c.integer64(sctid, ancestors))
    }
  }
}

parentOf <- function(sctid)
{
  if(as.character(sctid) == "*")
  {
    return(nonLeafConcepts())
  }
  else
  {
    parents <- isa[sourceId == sctid]$destinationId
    if(length(parents) == 0)
    {
      return(emptyVector())
    }
    else
    {
      return(parents)
    }
  }
}

childOf <- function(sctid)
{
  if(as.character(sctid) == "*")
  {
    return(anyExceptRoot())
  }
  else
  {
    children <- isa[destinationId == sctid]$sourceId
    if(length(children) == 0)
    {
      return(emptyVector())
    }
    else
    {
      return(children)
    }
  }
}

# wildCard functions,for << *, > *, ... faster than selecting unique values in transitiveclosure
any <- function()
{
  return(c.integer64(rootconcept,unique(isa$sourceId)))
}
anyExceptRoot <- function()
{
  return(unique(isa$sourceId))
}
nonLeafConcepts <- function()
{
  return(unique(isa$destinationId))
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
  att <- rel
  eclAttribute <- subExpressionConstraint(constraintOperator, eclFocusConcept = eclAttributeName)
  if(length(eclAttribute) == 0 | length(subExpressionConstraint) == 0)
  {
    return(data.table())
  }
  if(as.character(eclAttributeName[1]) != "*")
  {
    if(group)
    {
      att <- att[typeId %in% eclAttribute & relationshipGroup != 0]
    }
    else
    {
      att <- att[typeId %in% eclAttribute]
    }
  }
  if(as.character(subExpressionConstraint[1]) != "*")
  {
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
  return(att)
}
cardinalityHandler <- function(group, min, max, att, reverseFlag, grouped = FALSE)
{
  min <- as.numeric(min)# prevent that with "2", => 10 is not missed (min is always an numeric so safe)
  if(grouped)
  {
    att <- data.table(att)
    setnames(att,"att","sourceId")
    spec_att <- att[, occurrences:=.N, by= sourceId]
  }
  else
  {
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
  if(min == 0)
  {
    if(reverseFlag)
    {
      con <- unique(rel$destinationId) # all unique concepts with attributes
      ex_spec_att <- exclusion(con, att$destinationId) # all concepts that have attribute(s), but not the specified attribute
    }
    else
    {
      ex_spec_att <- exclusion(con, att$sourceId) # all concepts that have attribute(s), but not the specified attribute
    }
    #no_att <- exclusion(any(), con) # all concepts without attributes
    no_spec_att <- disjunction(ex_spec_att, no_att) # evertything that has not the specified attribute
    return(disjunction(no_spec_att, spec_att))
  }
  else
  {
    return(spec_att)
  }
}
