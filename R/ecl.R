query <- function(expressionConstraint)
{
  if(is.null(nrow(expressionConstraint)))
  {
    return(all$sctid)
  }
  else if(nrow(expressionConstraint) == 0)
  {
    return(c.integer64(c()))
  }
  else
  {
    return(unique(expressionConstraint$sctid))
  }
}
expressionConstraint <- function(refinedExpressionConstraint = NULL, compoundExpressionConstraint = NULL, dottedExpressionConstraint = NULL, subExpressionConstraint = NULL)
{
  if(!is.null(refinedExpressionConstraint))
  {
    return(refinedExpressionConstraint)
  }
  else if(!is.null(compoundExpressionConstraint))
  {
    return(compoundExpressionConstraint)
  }
  else if(!is.null(dottedExpressionConstraint))
  {
    return(dottedExpressionConstraint)
  }
  else
  {
    if(as.character(subExpressionConstraint[1]) == "*")
    {
      return(all)
    }
    else
    {
      return(subExpressionConstraint)
    }
  }
}
refinedExpressionConstraint <- function(simpleExpressionConstraint, eclRefinement)
{
  if(as.character(simpleExpressionConstraint[1]) == "*")
  {
    return(eclRefinement)
  }
  else
  {
    return(conjunction(simpleExpressionConstraint, eclRefinement))
  }
}
compoundExpressionConstraint <- function(conjunctionExpressionConstraint = NULL ,disjunctionExpressionConstraint = NULL,exclusionExpressionConstraint = NULL)
{
  if(!is.null(conjunctionExpressionConstraint))
  {
    return(conjunctionExpressionConstraint)
  }
  else if(!is.null(disjunctionExpressionConstraint))
  {
    return(disjunctionExpressionConstraint)
  }
  else
  {
    return(exclusionExpressionConstraint)
  }
}
conjunctionExpressionConstraint <- function(subExpressionConstraint ,conjunction_subExpressionConstraint)
{
  if(as.character(subExpressionConstraint[1]) == "*")
  {
    if("character" %in% lapply(conjunction_subExpressionConstraint, class))
    {
      return(any())
    }
    else
    {
      return(conjunctionList(conjunction_subExpressionConstraint))
    }
  }
  else
  {
    if("character" %in% lapply(conjunction_subExpressionConstraint, class))
    {
      return(subExpressionConstraint)
    }
    else
    {
      return(conjunction(subExpressionConstraint, conjunctionList(conjunction_subExpressionConstraint)))
    }
  }
}
disjunctionExpressionConstraint <- function(subExpressionConstraint, disjunction_subExpressionConstraint)
{
  if(as.character(subExpressionConstraint[1]) == "*" | "character" %in% lapply(disjunction_subExpressionConstraint, class))
  {
    return(any())
  }
  else
  {
    return(disjunction(subExpressionConstraint, conjunctionList(disjunction_subExpressionConstraint)))
  }
}
exclusionExpressionConstraint <- function(subExpressionConstraint, exclusion_subExpressionConstraint)
{
  if(as.character(exclusion_subExpressionConstraint[1]) == "*")
  {
    return(emptyVector())
  }
  if(as.character(subExpressionConstraint[1]) == "*")
  {
    subExpressionConstraint <- any()
  }
  else
  {
    return(exclusion(subExpressionConstraint, exclusion_subExpressionConstraint))
  }
}
dottedExpressionConstraint <- function(subExpressionConstraint, constraintOperator = NULL, eclAttributeName)
{
  return(eclAttribute(group = FALSE, minValue = NULL, maxValue = NULL, reverseFlag = TRUE, constraintOperator = constraintOperator, eclAttributeName, expressionComparisonOperator = TRUE, subExpressionConstraint))
}
subExpressionConstraint <- function(constraintOperator = NULL, memberOf = NULL, eclFocusConcept = NULL, expressionConstraint = NULL)
{
  if(!is.null(eclFocusConcept))
  {
    if(!is.null(constraintOperator))
    {
      return(concept(constraintOperator, eclFocusConcept))
    }
    else
    {
      return(eclFocusConcept)
    }
  }
  else
  {
    return(expressionConstraint)
  }
}
simpleExpressionConstraint <- function(constraintOperator = NULL, memberOf = NULL, eclFocusConcept)
{
  if(is.null(constraintOperator))
  {
    return(eclFocusConcept)
  }
  else
  {
    return(concept(constraintOperator, eclFocusConcept))
  }
}
eclFocusConcept <- function(conceptReference = NULL, wildCard = NULL)
{
  if(!is.null(conceptReference))
  {
    return(conceptReference)
  }
  else
  {
    return(wildCard)
  }
}
eclAttributeName <- function(conceptReference = NULL, wildCard = NULL)
{
  if(!is.null(conceptReference))
  {
    return(conceptReference)
  }
  else
  {
    return(wildCard)
  }
}
memberOf <- function()
{
  stop("memberOf is not supported yet")
}
conceptReference <- function(conceptId)
{
  return(self(conceptId))
}
wildCard <- function(any)
{
  return(any)
}
eclRefinement <- function(subRefinement, conjunctionRefinementSet = NULL, disjunctionRefinementSet = NULL)
{
  if(!is.null(conjunctionRefinementSet))
  {
    return(conjunction(subRefinement, conjunctionRefinementSet))
  }
  else if(!is.null(disjunctionRefinementSet))
  {
    return(disjunction(subRefinement, disjunctionRefinementSet))
  }
  else
  {
    return(subRefinement)
  }
}
conjunctionRefinementSet <- function(subRefinement)
{
  return(conjunctionList(subRefinement))
}
disjunctionRefinementSet <- function(subRefinement)
{
  return(disjunctionList(subRefinement))
}
subRefinement <- function(eclAttributeSet = NULL, eclAttributeGroup = NULL, eclRefinement = NULL)
{
  if(!is.null(eclAttributeSet))
  {
    return(eclAttributeSet)
  }
  if(!is.null(eclAttributeGroup))
  {
    return(eclAttributeGroup)
  }
  else
  {
    return(eclRefinement)
  }
}
eclAttributeSet <- function(group = FALSE, subAttributeSet, conjunctionAttributeSet = NULL, disjunctionAttributeSet = NULL)
{
  if(!is.null(conjunctionAttributeSet))
  {
    if(!group) # conjunction only on sctid
    {
      return(conjunction(subAttributeSet,conjunctionList(lapply(conjunctionAttributeSet, "[", , "sctid"))))
    }
    else # conjunction on sctid and relationshipGroup
    {
      conjunctionAttributeSet <- conjunctionList(conjunctionAttributeSet)
      return(conjunction(subAttributeSet,conjunctionAttributeSet, TRUE))
    }
  }
  else if(!is.null(disjunctionAttributeSet))
  {
    if(!group)
    {
      return(disjunction(subAttributeSet,disjunctionList(lapply(disjunctionAttributeSet, "[", , "sctid"))))
    }
    else
    {
      disjunctionAttributeSet <- disjunctionList(disjunctionAttributeSet)
      return(disjunction(subAttributeSet,disjunctionAttributeSet))
    }
  }
  else
  {
    return(subAttributeSet)
  }
}
conjunctionAttributeSet <- function(subAttributeSet)
{
  return(subAttributeSet)
}
disjunctionAttributeSet <- function(subAttributeSet)
{
  return(subAttributeSet)
}
subAttributeSet <- function(eclAttribute = NULL, eclAttributeSet = NULL)
{
  if(!is.null(eclAttribute))
  {
    return(eclAttribute)
  }
  else
  {
    return(eclAttributeSet)
  }
}
eclAttributeGroup <- function(minValue = NULL, maxValue = NULL, eclAttributeSet)
{
  if(!is.null(minValue))
  {
    set <- cardinalityHandler(FALSE, TRUE, minValue, maxValue, eclAttributeSet)
    if(nrow(set) == 0)
    {
      return(data.table(sctid = integer64(), relationshipGroup = integer64()))
    }
    else
    {
      return(set[relationshipGroup != 0])
    }
  }
  else
  {
    if(nrow(eclAttributeSet) == 0)
    {
      return(data.table(sctid = integer64(), relationshipGroup = integer64()))
    }
    else
    {
      return(eclAttributeSet[relationshipGroup != 0])
    }
  }
}
eclAttribute <- function(grouped = FALSE,minValue = NULL, maxValue = NULL, reverseFlag = FALSE, constraintOperator = NULL, eclAttributeName, expressionComparisonOperator = TRUE, subExpressionConstraint)
{

  att <- getAtt(reverseFlag,constraintOperator, eclAttributeName, expressionComparisonOperator, subExpressionConstraint)

  if(nrow(att) == 0)
  {
    return(data.table(sctid = integer64(), relationshipGroup = numeric()))
  }
  else
  {
    if(reverseFlag)
    {
      setnames(att, "destinationId", "sctid")
      if(!is.null(minValue))
      {
        return(cardinalityHandler(grouped, FALSE, minValue, maxValue, att[,c("sctid", "relationshipGroup")], TRUE))
      }
      else
      {
        return(att[,c("sctid", "relationshipGroup")])
      }
    }
    else
    {
      setnames(att, "sourceId", "sctid")
      if(!is.null(minValue))
      {
        return(cardinalityHandler(grouped, FALSE, minValue, maxValue, att[,c("sctid", "relationshipGroup")]))
      }
      else
      {
        return(att[,c("sctid", "relationshipGroup")])
      }
    }
  }
}
expressionComparisonOperator <- function(operator)
{
  if(operator == "=")
  {
    return(TRUE)
  }
  else
  {
    return(FALSE) # NOT =, <>, !=
  }
}
concept <- function(constraintOperator = NULL, eclFocusConcept)
{
  if(constraintOperator == "descendantOrSelfOf")
  {
    return(descendantOrSelfOf(eclFocusConcept))
  }
  else if(constraintOperator == "descendantOf")
  {
    return(descendantOf(eclFocusConcept))
  }
  else if(constraintOperator == "childOf")
  {
    return(childOf(eclFocusConcept))
  }
  else if(constraintOperator == "ancestorOf")
  {
    return(ancestorOf(eclFocusConcept))
  }
  else if(constraintOperator == "ancestorOrSelfOf")
  {
    return(ancestorOrSelfOf(eclFocusConcept))
  }
  else
  {
    return(parentOf(eclFocusConcept))
  }
}
