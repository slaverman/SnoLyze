query <- function(expressionConstraint)
{
  return(expressionConstraint)
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
    return(subExpressionConstraint)
  }
}
refinedExpressionConstraint <- function(simpleExpressionConstraint, eclRefinement)
{
  return(conjunction(simpleExpressionConstraint, eclRefinement))
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
  return(conjunction(subExpressionConstraint, conjunctionList(conjunction_subExpressionConstraint)))
}
disjunctionExpressionConstraint <- function(subExpressionConstraint, disjunction_subExpressionConstraint)
{
   return(disjunction(subExpressionConstraint, disjunctionList(disjunction_subExpressionConstraint)))
}
exclusionExpressionConstraint <- function(subExpressionConstraint, exclusion_subExpressionConstraint)
{
  return(exclusion(subExpressionConstraint, exclusion_subExpressionConstraint))
}
dottedExpressionConstraint <- function(subExpressionConstraint, constraintOperator = NULL, eclAttributeName)
{
  # TODO 1*(ws dot ws [attributeOperator ws] eclAttributeName)
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
dot <- function()
{
  stop("dot is not supported yet")
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
  return(self("*"))
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
eclAttributeSet <- function(subAttributeSet, conjunctionAttributeSet = NULL, disjunctionAttributeSet = NULL)
{
  if(!is.null(conjunctionAttributeSet))
  {
    return(conjunction(subAttributeSet,conjunctionAttributeSet))
  }
  else if(!is.null(disjunctionAttributeSet))
  {
    return(disjunction(subAttributeSet, disjunctionAttributeSet))
  }
  else
  {
    return(subAttributeSet)
  }
}
conjunctionAttributeSet <- function(subAttributeSet)
{
  return(conjunctionList(subAttributeSet))
}
disjunctionAttributeSet <- function(subAttributeSet)
{
  return(disjunctionList(subAttributeSet))
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
    stop("grouped cardinality is not supported yet")
  }
  return(eclAttributeSet)
}
eclAttribute <- function(group = FALSE, minValue = NULL, maxValue = NULL, reverseFlag = FALSE, constraintOperator = NULL, eclAttributeName, expressionComparisonOperator = TRUE, subExpressionConstraint)
{

  att <- getAtt(group, reverseFlag,constraintOperator, eclAttributeName, expressionComparisonOperator, subExpressionConstraint)

  if(nrow(att) == 0) # can not perform unique function on empty vector, cardinality doesn't matter anymore
  {
    return(emptyVector())
  }
  if(!is.null(minValue))
  {
    return(cardinalityHandler(group, minValue, maxValue, att, reverseFlag))
  }
  else
  {
    if(reverseFlag)
    {
      return(unique(att$destinationId))
    }
    else
    {
      return(unique(att$sourceId))
    }
  }
}
expressionComparisonOperator <- function(operator)
{
  if(operator == "=")
    return(TRUE)
  else
    return(FALSE) # NOT =, <>, !=
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
