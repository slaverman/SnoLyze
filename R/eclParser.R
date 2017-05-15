getRcode <- function()
{
  parser$eval("var rcode = parser.ast.toRcode();")
  return(parser$get("rcode"))
}

validate <- function(exp)
{
  parser$assign("exp", exp)
  parser$eval("var inputCharacterCodes = utils.stringToChars(exp);")
  parser$eval("var result = parser.parse(grammar, startRule, inputCharacterCodes);")
  return(parser$get("result")$success)
}

createParser <- function()
{
 parser <- v8()
  grammar <- system.file("js", "grammar.js", package="SnoLyze")
  apg <- system.file("js", "apg-lib.js", package="SnoLyze")
  parser$source(grammar) #extended with new rootconcept
  parser$source(apg)

  parser$eval("var grammar = new generatedGrammar();")
  parser$eval("var parser = new apglib.parser();")
  parser$eval("parser.ast = new apglib.ast();")
  parser$eval("var utils = apglib.utils;")
  parser$eval("var startRule = \"query\";")

  parser$eval("parser.ast.callbacks[\"query\"] = true;
              parser.ast.callbacks[\"expressionConstraint\"] = true;
              parser.ast.callbacks[\"refinedExpressionConstraint\"] = true;
              parser.ast.callbacks[\"compoundExpressionConstraint\"] = true;
              parser.ast.callbacks[\"conjunctionExpressionConstraint\"] = true;
              parser.ast.callbacks[\"disjunctionExpressionConstraint\"] = true;
              parser.ast.callbacks[\"exclusionExpressionConstraint\"] = true;
              parser.ast.callbacks[\"dottedExpressionConstraint\"] = true;
              parser.ast.callbacks[\"subExpressionConstraint\"] = true;
              parser.ast.callbacks[\"simpleExpressionConstraint\"] = true;
              parser.ast.callbacks[\"eclFocusConcept\"] = true;
              parser.ast.callbacks[\"dot\"] = true;
              parser.ast.callbacks[\"memberOf\"] = true;
              parser.ast.callbacks[\"conceptReference\"] = true;
              parser.ast.callbacks[\"conceptId\"] = true;
              parser.ast.callbacks[\"term\"] = true;
              parser.ast.callbacks[\"wildCard\"] = true;
              parser.ast.callbacks[\"constraintOperator\"] = true;
              parser.ast.callbacks[\"descendantOf\"] = true;
              parser.ast.callbacks[\"descendantOrSelfOf\"] = true;
              parser.ast.callbacks[\"childOf\"] = true;
              parser.ast.callbacks[\"ancestorOf\"] = true;
              parser.ast.callbacks[\"ancestorOrSelfOf\"] = true;
              parser.ast.callbacks[\"parentOf\"] = true;
              parser.ast.callbacks[\"conjunction\"] = true;
              parser.ast.callbacks[\"disjunction\"] = true;
              parser.ast.callbacks[\"exclusion\"] = true;
              parser.ast.callbacks[\"eclRefinement\"] = true;
              parser.ast.callbacks[\"conjunctionRefinementSet\"] = true;
              parser.ast.callbacks[\"disjunctionRefinementSet\"] = true;
              parser.ast.callbacks[\"subRefinement\"] = true;
              parser.ast.callbacks[\"eclAttributeSet\"] = true;
              parser.ast.callbacks[\"conjunctionAttributeSet\"] = true;
              parser.ast.callbacks[\"disjunctionAttributeSet\"] = true;
              parser.ast.callbacks[\"subAttributeSet\"] = true;
              parser.ast.callbacks[\"eclAttributeGroup\"] = true;
              parser.ast.callbacks[\"eclAttribute\"] = true;
              parser.ast.callbacks[\"cardinality\"] = true;
              parser.ast.callbacks[\"minValue\"] = true;
              parser.ast.callbacks[\"to\"] = true;
              parser.ast.callbacks[\"maxValue\"] = true;
              parser.ast.callbacks[\"many\"] = true;
              parser.ast.callbacks[\"reverseFlag\"] = true;
              parser.ast.callbacks[\"attributeOperator\"] = true;
              parser.ast.callbacks[\"eclAttributeName\"] = true;
              parser.ast.callbacks[\"expressionComparisonOperator\"] = true;")
  return(parser)
}
