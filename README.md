# SnoLyze
SnoLyze is a lightweight and fast SNOMED CT Expression Constraint Language Execution Engine in R to support data analytics over SNOMED CT enabled data. 

SnoLyze consists of three functions:
 - `launch()` initializes SnoLyze
 - `execute()` executes an expression
 - `getTransitiveClosure()` returns the initialized transitive closure file

## Installation
```r
# requires devtools
# install.packages("devtools")
devtools::install_github("slaverman/SnoLyze")
```

## Usage
```r
library(SnoLyze)

# without transitive closure file, takes around 70 seconds to read in the relationship file and create the transitive closure file
launch("PATH_TO_SNOMED_CT_RELATIONSHIP_FILE")

# with transitive closure file, takes around 10 seconds to read in the relationship and transitive closure files
launch("PATH_TO_SNOMED_CT_RELATIONSHIP_FILE", "PATH_TO_TRANSITIVE_CLOSURE_FILE")

# get transitive closure file, to save it with your favourite filewriter (example below uses fwrite of the data.table package)
transitive <- getTransitiveClosure()
fwrite(transitive, "transitive_closure_INT_20170131")

# < 19829001 |Disorder of lung| : 116676008 |Associated morphology|  =  79654002 |Edema|
execute("< 19829001 |Disorder of lung| : 116676008 |Associated morphology|  =  79654002 |Edema|")
# integer64
#  [1] 11468004          19242006          67782005          89687005          95437004          162970000         196151000        
#  [8] 196152007         196153002         233705000         233707008         233708003         233712009         240629003        
# [15] 276637009         405276000         698638005         698640000         700458001         10674871000119105
# run time: 23 ms

# select all cases in 'data' with a type of 'Diabetes mellitus', 
# but not a type of 'Diabetes mellitus during pregnancy, childbirth and the puerperium'
result <- execute("<< 73211009 | Diabetes mellitus (disorder)| MINUS << 199223000 | Diabetes mellitus during pregnancy, childbirth and the puerperium (disorder)|")
data[sctid %in% result]
#    pid   date      sctid           fsn
#     1   2011-01-06 5969009         Diabetes mellitus associated with genetic syndrome (disorder)
#     1   2012-02-07 609572000       Maturity-onset diabetes of the young, type 5 (disorder)
#     3   2013-03-08 426875007       Latent autoimmune diabetes mellitus in adult (disorder)
#     4   2014-04-09 426875007       Latent autoimmune diabetes mellitus in adult (disorder)
#     5   2015-05-10 426875007       Latent autoimmune diabetes mellitus in adult (disorder)
#    ---
#   61154 2011-12-07 359642000       Diabetes mellitus type 2 in nonobese (disorder)
#   61155 2012-11-06 359642000       Diabetes mellitus type 2 in nonobese (disorder)
#   61156 2013-10-05 359642000       Diabetes mellitus type 2 in nonobese (disorder)
#   61157 2014-09-04 359642000       Diabetes mellitus type 2 in nonobese (disorder)
#   61158 2015-08-03 359642000       Diabetes mellitus type 2 in nonobese (disorder)
```
## Supported features
SnoLyze currently supports the brief and long syntax of the Expression Constraint Language version 1.2 in combination with SNOMED CT Release Format 2.

Feature | Example | Supported
---------- | ----------------------- | --
Self | 404684003 \|Clinical finding\| | Yes
Descendant of | <  404684003 \|Clinical finding\| | Yes
Descendant or Self of | <<  404684003 \|Clinical finding\| | Yes
Child of | <!  404684003 \|Clinical finding\| | Yes
Ancestor of | >  40541001 \|Acute pulmonary edema\| | Yes
Ancestor or Self of | >>  40541001 \|Acute pulmonary edema\| | Yes
Parent of | >!  40541001 \|Acute pulmonary edema\| | Yes
Member of | ^ 700043003 \|Example problem list concepts reference set\| | No
Any | * | Yes
Attributes | <  19829001 \|Disorder of lung\| : 116676008 \|Associated morphology\|  =  79654002 \|Edema\| | Yes
Attribute Groups | <  404684003 \|Clinical finding\| : {363698007 \|Finding site\|  = <<  39057004 \|Pulmonary valve structure\| , 116676008 \|Associated morphology\|  = <<  415582006 \|Stenosis\| }, {363698007 \|Finding site\|  = <<  53085002 \|Right ventricular structure\| , 116676008 \|Associated morphology\|  = <<  56246009 \|Hypertrophy\| } | Yes
Nested Attributes | <  404684003 \|Clinical finding\| 47429007 \|Associated with\|  = (< 404684003 \|Clinical finding\| : 116676008 \|Associated morphology\|  = <<  55641003 \|Infarct\| ) | Yes
Attribute Operators | <<  404684003 \|Clinical finding\| : <<  47429007 \|Associated with\|  = <<  267038008 \|Edema\| |	Yes
Concrete Values	| < 373873005 \|Pharmaceutical / biologic product\| : 209999999104 \|Has trade name\| = "PANADOL"| No
Reverse Attributes | <  91723000 \|Anatomical structure\| : R  363698007 \|Finding site\|  = <  125605004 \|Fracture of bone\| | Yes
Any Attribute Name and Value | <  404684003 \|Clinical finding\| : * =  79654002 \|Edema\| | Yes
Simple Conjunction | <  19829001 \|Disorder of lung\|  AND <  301867009 \|Edema of trunk\| | Yes
Simple Disjunction | <  19829001 \|Disorder of lung\|  OR <  301867009 \|Edema of trunk\| | Yes
Attribute Conjunction | <  404684003 \|Clinical finding\| : 363698007 \|Finding site\|  = <<  39057004 \|Pulmonary valve structure\| , 116676008 \|Associated morphology\|  = <<  415582006 \|Stenosis\| | Yes
Attribute Disjunction | <  404684003 \|Clinical finding\| : 116676008 \|Associated morphology\|  = <<  55641003 \|Infarct\|  OR 42752001 \|Due to\|  = <<  22298006 \|Myocardial infarction\| | Yes
Attribute Group Conjunction and Disjunction | <  404684003 \|Clinical finding\| : { 363698007 \|Finding site\|  = <<  39057004 \|Pulmonary valve structure\| , 116676008 \|Associated morphology\|  = <<  415582006 \|Stenosis\| } OR { 363698007 \|Finding site\|  = <<  53085002 \|Right ventricular structure\| , 116676008 \|Associated morphology\|  = <<  56246009 \|Hypertrophy\| } | Yes
Attribute Value Conjunction | <  404684003 \|Clinical finding\| :  116676008 \|Associated morphology\|  = (<<  56208002 \|Ulcer\|  AND <<  50960005 \|Hemorrhage\| ) | Yes
Attribute Value Disjunction | <  404684003 \|Clinical finding\| :  116676008 \|Associated morphology\|  = (<<  56208002 \|Ulcer\|  OR <<  50960005 \|Hemorrhage\| ) | Yes
Exclusion of Simple Expressions	| <<  19829001 \|Disorder of lung\|  MINUS <<  301867009 \|Edema of trunk\| | Yes
Exclusion of Attribute Values | <  404684003 \|Clinical finding\| :  116676008 \|Associated morphology\|  = ((<<  56208002 \|Ulcer\|  AND <<  50960005 \|Hemorrhage\| ) MINUS <<  26036001 \|Obstruction\| ) | Yes
Not Equal to Attribute Value | <  404684003 \|Clinical finding\| : 116676008 \|Associated morphology\|  != <<  26036001 \|Obstruction\| | Yes

