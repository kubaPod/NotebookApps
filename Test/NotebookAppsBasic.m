(* ::Package:: *)

Needs @ "NotebookApps`";
testExpression = {"\:0105-\[Alpha]-\[ADoubleDot]", {1,2,3}};


(* ::Subsubsection:: *)
(*EncodeExpression*)


VerificationTest[
  EncodeExpression @ testExpression
, "(*!1N!*)mcm\nj<0|O6sT$>!X#kEujcdZ7QD5SFM+`C?WahDJH@]@\\9;Ijf';)@o8kjo.e:7jsJRAE/%,Ak\nv%Y#]YM$nCp !.sVv\"Bie_!;gGT3!!"
, TestID -> "EncodeExpression@{\"\:0105-\\[Alpha]-\[ADoubleDot]\",{1,2,3}}"
]


VerificationTest[
  Module[{str},
str = StringToStream @ EncodeExpression @ testExpression;
{ testExpression === Get @ str, Close@str}
]
, {True, String}
, TestID -> "expr === Get @ StringToStream @ EncodeExpression @ expr"
]


(* ::Subsubsection:: *)
(*PopulateLoading*)


VerificationTest[
  (# === ReleaseHold @ NotebookApps`Private`PopulateLoading[  Hold[#] ]  )& @ testExpression
, True
, TestID -> "expr === ReleaseHold @ PopulateLoading @ Hold @ expr"
]

