(* ::Package:: *)

Needs @ "NotebookApps`";
testExpression = {"\:0105-\[Alpha]-\[ADoubleDot]", {1,2,3}};


(* ::Section:: *)
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


(* ::Section:: *)
(*PopulateLoading*)


VerificationTest[
  (# === ReleaseHold @ NotebookApps`Private`PopulateLoading[  Hold[#] ]  )& @ testExpression
, True
, TestID -> "expr === ReleaseHold @ PopulateLoading @ Hold @ expr"
]



(* ::Section:: *)
(*BaseContextFunction*)


VerificationTest[
  NotebookApps`Private`BaseContextFunction[None]
, Identity
, TestID -> "BaseContextFunction[None]"
]


Remove @ "*`notebookAppsTestSymbol";
VerificationTest[
  NotebookApps`Private`BaseContextFunction[{Begin,"`Sub`"}] @ ToExpression["notebookAppsTestSymbol"]
; Context @ Evaluate @ First @ Names["*`notebookAppsTestSymbol"]
, "Global`Sub`"
, TestID -> "NotebookApps`Private`BaseContextFunction[{Begin,\"`Sub`\"}]@ToExpr..."
]


(* ::Section:: *)
(*RelativeContextFunction*)


VerificationTest[
  NotebookApps`Private`RelativeContextFunction["asd",None]
, Identity
, TestID -> "RelativeContextFunction[\"asd\",None]"
]


Quiet@Remove[ "*`notebookAppsTestSymbol", "*`*`notebookAppsTestSymbol"];

Block[{$ContextPath}
, NotebookApps`Private`RelativeContextFunction["MyPackage`", Automatic] @ ToExpression["
BeginPackage[\"MyPackage`\"];
notebookAppsTestSymbol;
EndPackage[];
"]
];

VerificationTest[
  Context @ Evaluate @ First @ Names["*`*`notebookAppsTestSymbol"]
, "Global`MyPackage`"
, TestID -> "RelativeContextFunction[\"MyPackage`\", Automatic]"
]
