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



VerificationTest[
  Block[{Internal`$ContextMarks = False}
      , RawBoxes @ ToBoxes @ NotebookApps`Private`PopulateLoading[
            Hold @ GetInjected["NotebookApps`","ContextRules" -> Automatic]
          , False
        ]/. s_String ? (StringLength[#] > 100 &):>"(**)"
    ]
, RawBoxes@RowBox[{"Hold","[",RowBox[{RowBox[{"Function","[",RowBox[{"expr",",",RowBox[{"Block","[",RowBox[{RowBox[{"{",RowBox[{RowBox[{"$LocalPackages","=",RowBox[{"{","}"}]}],",",RowBox[{"$NotebookContext","=","$Context"}]}],"}"}],",",RowBox[{"InheritedBlock","[",RowBox[{RowBox[{"{","Needs","}"}],",",RowBox[{RowBox[{"Unprotect","[","Needs","]"}],";",RowBox[{RowBox[{"Needs","[",RowBox[{"context_String","/;",RowBox[{"MemberQ","[",RowBox[{"$LocalPackages",",","context"}],"]"}]}],"]"}],":=",RowBox[{"Needs","[",RowBox[{"$NotebookContext","<>","context"}],"]"}]}],";",RowBox[{"Protect","[","Needs","]"}],";","expr"}]}],"]"}]}],"]"}],",","HoldFirst"}],"]"}],"[",RowBox[{RowBox[{"Function","[",RowBox[{RowBox[{"{","source$","}"}],",",RowBox[{"Module","[",RowBox[{RowBox[{"{","stream$","}"}],",",RowBox[{"WithLocalSettings","[",RowBox[{RowBox[{"stream$","=",RowBox[{"StringToStream","[",RowBox[{"Uncompress","[","source$","]"}],"]"}]}],",",RowBox[{RowBox[{"Function","[",RowBox[{"expr",",",RowBox[{"InheritedBlock","[",RowBox[{RowBox[{"{","BeginPackage","}"}],",",RowBox[{RowBox[{"Unprotect","[","BeginPackage","]"}],";",RowBox[{RowBox[{"BeginPackage","[",RowBox[{"context_String","/;",RowBox[{"!",RowBox[{RowBox[{"StringStartsQ","[","$NotebookContext","]"}],"[","context","]"}]}]}],"]"}],":=",RowBox[{"(",RowBox[{RowBox[{"AppendTo","[",RowBox[{"$LocalPackages",",","context"}],"]"}],";",RowBox[{"BeginPackage","[",RowBox[{"$NotebookContext","<>","context"}],"]"}]}],")"}]}],";",RowBox[{"Protect","[","BeginPackage","]"}],";","expr"}]}],"]"}],",","HoldFirst"}],"]"}],"[",RowBox[{"Identity","[",RowBox[{"Get","[","stream$","]"}],"]"}],"]"}],",",RowBox[{"Close","[","stream$","]"}]}],"]"}]}],"]"}]}],"]"}],"[","(**)","]"}],"]"}],"]"}]
, TestID -> "PopulateLoading @ GetInjected[package, ContextRules -> Automatic]"
]


VerificationTest[
  NotebookApps`Private`PopulateLoading[
      Hold @ GetInjected["NotebookApps`","ContextRules" -> Automatic]
    , False
  ]
, NotebookApps`Private`PopulateLoading[ Hold @ GetInjected["NotebookApps`"] , False ]
, TestID -> "ContextRules Automatic by default"
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


(* ::Section:: *)
(*WithLocalizedContexts*)


VerificationTest[
  Internal`InheritedBlock[
    {$ContextPath, $Packages}
  , NotebookApps`Private`WithLocalizedContexts[
        NotebookApps`Private`LocalizeNewContexts[Automatic] @ ToExpression @ "BeginPackage[\"MyPackage`\"];EndPackage[];"
      ; Needs@"MyPackage`"; MemberQ[$ContextPath, $Context <> "MyPackage`"]
    ]
]  
, True
, TestID -> "Internal`InheritedBlock[{$ContextPath,$Packages},NotebookApps`Pr..."
]


