(* ::Package:: *)

(* Mathematica Package *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)

(* :Title: NotebookApps *)
(* :Context: NotebookApps` *)
(* :Author: Kuba *)
(* :Date: 2017-12-11 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: *)
(* :Copyright: (c) 2017 Kuba *)
(* :Keywords: *)
(* :Discussion: *)



(* ::Section:: *)
(*Begin*)


BeginPackage["NotebookApps`"];

  ClearAll["`*", "`*`*"];

  NewNotebookApp;
  AppNotebook;

    (*symbolic wrappers*)
  GetInjected;
  SetInjected;
  AppSession;
  AppNotebook;

  CreateSessionPreview;
  
  NotebookLayouts;
  BasicLayout;
  
  BookmarkSession;
  BookmarkSessionLoad;
  
  EncodeExpression;


  ThemeButton;

  $LocalPackages::usage = "$LocalPackages stores packages that were pushed down to $NotebookContext. " <>
    "NotebookApps`$LocalPackages is available during app initialization.";
  $NotebookContext::usage = "$NotebookContext contains notebook's $Context. NotebooksApps`$NotebookContext is "<>
    "available during app initialization";


Begin["`Private`"];


$BuildMonitor = Print;


(* ::Section:: *)
(*Apps*)


(* ::Subsection::Closed:: *)
(*NewApp*)


NewNotebookApp::usage = "NewNotebookApp[name, dir_.] creates dir/name with files essential for an app.";
NewNotebookApp::dirTaken = "`` is not an empty directory, please delete content and try again.";

NewNotebookApp[name_]:= NewNotebookApp[name, Directory[]];

NewNotebookApp[name_, dir_]:= Module[
  { appDir, tag
  , appSourceFile = name <> "Source.wl"
  , devNb         = name <> ".nb"
  }
, Catch[
    appDir = FileNameJoin[{dir, name}]
  ; If[
        Not @ DirectoryQ @ appDir
      , CreateDirectory @ appDir
      , If[
        FileNames["*",appDir] =!= {}
        , Message[NewNotebookApp::dirTaken, appDir]
        ; Throw[$Failed, tag]
      ]
    ]
  ; Internal`WithLocalSettings[
      SetDirectory @ appDir
      
    , Export[devNb, DevNotebookTemplate @ appSourceFile ]
    ; CreateFile @ appSourceFile
    ; Export[appSourceFile, methodsTemplate[], "Text", PageWidth->\[Infinity]]
    ; NotebookOpen @ AbsoluteFileName @ devNb
    ; NotebookOpen @ AbsoluteFileName @ appSourceFile
    
    , ResetDirectory[]
    ]
  , tag
  ] \[Infinity]
];

DevNotebookTemplate[appSourceFile_String]:= Module[{cells}
, cells = {
"Needs @ \"NotebookApps`\"",

"
Quiet @ NotebookDelete /@ {$debugNbObject, $appNbObject};

$appNotebook = AppNotebook[
    \"BuildRoot\" -> NotebookDirectory[]
  , \"InitializationText\" -> \"Initialization...\"
  , Initialization :> (

        GetInjected[\"``\"]
      ; Symbol[\"AppInitialization\"][]

    )
  , WindowSize -> {700, 500}
];

$appNbObject = NotebookPut @ $appNotebook" // StringTemplate // # @ appSourceFile &,

"$debugNbObject = CreateDocument[{}, CellContext -> CurrentValue[$appNbObject, {TaggingRules, \"Context\"}] ]",

"CDFDeploy[\[IndentingNewLine]  FileNameJoin[{NotebookDirectory[],\"app.cdf\"}]
, $appNotebook\[IndentingNewLine]]",

"SystemOpen @ %"
}

; Notebook[
    Cell[BoxData@#, "Code"] & /@ cells
  , "TrackCellChangeTimes" -> False
  , PrivateNotebookOptions -> {"FileOutlineCache" -> False}
  ]

];

methodsTemplate[]:= "
  (* AppPanel and AppInitialization are special names, don't change them. Rest is up to you.
     Don't bother with BeginPackage and friends unless you know what you are doing.
     NotebookApps makes sure this fill will be read within notebook's local context'
  *)
  (* I suggest the following naming convention:
     $$name        for symbols that represent app state, are meant to be changed etc.
     $name         for 'static' variables that are not going to change e.g. $fontSize
     action$name[] for actions working with $$name variables / app state
     view$name     for view elements that will be affected by interactive manipulations / rewriting
  *)





    (*AppPanel name should not be changed*)
  AppPanel[]:= Pane[
    Column[{
      headerPanel[]
    , mainPanel[]
    }]
  , FrameMargins -> 15
  ];

    (*AppInitialization name should not be changed*)
  AppInitialization[]:= {
    $$y = 1;
  };





headerPanel[]:=Pane[
  Row[{\"this is a header, put here a logo or whatever\", Slider @ Dynamic @ $$y}, Spacer @ 50]
, {Full, Full}
, Alignment->Left
];

mainPanel[]:=Graphics[Line @ {{-1,-1}, Dynamic@{1, $$y}}
,  PlotRange->1
, Frame -> True
, ImageSize -> 300
];

";


(* ::Subsection::Closed:: *)
(*AppNotebook*)


AppNotebook // Options = {

  "Name"                   -> ""
, "BuildRoot"              :> Directory[]
, "InitializationEncoding" -> True
, "InitializationText"     -> "Initialization..."
, Initialization           :> {}
, WindowSize               -> 1000 {1, 1/GoldenRatio}
     
};



AppNotebook[ options:OptionsPattern[{AppNotebook, Notebook}]]:= Internal`WithLocalSettings[
  SetDirectory @ OptionValue["BuildRoot"]
  
, Notebook[
    { Cell[ 
        BoxData @ ToBoxes @ AppLoadingPanel @ FilterRules[{options}, Options[AppNotebook]] 
      ] 
    }    
  , Sequence @@ FilterRules[{options}, Options[Notebook]]  
  
  , CacheGraphics          -> False  
  , Background             -> GrayLevel@.95
  , CellContext            -> Notebook      
  , CellMargins            -> 0 {{1, 1}, {1, 1}}
  , CellFrameMargins       -> 0   
  
 
  , TaggingRules           -> {} (*Otherwise they can inherit too much*)
  , WindowFrameElements    -> All
  , WindowElements         -> {"StatusArea", "MagnificationPopUp"}
  
  , StyleDefinitions       -> "Dialog.nb" 
  , ScrollingOptions       -> {"VerticalScrollRange" -> Fit}
  , PrivateNotebookOptions -> {"ExcludeFromShutdown" -> False}
  
  ]
  
, ResetDirectory[]
];


(* ::Subsection::Closed:: *)
(*AppLoadingPanel*)


AppLoadingPanel // Options = Options @ AppNotebook;

AppLoadingPanel[options:OptionsPattern[]]:=With[
  {
    failedLoadSign = Style["\[WarningSign]", Blend[{Red,Orange}], 80, ShowStringCharacters->False]
  , waitingPane = $defaultWaitingPane @ OptionValue["InitializationText"]
  , loading = (
      PrintTemporary["creating notebook initialization"]
    ; PopulateLoading[
        OptionValue[Automatic, Automatic, Initialization, Hold],
        OptionValue["InitializationEncoding"]
      ]
    )
  }
, DynamicModule[{ mainViewState = "loading", loaded = False, loadFailed = False, theApp }

    , PaneSelector[
        { "loading" -> waitingPane
        , "failed"  -> failedLoadSign
        , "ok"      -> Dynamic[Refresh[theApp, None]]}
      , Dynamic[ mainViewState + 0 ]
      , ImageSize -> Automatic
      ]

    , UnsavedVariables          :> {loaded, theApp, mainViewState}
    , SynchronousInitialization -> False
    , Initialization            :> Catch @ (

        mainViewState = "loading" (*jic*)
      ; Pause[.001]
      
      ; CurrentValue[EvaluationNotebook[], {TaggingRules, "Context"}] = $Context
      
      ; ReleaseHold @ loading

      ; theApp = $CellContext`AppPanel[] /. _$CellContext`AppPanel :> (
          mainViewState = "failed"; Throw @ $Failed
        )

      ; mainViewState = "ok"
      )         
    ] (*TODO: msg handler for initialization*)
];

$defaultWaitingPane = Pane[
  Overlay[
    { ProgressIndicator[Appearance->"Necklace",ImageSize->Scaled/@{.3,.3}]
    , #
    }
  , All
  , Alignment->{Center,Center}  
  ]
, ImageSize -> FrontEnd`AbsoluteCurrentValue[WindowSize]
, Alignment->{Center,Center}
]&;




(* ::Subsection::Closed:: *)
(*PopulateLoading*)


PopulateLoading // ClearAll

PopulateLoading::encErr = "Something went wrong when trying to encode app initialization";

PopulateLoading[loadingProcedure:_Hold, encode_:True]:= Catch @ Module[
  { temp}
, Check[
    temp = loadingProcedure /. {
      source_GetInjected :> With[
        { content      = GIcontent @@ source
        , readFunction = GIreadFunction @@ source
        }
      , readFunction[content] /; True
      ]
    , spec_SetInjected :> With[
        { resources = SIcontent @@ spec
        , loadFunction = SIfunction @@ spec
        }
      , loadFunction[resources] /; True
      ]
    }

  ; temp = temp /. Hold[proc_] :> With[{ foo = WithLocalizedContexts}, Hold[foo @ proc]]

  ; If[Not @ TrueQ @ encode, $BuildMonitor["not encoded"]; Throw @ temp]

  ; With[
      { enc = EncodeExpression @ temp
      }
    , PrintTemporary["Encoded"]
    ; Hold @ Module[{str = StringToStream @ enc, res}
      , res = ReleaseHold @ Get @ str
      ; Close @ str
      ; res
      ]
    ]
  , Message[PopulateLoading::encErr]; Throw @ $Failed
  ]
];


(*what do we want from those two following functions?

 WithLocalizedContext should provide an environment to be able to keep track of localized contexts so that
 Needs can call them instead of their non localized progenitors. It wraps whole initialization.

 LocalizeNewContexts should make BeginPackage[context] perform BeginPackage[notebookContext`context] if
 appropriate ContextRules are used.

 We need to keep track of $NotebookContext because $Context, "`"` and friends are affected by all
 BeginPackage / Package stuff that is going on when loading dependencies.

 We need to be able to call only context that were localized. Not every Needs will call embedded package, it
 is perfectly fine to call Needs @ GeneralUtilities` and it should call system resources. So we need to keep
 track of what we localized.
 *)

WithLocalizedContexts = Function[
    expr
  , Block[
        {$LocalPackages = {}, $NotebookContext = $Context}
      , Internal`InheritedBlock[
            {Needs}
          , Needs // Unprotect
          ; Needs[context_String /; MemberQ[$LocalPackages, context] ] := Needs[ $NotebookContext <> context ]
          ; Needs // Protect
          ; expr
        ]
    ]
  , HoldFirst
];



LocalizeNewContexts[Automatic] = Function[
    expr
  , Internal`InheritedBlock[
        {BeginPackage}

      , BeginPackage // Unprotect
      ; BeginPackage[context_String /; Not @ StringStartsQ[$NotebookContext] @ context ] := (
          AppendTo[$LocalPackages, context]
        ; BeginPackage[$NotebookContext <> context]
        )
      ; BeginPackage // Protect
      ; expr
    ]
  , HoldFirst
];

LocalizeNewContexts[_] = Identity;



(* ::Subsection::Closed:: *)
(*EncodeExpression*)


EncodeExpression::usage = "EncodeExpression[expr] creates returns a string generated by, roughly, ReadString @ Encode @ Save @ expr.";

EncodeExpression[expr_]:= Module[{file, fileEnc, res}
, file = FileNameJoin[{$TemporaryDirectory, CreateUUID[]}]
; fileEnc = file<>"enc"

; Export[file, expr, "Package"]
; Encode[file, fileEnc]
; res = Import[fileEnc, "Text"]

; DeleteFile /@ {file, fileEnc}

; res
]


(* ::Subsection:: *)
(*SetInjected*)


SetInjected::usage = "SetInjected[symbolName, spec] is a symbolic representation which AppNotebook replaces with symbol = importedSpec.";


SIfunction[ symbolName_String, ___]:= Function[
  source
, ToExpression[
    symbolName
  , InputForm
  , Function[symbol, symbol = Uncompress @ source, HoldFirst]
  ]
]


(*Just imports path*)
SIcontent[_String, path_String ? FileExistsQ ]:= Compress @ Import @ path;



(*returns <| a -> <| b -> <|fileC -> ..., fileD -> ...|>, fileE -> ... |> |> *)
SIcontent[_String, spec__ ]:= Module[{temp}

, temp = FileNames[spec]

; temp = Function[
    file
  , $BuildMonitor["importing resource: ", File[file] ]
  ; Fold[
      <|#2 -> #|>&
    , <|FileBaseName[file] -> Import[file]|>
    , Rest @ Reverse @ FileNameSplit @ file
    ]
  ] /@ temp

; Compress @ MergeNested @ temp
]


MergeNested=If[MatchQ[#,{__Association}],Merge[#,#0],Last[#]]&;


(* ::Subsection::Closed:: *)
(*GetInjected*)


GetInjected // Options = {
  "Scope" -> None,
  "ContextRules" -> Automatic
};

GetInjected::usage = "GetInjected[source, opts] is a symbolic wrapper which AppNotebook will replace with injected source";


GIcontent[file_String, ___]:= Module[
  {path = FindFile @ file}
, $BuildMonitor["compressing: ", file, " - ", File[path]]
; Compress @ Import[path, "Text"]  
];



GIreadFunction // Options = Options @ GetInjected;

GIreadFunction[spec_, OptionsPattern[]]:= With[
  {
    baseContextBlock     = BaseContextFunction @ OptionValue @ "Scope"
  , contextHandlers      = LocalizeNewContexts @ OptionValue @ "ContextRules"
  }

, Function[{source}
  , Module[{stream}
    , Internal`WithLocalSettings[
        stream = StringToStream @ Uncompress @ source
      , contextHandlers @ baseContextBlock @ Get @ stream
      , Close @ stream
      ]
    ]
  ]

];

(*just legacy code I guess*)
BaseContextFunction::usage = "BaseContextFunction[spec] returns a function to that creates context envirnment for a source file";

BaseContextFunction::invArgs = "Can't use ``";

BaseContextFunction[args___]:= (Message[BaseContextFunction::invArgs, {args}];$Failed);

BaseContextFunction[None] = Identity;

BaseContextFunction[{start: (Begin | BeginPackage ), context_String}]:= With[
  {  end = start /. {Begin -> End, BeginPackage -> EndPackage}
  }
, Function[
    expr
  , start[context]; expr; end[]
  , HoldAll
  ]
];



(*TODO: All, specific context, context rules*)

RelativeContextFunction::usage =
 "RelativeContextFunction[context, method] return a function " <>
 "that makes BeginPackage[context] behave like BeginPackage[`context]";

RelativeContextFunction::invArgs = "Can't use ``";

RelativeContextFunction[args___]:= (Message[RelativeContextFunction::invArgs, {args}];$Failed);

RelativeContextFunction[spec_, None] = Identity;

RelativeContextFunction[spec:_String ? ( StringEndsQ["`"] ), Automatic] := Function[
  expr
, Internal`InheritedBlock[{BeginPackage}
    , BeginPackage // Unprotect
    ; BeginPackage[spec]:=BeginPackage[ "`" <> spec ]
    ; BeginPackage // Protect
    ; expr
  ]
, HoldAll
]


(* ::Section::Closed:: *)
(*SessionTools*)


(* ::Subsection::Closed:: *)
(*CreateSessionPreview*)


	(*TODO: adapt to work with PlayerPRO*)
CreateSessionPreview::usage = "CreateSessionPreview[context_:$Context] creates a notebook with all context's variables displayed and highlighted dynamically.";
CreateSessionPreview[]:=CreateSessionPreview @ $Context;
CreateSessionPreview[notebookContext_String?(StringEndsQ["`"])]:= Module[{sessionContext}
, Print["creating session preview"]
; Print @ $Context
; sessionContext = notebookContext <> "AppSession`"
; CreateDocument[
    Column[
      Join[
        { Dynamic[{$Context,RandomReal[]}, UpdateInterval->2]
        , Dynamic[{$HomeDirectory, RandomReal[]}, UpdateInterval->2]
        }
      , ToExpression[#, StandardForm, VariableTracker ]& /@ Names[sessionContext <> "*"]
      ]
    ]
    , CellContext ->  notebookContext
    , WindowTitle -> notebookContext
  ]
];


VariableTracker // Attributes = {HoldAll};
VariableTracker[symbol_]:= DynamicModule[
  {col = Red}
, Dynamic[
    Framed[Tooltip[SymbolName[Unevaluated@symbol], symbol],Background->(col = col/. {Red->Blue,Blue->Red})]
  , TrackedSymbols:>{symbol}
  ]
]


(* ::Section::Closed:: *)
(*Bookmarks*)


(* ::Subsection::Closed:: *)
(*BookmarkSession*)


(*TODO: make names more informative in terms of parent app*)

$AppMonitor = HoldComplete;

BookmarkSession::usage = "BookmarkSession[context_:$Context] saves context in a .session file in choosen directory.";


BookmarkSession[]:= BookmarkSession @ $Context;

BookmarkSession[ path_String /; !StringEndsQ[path, "`"] ]:= BookmarkSession[ path, $Context ]

BookmarkSession[context_String /; StringEndsQ[context, "`"]]:=Module[
  {path, sessionContext = context <> "AppSession`", dir}
, dir = SystemDialogInput["Directory", $HomeDirectory]
; path = FileNameJoin[{dir, CreateUUID["myFirstApp"]<>".session"}];
; BookmarkSession[path, context]
]

BookmarkSession[ path_String, context_String /; StringEndsQ[context, "`"] ]:= Module[
  { sessionContext = context <> "AppSession`", dir}
, $AppMonitor @ "saving session"
; $AppMonitor @ sessionContext
; Block[{$ContextPath = {sessionContext}}, Save[path, Evaluate @ sessionContext]  ]
]




(* ::Subsection::Closed:: *)
(*BookmarkSessionLoad*)


BookmarkSessionLoad::usage = "BookmarkSessionLoad[file] reads file with session data into current context, nothing fancy.";

BookmarkSessionLoad // Options = {
   "SystemDialogInit" -> "*"
};

BookmarkSessionLoad[ OptionsPattern[] ]:= Module[{file = ""}
, file = SystemDialogInput["FileOpen", OptionValue["SystemDialogInit"]]
; Switch[ file
  , $Canceled | $Failed | "", $Failed
  , _String, BookmarkSessionLoad[file]
  ]
]

BookmarkSessionLoad[file_String /; FileExistsQ[file]]:=Module[
  {}
, $AppMonitor["loading session"];
; $AppMonitor[$Context]

; Get @ file
];


(* ::Section::Closed:: *)
(*NotebookLayouts*)


NotebookLayouts["Basic"]:= BasicLayout;


(*FrontEnd`AbsoluteCurrentValue was casing lags*)
withNotebookMagnification// ClearAll;
withNotebookMagnification[expr_]:= Style[
  expr
, Magnification-> Dynamic @ AbsoluteCurrentValue[EvaluationNotebook[], Magnification]
];

(*withNotebookMagnification = Identity;*)


(* ::Subsection::Closed:: *)
(*basicLayout*)


BasicLayout // ClearAll 

BasicLayout::usage = "BasicLayout[header, main, settings, options___] creates a following layout: " <> ToString[Grid[{{"header",SpanFromLeft},{"main","settings"}},Frame->All],StandardForm];

BasicLayout // Options = {
  "HeaderHeight" -> Automatic
, "SettingsWidth" -> Automatic  
, "ItemFrameStyle" -> Directive[Thickness[Tiny],GrayLevel[.8]]
, "ItemFrameMargins" -> 10
, "SpacingWidth" -> 10
  
};
BasicLayout[header_, main_, settings_, OptionsPattern[]]:=With[
  { spacerSize = OptionValue["SpacingWidth"]
  , headerH = OptionValue["HeaderHeight"] /. Automatic :> Rasterize[header, "BoundingBox"][[2]] + 2 OptionValue["ItemFrameMargins"]
  , notebookFrameWidths = {4, 25}
  , pixelColumnsOfUnkownOrigin = 3
  }
, DynamicModule[
    { cellContentSize
    , settSize = {
        OptionValue["SettingsWidth"] /. Automatic -> 200
      , All
      }
    , mainSize
     
    }
  , Module[
      { $spacerItem
      , $sizeListener
      , $sizeAdjuster
      , $headerItem
      , $mainItem
      , $settingsItem 
      , $Framed, $Pane, $Grid
      }
    , $spacerItem = Spacer[spacerSize {1,1}]
    
    ; $sizeListener = With[{spacer = $spacerItem}, DynamicModule[{windowSize}
      , DynamicWrapper[
          DynamicWrapper[
            spacer
            
          , cellContentSize = windowSize - notebookFrameWidths - 2 spacerSize
          ; mainSize        = cellContentSize - {settSize[[1]], headerH} -  spacerSize - pixelColumnsOfUnkownOrigin
          ; settSize[[2]]   = mainSize[[2]]
          
          , TrackedSymbols:>{windowSize}
          ]
        , FEPrivate`Set[windowSize, CurrentValue[WindowSize]]
                 
        ]
      , Initialization :> (windowSize = CurrentValue[WindowSize])        
      ]
      ]
      (*The outer DynamicWrapper is needed to fix this bug: https://mathematica.stackexchange.com/q/163091/5478 *)
      
    
        
      
      (*I do this that way to avoid injecting options to every instance or passing those options too deeply *)
    ; $Framed = Framed[##,  FrameMargins -> 0, ImageMargins -> 0, FrameStyle   -> OptionValue["ItemFrameStyle"]]&
    ; $Pane = Pane[##, FrameMargins -> OptionValue["ItemFrameMargins"], ImageMargins -> 0, Alignment->{Center,Center}, BaseStyle->{LineBreakWithin->False}]&  
    ; $Grid = Grid[##, Alignment -> {Left, Top}, Spacings -> {0,0}, ItemSize->{Automatic,0}]&
       
    ; $headerItem = $Framed @ $Pane[withNotebookMagnification @ header, ImageSize->Dynamic[{cellContentSize[[1]],headerH}]] 
    
    ; $mainItem = $Framed @ $Pane[
        withNotebookMagnification @ main
      , ImageSize->Dynamic[
          mainSize
        , ( mainSize[[1]]=#[[1]]          
          ; settSize[[1]]=cellContentSize[[1]]-#[[1]] - spacerSize-pixelColumnsOfUnkownOrigin
          )&
        ]
      , AppearanceElements->"ResizeArea"
      ]
      
    ; $settingsItem = $Framed @ $Pane[
        withNotebookMagnification @ settings
      , Dynamic[settSize]
      , Scrollbars -> True
      , AppearanceElements -> None
      , Alignment -> {Left, Top}
      ]     
      
    ; $Grid[
        { {$sizeListener (*!*)} 
        , {$Grid[{{$spacerItem, $headerItem,  $spacerItem}}]}
        , {$spacerItem} 
        , {$Grid[{{$spacerItem,   $mainItem,    $spacerItem,  $settingsItem, $spacerItem}}]}
        , {$spacerItem}
        }      
      , BaseStyle -> {   
          CacheGraphics->False
        , Magnification -> 1  
        }
      ]
    ]
  ]
];




(* ::Section::Closed:: *)
(*GUI elements*)


ThemeOptions[textColor_:GrayLevel[.9], bgCol_:GrayLevel[.1]] := {
  {GraphicsBoxOptions, Background} -> bgCol,
  {GraphicsBoxOptions, FrameTicksStyle} -> textColor,
  {ButtonBoxOptions, Background} -> bgCol,
  {ButtonBoxOptions, BaseStyle, FontColor} -> textColor,
  {FontColor} -> textColor,
  {Background} -> bgCol
};

ThemeButton[textColor_:GrayLevel[.9], bgCol_:GrayLevel[.1]]:= With[
  { darkStyles  = ThemeOptions[textColor, bgCol]
  , nb         := EvaluationNotebook[]
  , themeCV    := CurrentValue[EvaluationNotebook[],{TaggingRules,"Theme"}]
  }
, Button[
    Graphics[
      { DynamicBox[FEPrivate`If[FEPrivate`SameQ[themeCV,"dark"],GrayLevel[1],GrayLevel[0]]]
      , Disk[{0,0},1,{-Pi/2,Pi/2}], Thick, Circle[]
      }
    , ImageSize->{15,15}
    ]
  , If[themeCV==="dark"
    , (CurrentValue[nb, #] = Inherited) & @@@ darkStyles; themeCV="light"
    , (CurrentValue[nb, #] = #2) & @@@ darkStyles; themeCV="dark"
    ]
, Appearance->None
]
]


(* ::Section::Closed:: *)
(*End*)


End[];
EndPackage[];
