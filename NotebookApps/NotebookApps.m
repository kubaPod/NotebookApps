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

  AppNotebook;

  CreateSessionPreview;
  
  NotebookLayouts;
  
  BookmarkSession;
  BookmarkSessionLoad;


Begin["`Private`"];

$NotebookAppsSourcePath = $InputFileName /. "" :> NotebookDirectory[];


(* ::Section:: *)
(*Apps*)


AppNotebook[spec_]:=Notebook[
    { Cell[ BoxData @ ToBoxes @ AppLoadingPanel[spec] ] }
    
  , CacheGraphics          -> False  
  
  , CellContext            -> Notebook      
  , CellMargins            -> 0 {{1, 1}, {1, 1}}
  , CellFrameMargins       -> 0   
  
  , WindowSize             -> 800 {1, 1/GoldenRatio}
  , WindowTitle            -> spec["name"]
  , WindowFrameElements    -> All
  , WindowElements         -> {"StatusArea", "MagnificationPopUp"}
  
  , StyleDefinitions       -> "Dialog.nb" 
  , ScrollingOptions       -> {"VerticalScrollRange" -> Fit}
  , PrivateNotebookOptions -> {"ExcludeFromShutdown" -> False}
  
  ];


AppLoadingPanel[spec_]:=With[
  { failedLoadSign = Style["\[WarningSign]",Blend[{Red,Orange}],80]  
  , source = Import[#, "Text"]& /@ Lookup[spec, {"sessionSource", "methodsSource"}, Nothing]
  }
, DynamicModule[{ loaded = False, loadFailed = False }
    , Dynamic[
        Which[ 
          Not @ TrueQ @ loaded, ProgressIndicator[Appearance->"Necklace",ImageSize->300]
        , TrueQ @ loadFailed, failedLoadSign
        , True, $CellContext`AppNotebook`AppPanel[]
        ]
      , TrackedSymbols:>{loaded}  
      ]
    , UnsavedVariables :> {loaded}  
    , SynchronousInitialization->False
    , Initialization :> (
        loaded = False
      ; Pause[.1] 
      ; Check[ (*loading procedure*)
          Needs["PacletManager`"]
        ; Module[{s}, s = StringToStream[#];Get[s];Close[s]]& /@ source  
        ; $CellContext`AppNotebook`AppInitialization[]
        , loadFailed = True 
        ]
      ; loaded = True  
      )         
    ]
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
BookmarkSession::usage = "BookmarkSession[context_:$Context] saves context in a .session file in choosen directory.";
BookmarkSession[]:= BookmarkSession @ $Context;
BookmarkSession[notebookContext_String?(StringEndsQ["`"])]:=Module[
  {path, sessionContext = notebookContext <> "AppSession`", dir}
, dir = SystemDialogInput["Directory", $HomeDirectory]
; path = FileNameJoin[{dir, CreateUUID["myFirstApp"]<>".session"}];
; Print @ "saving session"
; Print @ sessionContext
; Block[{$ContextPath = {sessionContext}}, Save[path, Evaluate @ sessionContext]  ]
]



(* ::Subsection::Closed:: *)
(*BookmarkSessionLoad*)


BookmarkSessionLoad::usage = "BookmarkSessionLoad[file] reads file with session data into current context, nothing fancy.";
BookmarkSessionLoad[]:= Module[{file}
, file = SystemDialogInput["FileOpen", "*.session"]
; Print["loading session"];
; Print[$Context]
; Print @ Import[file, "text"]
; If[
    Quiet @ TrueQ @ FileExistsQ @ #
  , Get @ #
  ]& @ file
];


(* ::Section::Closed:: *)
(*NotebookLayouts*)


NotebookLayouts["Basic"]:= basicLayout;

withNotebookMagnification = Style[#, Magnification-> FrontEnd`AbsoluteCurrentValue[EvaluationNotebook[], Magnification]]&;

basicLayout[header_, main_, settings_, OptionsPattern[]]:=With[
  { panelMargin = 8
  , headerH = 50
  , notebookFrameWidths = {2,10}
  , pixelColumnsOfUnkownOrigin = 3
  }
, DynamicModule[{ cellContentSize, settSize={200,All}, mainSize}
, Grid[
    { { Framed @ Pane[withNotebookMagnification @ header, ImageSize->Dynamic[{cellContentSize[[1]],headerH}]]
      , SpanFromLeft
      }
    , { Framed @ Pane[withNotebookMagnification @ main
        , ImageSize->Dynamic[mainSize, (mainSize[[1]]=#[[1]];settSize[[1]]=cellContentSize[[1]]-#[[1]]-2panelMargin-pixelColumnsOfUnkownOrigin)&]
        , AppearanceElements->"ResizeArea"
        ]
      , Framed @ Pane[withNotebookMagnification @ settings, Dynamic[settSize], Scrollbars->True, AppearanceElements->None]
      }
    , { Dynamic[
          cellContentSize = AbsoluteCurrentValue[EvaluationNotebook[],WindowSize]-notebookFrameWidths-2panelMargin
        ; mainSize=cellContentSize-{settSize[[1]],headerH}-2panelMargin-pixelColumnsOfUnkownOrigin
        ; settSize[[2]]=mainSize[[2]]
        ; Spacer[0]
        , TrackedSymbols:>{}]
      }
    }
  , Spacings->{0,0}
  , Alignment->{Left,Top}
  ]
, BaseStyle -> {   
    CacheGraphics->False
  , Magnification -> 1  
  , FrameBoxOptions->{
      FrameMargins->0
    , ImageMargins->panelMargin
    , FrameStyle->Directive[Thickness[Tiny],GrayLevel[.8]]    
    }
  , PaneBoxOptions->{
      FrameMargins->0
    , ImageMargins->0
    , Alignment->{Center,Center}
    , BaseStyle->{LineBreakWithin->False}
    }
  }
]];




(* ::Section::Closed:: *)
(*End*)


End[];
EndPackage[];

