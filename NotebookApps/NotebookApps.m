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

    (*symbolic wrappers*)
  GetInjected;
  AppSession;
  AppNotebook;

  CreateSessionPreview;
  
  NotebookLayouts;
  
  BookmarkSession;
  BookmarkSessionLoad;


Begin["`Private`"];


(* ::Section:: *)
(*Apps*)


(* ::Subsection:: *)
(*NewApp*)


(* ::Subsection:: *)
(*AppNotebook*)


AppNotebook // Options = {
  "name" -> "",
  "loading" :> Automatic
};

AppNotebook[options:OptionsPattern[{AppNotebook, Notebook}]]:=Notebook[
    { Cell[ BoxData @ ToBoxes @ AppLoadingPanel[options] ] }
    
  , Sequence @@ FilterRules[{options}, Options[Notebook]]  
  
  , CacheGraphics          -> False  
  , Background             -> GrayLevel@.95
  , CellContext            -> Notebook      
  , CellMargins            -> 0 {{1, 1}, {1, 1}}
  , CellFrameMargins       -> 0   
  
  , WindowSize             -> All(*800 {1, 1/GoldenRatio}*)
  , WindowTitle            -> OptionValue["name"]
  , WindowFrameElements    -> All
  , WindowElements         -> {"StatusArea", "MagnificationPopUp"}
  
  , StyleDefinitions       -> "Dialog.nb" 
  , ScrollingOptions       -> {"VerticalScrollRange" -> Fit}
  , PrivateNotebookOptions -> {"ExcludeFromShutdown" -> False}
  
  ];


(* ::Subsection::Closed:: *)
(*AppLoadingPanel*)


AppLoadingPanel // Options = Options @ AppNotebook;

AppLoadingPanel[options:OptionsPattern[]]:=With[
  { failedLoadSign = Style["\[WarningSign]",Blend[{Red,Orange}],80]  
  , loading = PopulateLoading @ OptionValue[Automatic,Automatic, "loading", Hold]
  }
, DynamicModule[{ loaded = False, loadFailed = False }
    , Dynamic[
        Which[ 
          Not @ TrueQ @ loaded, Pane[ProgressIndicator[Appearance->"Necklace",ImageSize->300], ImageSize->800{1,1/GoldenRatio},Alignment->{Center,Center}]
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
      ; Check[ ReleaseHold @ loading, loadFailed = True ]
      ; loaded = True  
      ; SetOptions[EvaluationNotebook[], WindowSize -> 800 {1, 1/GoldenRatio}]
      )         
    ]
];


(* ::Subsection:: *)
(*PopulateLoading*)


PopulateLoading[loadingProcedure_Hold]:= ReplaceAll[
  loadingProcedure
, { 
    GetInjected["NotebookApps`"] :> With[
      { content = Compress @ Import[ FindFile @ "NotebookApps`", "Text"]}
    , Module[{stream}
      , stream=StringToStream @ Uncompress @ content
      ; Get @ stream
      ; Close @ stream
      ] /; True
    ]
    
  , AppSession[path_] :> With[
      { content = Compress @ Import[ path, "Text"]}
    , Module[{stream}
      , stream=StringToStream @ Uncompress @ content
      ; BeginPackage["`AppSession`"]; Get @ stream; EndPackage[]
      ; Close @ stream
      ] /; True
    ]  
    
  , AppNotebook[path_] :> With[
      { content = Compress @ Import[ path, "Text"]}
    , Module[{stream}
      , stream=StringToStream @ Uncompress @ content
      ; Begin["`AppNotebook`"]; Get @ stream; End[]
      ; Close @ stream
      ] /; True
    ]  
  }

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


(* ::Section:: *)
(*NotebookLayouts*)


NotebookLayouts["Basic"]:= basicLayout;

withNotebookMagnification = Style[#, Magnification-> FrontEnd`AbsoluteCurrentValue[EvaluationNotebook[], Magnification]]&;

basicLayout[header_, main_, settings_, OptionsPattern[]]:=With[
  { spacerSize = 10
  , headerH = 70
  , notebookFrameWidths = {4, 25}
  , pixelColumnsOfUnkownOrigin = 3
  }
, DynamicModule[{ cellContentSize, settSize={200,All}, mainSize}, Module[{$spacerItem, $sizeListener,$headerItem,$mainItem,$settingsItem }
  , $spacerItem = Spacer[spacerSize {1,1}]
  ; $sizeListener = With[{spacer = $spacerItem}
    , Dynamic[
        cellContentSize = AbsoluteCurrentValue[EvaluationNotebook[],WindowSize] - notebookFrameWidths - 2 spacerSize
      ; mainSize = cellContentSize - {settSize[[1]], headerH} -  spacerSize - pixelColumnsOfUnkownOrigin
      ; settSize[[2]]=mainSize[[2]]
      ; spacer
      , TrackedSymbols:>{}
      ]
    ]
  ; $headerItem = Framed @ Pane[withNotebookMagnification @ header, ImageSize->Dynamic[{cellContentSize[[1]],headerH}]] 
  
  ; $mainItem = Framed @ Pane[
      withNotebookMagnification @ main
    , ImageSize->Dynamic[mainSize, (mainSize[[1]]=#[[1]]; settSize[[1]]=cellContentSize[[1]]-#[[1]] - spacerSize-pixelColumnsOfUnkownOrigin)&]
    , AppearanceElements->"ResizeArea"
    ]
  ; $settingsItem = Framed @ Pane[withNotebookMagnification @ settings, Dynamic[settSize], Scrollbars->True, AppearanceElements->None]     
    
; Grid[
    { {$sizeListener} 
    , {Grid[{{$spacerItem, $headerItem,  $spacerItem}}]}
    , {$sizeListener} 
    , {Grid[{{$spacerItem,   $mainItem,    $spacerItem,  $settingsItem, $spacerItem}}]}
    , {$spacerItem}
    }  
  ]]
, BaseStyle -> {   
    CacheGraphics->False
  , Magnification -> 1  
  
  , GridBoxOptions -> {
      GridBoxAlignment->{"Columns"->{{Left}},"Rows"->{{Top}}}
    , GridBoxSpacings->{"Columns"->{{0}},"Rows"->{{0}}}  
    }
    
  , FrameBoxOptions->{
      FrameMargins->0
    , ImageMargins->0
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

