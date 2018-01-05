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

  
  NewNotebookApp;
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


NewNotebookApp::usage = "NewNotebookApp[name, dir_.] creates dir/name with files essential for an app.";
NewNotebookApp::dirTaken = "`` is not an empty directory, please delete content and try again.";

NewNotebookApp[name_]:= NewNotebookApp[name, Directory[]];

NewNotebookApp[name_, dir_]:= Module[{appDir, tag,devNb}
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
      
    , devNb = StringTemplate["``.nb"][name]
    ; Export[devNb, DevNotebookTemplate[]]
    ; CreateFile["session.m"]
    ; CreateFile["methods.m"]
    ; Put[methodsTemplate[], "methods.m", PageWidth -> 200]
    ; NotebookOpen @ AbsoluteFileName @ devNb
    ; NotebookOpen @ AbsoluteFileName @ "methods.m"
    
    
    , ResetDirectory[]
    ]
  , tag
  ]
];

DevNotebookTemplate[]:=Notebook[
  Function[
    expr
  , Cell[BoxData@MakeBoxes[expr], "Input"]
  , {HoldAll, Listable}
  ][
    { Needs @ "NotebookApps`"
    , SetDirectory@NotebookDirectory[]
    , NotebookPut @ AppNotebook[WindowSize -> {700, 500}];     
    }
  ]
];

methodsTemplate[]:= OutputForm@"
  (* This is a special file for NotebookApps, its content will be localized within your notebook. *)

  (* AppPanel and AppInitialization are special names, don't change them. Rest is up to you. *)

  (* You don't need to use NotebookLayouts but it is an effort free way to get nice layout. *)
  (* Alternatively just put a Grid there or whatever.*)


  AppPanel[]:= NotebookLayouts[\"Basic\"][
    Pane[\"this is a header, put here a logo or whatever\", {Full, Full}, Alignment\[Rule]Left, FrameMargins\[Rule]15]
  , Graphics[Line @ {{-1,-1}, Dynamic@{1,y}},  PlotRange\[Rule]1, Frame \[Rule] True, AspectRatio\[Rule]Full, ImageSize \[Rule] Full]
  , Slider @ Dynamic @ y
  , \"SettingsW\" -> 300
];

AppInitialization[]:= {};


";


(* ::Subsection::Closed:: *)
(*AppNotebook*)


AppNotebook // Options = {
  "Name" -> "",
  "Loading" :> (
          GetInjected@"NotebookApps`"
        ; AppSession["session.m"]
        ; AppNotebook["methods.m"]
        ; $CellContext`AppNotebook`AppInitialization[]
      ),
  WindowSize -> 1000 {1, 1/GoldenRatio}    
     
};

AppNotebook[options:OptionsPattern[{AppNotebook, Notebook}]]:=Notebook[
    { Cell[ BoxData @ ToBoxes @ AppLoadingPanel@ FilterRules[{options}, Options[AppNotebook]] ] }
    
  , Sequence @@ FilterRules[{options}, Options[Notebook]]  
  
  , CacheGraphics          -> False  
  , Background             -> GrayLevel@.95
  , CellContext            -> Notebook      
  , CellMargins            -> 0 {{1, 1}, {1, 1}}
  , CellFrameMargins       -> 0   
  
 
  
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
  { failedLoadSign = Style["\[WarningSign]",Blend[{Red,Orange}],80, ShowStringCharacters->False]  
  , loading = (
      Print["creating notebook initialization"]
    ; PopulateLoading @ OptionValue[Automatic, Automatic, "Loading", Hold]
    )
  }
, DynamicModule[{ loaded = False, loadFailed = False }
    , Dynamic[
        Which[ 
          Not @ TrueQ @ loaded, Pane[ProgressIndicator[Appearance->"Necklace",ImageSize->300], ImageSize->800{1,1/GoldenRatio},Alignment->{Center,Center}]
        , TrueQ @ loadFailed, failedLoadSign
        , True, $CellContext`AppNotebook`AppPanel[] /. _$CellContext`AppNotebook`AppPanel -> failedLoadSign
        ]
      , TrackedSymbols:>{loaded}  
      ]
    , UnsavedVariables :> {loaded}  
    , SynchronousInitialization->False
    , Initialization :> (
        loaded = False
      ; Pause[.001] 
      ; Check[ ReleaseHold @ loading, loadFailed = True ]
      ; loaded = True  
    
      )         
    ]
];


(* ::Subsection::Closed:: *)
(*PopulateLoading*)


PopulateLoading[loadingProcedure_Hold]:= ReplaceAll[
  loadingProcedure
, { 
    GetInjected["NotebookApps`"] :> With[
      { content = (
          Print["compressing NotebookApps` from ", File @ #]
        ; Compress @ Import[ #, "Text"]
        )& @ FindFile @ "NotebookApps`"
      }
    , Module[{stream}
      , stream=StringToStream @ Uncompress @ content
      ; Get @ stream
      ; Close @ stream
      ] /; True
    ]
    
  , AppSession[path_] :> With[
      { content = (Print["compressing ", File @ path ];Compress @ Import[ path, "Text"])}
    , Module[{stream}
      , stream=StringToStream @ Uncompress @ content
      ; BeginPackage["`AppSession`"]; Get @ stream; EndPackage[]
      ; Close @ stream
      ] /; True
    ]  
    
  , AppNotebook[path_] :> With[
      { content = (Print["compressing ", File @ path ]; Compress @ Import[ path, "Text"])}
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


(* ::Section::Closed:: *)
(*NotebookLayouts*)


NotebookLayouts["Basic"]:= basicLayout;

withNotebookMagnification = Style[#, Magnification-> FrontEnd`AbsoluteCurrentValue[EvaluationNotebook[], Magnification]]&;


(* ::Subsection:: *)
(*basicLayout*)


(*        header            *)
(* ---------------------- *)
(* main area | settingsArea *)


basicLayout // ClearAll 
basicLayout // Options = {
  "HeaderH" -> Automatic
, "SettingsW" -> Automatic  
, "ItemFrameStyle" -> Directive[Thickness[Tiny],GrayLevel[.8]]
, "ItemFrameMargins" -> 0
, "SpacingW" -> 10
  
};
basicLayout[header_, main_, settings_, OptionsPattern[]]:=With[
  { spacerSize = OptionValue["SpacingW"]
  , headerH = OptionValue["HeaderH"] /. Automatic :> Rasterize[header, "BoundingBox"][[2]]
  , notebookFrameWidths = {4, 25}
  , pixelColumnsOfUnkownOrigin = 3
  }
, DynamicModule[
    { cellContentSize
    , settSize = {
        OptionValue["SettingsW"] /. Automatic -> 200
      , All
      }
    , mainSize
    , windowSize
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
    
    ; $sizeListener = With[{spacer = $spacerItem}
      , DynamicWrapper[
          spacer
        , If[ 
            windowSize =!= AbsoluteCurrentValue[EvaluationNotebook[],WindowSize]            
          , windowSize = AbsoluteCurrentValue[EvaluationNotebook[],WindowSize]
          ]
        , TrackedSymbols:>{}
        ]
        
      ]
      
    ; $sizeAdjuster =  With[{spacer = $spacerItem}
      , Dynamic[          
          cellContentSize = windowSize - notebookFrameWidths - 2 spacerSize
        ; mainSize        = cellContentSize - {settSize[[1]], headerH} -  spacerSize - pixelColumnsOfUnkownOrigin
        ; settSize[[2]]   = mainSize[[2]]
        ; spacer          
        , TrackedSymbols:>{windowSize}
        ]
        
      ]
      (*I do this that way to avoid injecting options to every instance or passing those options to deeply *)
    ; $Framed = Framed[##,  FrameMargins -> 0, ImageMargins -> 0, FrameStyle   -> OptionValue["ItemFrameStyle"]]&
    ; $Pane = Pane[##, FrameMargins -> OptionValue["ItemFrameMargins"], ImageMargins -> 0, Alignment->{Center,Center}, BaseStyle->{LineBreakWithin->False}]&  
    ; $Grid = Grid[##, Alignment -> {Left, Top}, Spacings -> {0,0}]&
       
    ; $headerItem = $Framed @ $Pane[withNotebookMagnification @ header, ImageSize->Dynamic[{cellContentSize[[1]],headerH}]] 
    
    ; $mainItem = $Framed @ $Pane[
        withNotebookMagnification @ main
      , ImageSize->Dynamic[mainSize, (mainSize[[1]]=#[[1]]; settSize[[1]]=cellContentSize[[1]]-#[[1]] - spacerSize-pixelColumnsOfUnkownOrigin)&]
      , AppearanceElements->"ResizeArea"
      ]
    ; $settingsItem = $Framed @ $Pane[withNotebookMagnification @ settings, Dynamic[settSize], Scrollbars->True, AppearanceElements->None]     
      
    ; $Grid[
        { {$sizeListener (*!*)} 
        , {$Grid[{{$spacerItem, $headerItem,  $spacerItem}}]}
        , {$sizeAdjuster (*!*)} 
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
(*End*)


End[];
EndPackage[];

