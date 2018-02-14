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
  BasicLayout;
  
  BookmarkSession;
  BookmarkSessionLoad;


Begin["`Private`"];


(* ::Section:: *)
(*Apps*)


(* ::Subsection::Closed:: *)
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
    ; Export["methods.m", methodsTemplate[], "Text", PageWidth->\[Infinity]]
    ; NotebookOpen @ AbsoluteFileName @ devNb
    ; NotebookOpen @ AbsoluteFileName @ "methods.m"
    
    
    , ResetDirectory[]
    ]
  , tag
  ]
];

DevNotebookTemplate[]:=Notebook[
  Cell[BoxData@#, "Input"] & /@ { 
  "Needs @ \"NotebookApps`\""
, "$appNotebook = AppNotebook[
    \"BuildRoot\" \[Rule] NotebookDirectory[]
  , \"InitializationText\" -> \"Initialization...\"
  , WindowSize -> {700, 500}
];

NotebookPut @ $appNotebook"

, "CDFDeploy[\[IndentingNewLine]  FileNameJoin[{NotebookDirectory[],\"app.cdf\"}]
, $appNotebook\[IndentingNewLine]]"
, "SystemOpen @ %"     
    }  
];

methodsTemplate[]:= "
  (* This is a special file for NotebookApps, its content will be localized within your notebook. *)

  (* AppPanel and AppInitialization are special names, don't change them. Rest is up to you. *)

  (* You don't need to use BasicLayout but it is an effort free way to get nice layout. *)
  (* Alternatively just put a Grid there or whatever.*)



    (*AppPanel name should not be changed*)
  AppPanel[]:= BasicLayout[
    headerPanel[]
  , mainPanel[]
  , settingsPanel[]
  , \"SettingsWidth\" -> 300
  ];

    (*AppInitialization name should not be changed*)
  AppInitialization[]:= {
    y = 1;
  };





headerPanel[]:=Pane[\"this is a header, put here a logo or whatever\"
, {Full, Full}
, Alignment\[Rule]Left
];

mainPanel[]:=Graphics[Line @ {{-1,-1}, Dynamic@{1,y}}
,  PlotRange\[Rule]1
, Frame \[Rule] True
, AspectRatio\[Rule]Full
, ImageSize \[Rule] Full
];

settingsPanel[]:=Slider @ Dynamic @ y;


";


(* ::Subsection:: *)
(*AppNotebook*)


AppNotebook // ClearAll

AppNotebook // Options = {

  "Name" -> ""
, "BuildRoot" :> Directory[]

, Initialization :> (
    GetInjected[ "NotebookApps`" ]
  ; GetInjected[ "methods.m", "Scope" -> {Begin, "`AppNotebook2`"}]
  ; $CellContext`AppInitialization[]
  )
  
, WindowSize -> 1000 {1, 1/GoldenRatio}   

, "InitializationText" -> "Initialization..."
     
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
  
 
  
  , WindowFrameElements    -> All
  , WindowElements         -> {"StatusArea", "MagnificationPopUp"}
  
  , StyleDefinitions       -> "Dialog.nb" 
  , ScrollingOptions       -> {"VerticalScrollRange" -> Fit}
  , PrivateNotebookOptions -> {"ExcludeFromShutdown" -> False}
  
  ]
  
, ResetDirectory[]
];


(* ::Subsection:: *)
(*AppLoadingPanel*)


AppLoadingPanel // Options = Options @ AppNotebook;

AppLoadingPanel[options:OptionsPattern[]]:=With[
  { failedLoadSign = Style["\[WarningSign]", Blend[{Red,Orange}], 80, ShowStringCharacters->False]  
  , waitingPane = $defaultWaitingPane @ OptionValue["InitializationText"]
  , loading = (
      Print["creating notebook initialization"]
    ; PopulateLoading @ OptionValue[Automatic, Automatic, Initialization, Hold]
    )
  }
, DynamicModule[{ loaded = False, loadFailed = False, theApp }
    , 
    
    Dynamic[
        Which[ 
          Not @ TrueQ @ loaded, waitingPane
        , TrueQ @ loadFailed, failedLoadSign
        , True, Refresh[theApp, None]  
        ]
      , TrackedSymbols:>{loaded}  
      ]
    , UnsavedVariables :> {loaded, theApp}  
    , SynchronousInitialization->False
    , Initialization :> (
        loaded = False
      ; Pause[.001] 
           (*'export to notebook context'*)
      ; $CellContext`AppPanel
      ; $CellContext`AppInitialization
      
      ; Check[ ReleaseHold @ loading, loadFailed = True ]
         (*there was no theApp at the beginning but then dynamic went crazy on window resize/move*)
         (*moving rhs from theApp from top Which[] fixed the problem...*)
         (*I wasted to much time for this*)
      ; theApp = $CellContext`AppPanel[] /. _$CellContext`AppPanel -> failedLoadSign
      
      ; loaded = True  
    
      )         
    ]
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




(* ::Subsection:: *)
(*PopulateLoading*)


PopulateLoading // ClearAll
PopulateLoading // Options = {};
PopulateLoading // Attributes = {};
PopulateLoading[loadingProcedure_Hold]:= Module[
  {tag = "PopulateLoading"}
, Catch[
    Check[
      loadingProcedure /. {
        source_GetInjected :> With[
          {content = GIcontent @@ source, readFunction = GIreadFunction @@ source}
        , readFunction[content] /; True
        ]
      }
    , Throw[$Failed, tag]  
    ]
  , tag
  ]
];


GetInjected // Options = {
  "Scope" -> None
};

GetInjected::usage = "GetInjected[source, opts] is a symbolic wrapper which AppNotebook will replace with injected source";


GIcontent[file_String, ___]:= Module[
  {path = FindFile @ file}
, Print["compressing: ", File[path]]
; Compress @ Import[path, "Text"]  
];


GIreadFunction // Options = Options @ GetInjected;

GIreadFunction[spec_, OptionsPattern[]]:= GIreadFunction[spec, OptionValue["Scope"]]
GIreadFunction[spec_, None]:= Function[
  source
, Module[{stream}
  , Internal`WithLocalSettings[
      stream=StringToStream @ Uncompress @ source
    , Get @ stream
    , Close @ stream
    ]
  ]
];

GIreadFunction[spec_, {start: Begin|BeginPackage, context_String}]:= With[
  { end = start /. {Begin -> End, BeginPackage -> EndPackage}}
, Function[
    source
  , Module[{stream}
    , Internal`WithLocalSettings[
        stream=StringToStream @ Uncompress @ source
      , start[context]; Get @ stream; end[]
      , Close @ stream
      ]
    ]
 ]
]; 



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


NotebookLayouts["Basic"]:= BasicLayout;


(*FrontEnd`AbsoluteCurrentValue was casing lags*)
withNotebookMagnification// ClearAll;
withNotebookMagnification[expr_]:= Style[
  expr
, Magnification-> Dynamic @ AbsoluteCurrentValue[EvaluationNotebook[], Magnification]
];

(*withNotebookMagnification = Identity;*)


(* ::Subsection:: *)
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
(*End*)


End[];
EndPackage[];

