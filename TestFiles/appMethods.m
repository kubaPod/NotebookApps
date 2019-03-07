
(*you can assume that sessions symbols are imported here so use them freely*)
(*ale packages specified in loading are here too*)

(*AppPanel and AppInitialization functions need to have those exact names *)

AppPanel[]:= NotebookLayouts["Basic"][
    importPanel[]
  , plotPanel[]
  , settingsPanel[]
];

AppInitialization[]:= ($data = Sort@RandomReal[1, {100, 2}]);


(* internal part*)


importPanel[]:=Grid[{{
  Button["shuffleData", $data = Sort@RandomReal[1, {100, 2}]],
  Button["show session Preview", CreateSessionPreview[], Method->"Queued"],
  Button["save session", BookmarkSession[], Method->"Queued"],
  Button["load session", BookmarkSessionLoad[], Method->"Queued"]
  
}}];



plotPanel[]:= Graphics[
  {Thick, Dynamic @ $frameColor, Line @ Dynamic @ $data}
  , AspectRatio->Full
  , ImageSize -> Full
  , ImagePadding->33
  , Background->White
(*, Frame -> True
, FrameTicks->All*)
  , FrameStyle -> Dynamic @ Directive[Thick, $frameColor]
];

settingsPanel[] = Column[{
  ColorSlider[Dynamic@$frameColor]
  , Slider @ Dynamic @ $x
  , Dynamic[{$Context,RandomReal[]}, UpdateInterval->2, TrackedSymbols:>{}]
  , Dynamic[{$HomeDirectory, RandomReal[]}, UpdateInterval->2]
}];




