(* ::Package:: *)

ExtractSingleCD[CDTensor_] := Module[{CDindex, index, actingOn},
	CDindex = Head[CDTensor];
	actingOn = First[Apply[List, CDTensor]];
	index = First[Apply[List, CDindex]];
	Return[<|"actingOn" -> actingOn, "index"->index|>];
];

IsCD[expr_] := MatchQ[expr, _[__][__] /; Head[Head[expr]] === CD];

IsNotCD[expr_] := Not[IsCD[expr]]

ExtractAllCD[expr_] := Module[{current = expr, acc = {}, indices = <||>, i=1},
	If[IsNotCD[expr],
		Return[<|"base"->expr, "CDlayers"-><||>|>]
	 ];
	While[IsCD[current],
	    With[{state = ExtractSingleCD[current]},
	      AssociateTo[indices, i -> state["index"]];
	      current = state["actingOn"];
	      
	      i++
	    ];
  ];
  <|
    "CDlayers" -> indices,
    "base"   -> current
  |>
];

ConvertIndexToString[index_] := Module[{rawIndex},
Which[
	Head[index]===Times,
		rawIndex = DeleteCases[index, -1];
		Return[ToString[rawIndex]],
	Head[index]===Symbol,
		Return["^"<>ToString[index]],
	True,
		Return[Throw["Index position not identified", index]]
]
];

Clear[TensorToString];
TensorToString[bundle_] := Module[
	{components, indices, pertSymb, indicesStr, pertStr, layers, indicesCD, 
	indicesCDStr,indicesCDSymb, pertTensor, CDlayers, beautyTensors},
	pertTensor = bundle["base"];
	CDlayers = bundle["CDlayers"];
	beautyTensors = {RicciCD->R, RicciScalarCD->Ricciscalar, RiemannCD->R,
	RicciScalarCD[]->Ricciscalar};
	Which[
		Head[pertTensor]===Power,
			pertSymb = InputForm[pertTensor/.beautyTensors];
			components = {},
		Head[Head[pertTensor]]===Symbol,
			pertSymb = Head[pertTensor];
			components = Apply[List, pertTensor];
		];
		
	pertStr = ToString[pertSymb/.beautyTensors];
	
	If[components==={},
		Return[pertStr]
	];
	
	If[
	Head[First[components]]===LI,
		indices = Rest[components],
			indices = components
		];
	
	indicesStr = Map[ConvertIndexToString, indices];
	
	layers = Reverse[Sort[Keys[CDlayers]]];
	
	If[
	layers =!= {}, 
		indicesCD = Table[CDlayers[inx], {inx, layers}];
		indicesCDStr = Map[ConvertIndexToString, indicesCD];
		indicesCDSymb = Prepend[Riffle[indicesCDStr, " ;"], " ;"],
			indicesCDSymb = {};
	];
	Return[pertStr<>"{"<>StringRiffle[indicesStr, " "]<>indicesCDSymb<>"}"]
];

ConvertListTermToString[term_] := Module[{},
	Which[
	NumberQ[term]&&(term<0),
		Return[ToString[InputForm[term]]],
	NumberQ[term]&&(term>0),
		Return["+"<>ToString[InputForm[term]]],
	True,
			TensorToString[ExtractAllCD[term]]
	]
];
FromSumToList[Terms_]:=Module[{terms, listTerms},
	terms = Expand[Terms];
	Which[
	Head[terms]===Plus,
		Return[Apply[List, terms]],
	Head[terms]===Times,
		Return[{terms}]
	];
	Throw["Error in converting Sum into list"]
];

FromTimesToList[term_] := Module[{},
	If[
	Head[term]===Times,
		Return[Apply[List, term]]
	];
	Return[{term}];
];

AddPlusString[term_String]:= Which[
	StringContainsQ[term,"-"],
		term,
	StringContainsQ[term,"+"],
		term,
	True,
		"+"<>term
];

ToGRtensor[xTensorTerms_] := Module[{termsAsLists, simpSingleList, term, allterms},
	termsAsLists = Map[FromTimesToList,ScreenDollarIndices[FromSumToList[xTensorTerms]]];
	allterms = {};
	Do[simpSingleList = singleList;
		term = StringRiffle[Map[ConvertListTermToString, simpSingleList],"*"];
		term = AddPlusString[term];
		AppendTo[allterms, term]
	,{singleList, termsAsLists}];
	Return[StringRiffle[allterms, " "]]
]
