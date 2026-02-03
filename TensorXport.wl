(* ::Package:: *)

ExtractSingleCD[CDTensor_] := Module[{CDindex, index, actingOn},
	CDindex = Head[CDTensor];
	actingOn = First[Apply[List, CDTensor]];
	index = First[Apply[List, CDindex]];
	Return[<|"actingOn" -> actingOn, "index"->index|>];
];

IsCD[expr_] := MatchQ[expr, _[__][__] /; Head[Head[expr]] === CD];

IsNotCD[expr_] := Not[IsCD[expr]]

ExtractAllCD[factor_] := Module[{current = factor, acc = {}, indices = <||>, i=1},
	If[IsNotCD[factor],
		Return[<|"base"->factor, "CDlayers"-><||>|>]
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
	
	Which[
	(components==={})&&(CDlayers===<||>),
		Return[pertStr],
	components=!={},
		If[Head[First[components]]===LI,
			indices = Rest[components],
				indices = components
		],
	components==={},
		indices = {}
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
	Or[(Head[terms]===Times), (Head[Head[terms]]===Inactive[CD])],
		Return[{terms}]
	];
	Throw["Error in converting Sum To List", Terms];
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
	Do[
		term = StringRiffle[Map[ConvertListTermToString, singleList],"*"];
		term = AddPlusString[term];
		AppendTo[allterms, term]
	,{singleList, termsAsLists}];
	Return[StringRiffle[allterms, " "]]
];

"""
-----------------------------------------

	Integration by parts

-----------------------------------------
"""

ContainsCDQ[expr_] := !FreeQ[expr, CD];
PluralCDQ[term_] := Module[{factorsWithCD},
	factorsWithCD = Select[FromTimesToList[term], ContainsCDQ];
	If[Length[factorsWithCD]>1,
		Return[True],
			Return[False]
	];
];

CountCDdegree[factor_Association] :=  Module[{},
	If[Head[factor["base"]]===hh,
		Return[Length[factor["CDlayers"]]],
			Return[0]
	]];

ChoosePertFactor[CDdegree_List] := Module[{CDdegreeOnlyPert},
	CDdegreeOnlyPert = DeleteCases[CDdegree, 0];
	First@FirstPosition[CDdegree,
	  Min[CDdegreeOnlyPert]
]];

GetLayers[CDlayers_] := Reverse[Sort[Keys[CDlayers]]];

ActCDLayers[expr_, CDlayers_] := Module[{layers, exprLoop},
	layers = GetLayers[CDlayers];
	exprLoop = expr;
	Do[
		exprLoop = CD[CDlayers[indx]][exprLoop]
	, {indx, layers}];
	Return[exprLoop]
]

IntByPartSingle[term_] := Module[
	{DecompositionCD, factorList, CDdegrees, indxToInt, BundleToInt, layersFromOutside,
	CDlayers, rest, base},
	If[Not[PluralCDQ[term]],
		Return[<|"bdy_term" -> 0, "bulk_term" -> term|>]
	];
	factorList = FromTimesToList[term];
	DecompositionCD = Map[ExtractAllCD, factorList];
	CDdegrees = Map[CountCDdegree, DecompositionCD];
	indxToInt = ChoosePertFactor[CDdegrees];
	rest = Apply[Times, Delete[factorList, indxToInt]];
	BundleToInt = DecompositionCD[[indxToInt]];
	CDlayers = BundleToInt["CDlayers"];
	base = BundleToInt["base"];
	layersFromOutside = Sort[Keys[CDlayers]];
	CDlayersReduced = CDlayers; 
	restIter = rest;
	boundaryterms = {};
	Do[
		CDlayersReduced = KeyDrop[CDlayersReduced, indx]; 
		PertIter = ActCDLayers[base, CDlayersReduced];
		state = <|
			"bdy_term" -> Inactive[CD][CDlayers[indx]][restIter*PertIter],
			"bulk" -> -Inactive[CD][CDlayers[indx]][restIter]*PertIter
		|>;
		AppendTo[boundaryterms, state["bdy_term"]];
		restIter = -Inactive[CD][CDlayers[indx]][restIter];
	,{indx, layersFromOutside}];
	Return[
		<|"bdy_term" -> Apply[Plus, boundaryterms],
		  "bulk_term" -> state["bulk"]|>
		]
]



PrepareBdyList[bdyTerms_] := Module[{nonzeroBdyTerms}, 
	nonzeroBdyTerms = Select[bdyTerms, #=!=0&];
	Return[
		Flatten[Map[FromSumToList, nonzeroBdyTerms]]
	];
];

ExtractSingleDerivative[term_,normal_, index_] := Module[
	{vector, indexVecBare, mapIndex, indexCD, ShouldBeCD},

	ShouldBeCD = Head[Head[term]];
	If[(ShouldBeCD =!= CD) && (ShouldBeCD =!= Inactive[CD]),
		Throw["Error: boundary term has not CD factorized", term]
	];
	vector = First[Apply[List, term]];
	indexCD = First[Apply[List, Head[term]]];
	Return[normal[indexCD]*vector]
]; 


ExtractBdyDerivative[bdyTerms_List, normal_, newIndex_] := Module[{bdytermsSimp, allbdy},
	bdytermsSimp = PrepareBdyList[bdyTerms];
	allbdy = Total[Map[ExtractSingleDerivative[#, normal, newIndex]&, bdytermsSimp]];
	Return[Activate[allbdy]];
];

ClearAll[IntegrateByParts, SetNormalVector];

SetNormalVector[norm_, M_] := <|"normal"->norm, "index"->DummyIn[TangentM]|>;

IntegrateByParts[BoundaryObjects_][xTensorTerms_] := Module[
	{termsAsLists, simpSingleList, term, allterms, terms},
	terms = ScreenDollarIndices[FromSumToList[xTensorTerms]];
	allterms = {};
	Do[
		AppendTo[allterms, IntByPartSingle[term]]
	,{term, terms}];
	
	Return[
	<|
  	"bdy_term"  -> ExtractBdyDerivative[allterms[[All, "bdy_term"]], 
  										BoundaryObjects["normal"], 
  										BoundaryObjects["index"]
  										],
  	"bulk_term" -> Activate[Total[allterms[[All, "bulk_term"]]]]
	|>]
];

ExtractNormalVector[BoundaryObjects_, index_][bdyTerm_] := ContractMetric[VarD[BoundaryObjects["normal"][-index], CD][bdyTerm]];
