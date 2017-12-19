(* ::Package:: *)
(* ::Title:: *)
(*Nonograms(数织)*)
(* ::Subchapter:: *)
(*程序包介绍*)
(* ::Text:: *)
(*Mathematica Package*)
(*Created by Mathematica Plugin for IntelliJ IDEA*)
(*Establish from GalAster's template*)
(**)
(*Author: Roy Levien*)
(*Creation Date: 2017.12.16*)
(*Copyright:CC4.0 BY+NA+NC*)
(**)
(*该软件包遵从CC协议:署名、非商业性使用、相同方式共享*)
(**)
(*这个项目移植自 Roy Levien 的 https://github.com/orome/qr-puzzles-ma*)
(* ::Section:: *)
(*函数说明*)
BeginPackage["Nonograms`"];
NonogramsShow::usage = "";
NonogramsClues::usage = "";
NonogramsMissing::usage = "";
NonogramsSolver::usage = "";
(* ::Section:: *)
(*程序包正体*)
(* ::Subsection::Closed:: *)
(*主设置*)
Nonograms::usage = "程序包的说明,这里抄一遍";
Begin["`Private`"];
(* ::Subsection::Closed:: *)
(*主体代码*)
Nonograms$Version="V0.5";
Nonograms$LastUpdate="2017-12-19";
$Unknown = "-";
$CellGraphics = {
	1 -> Graphics[{Black, Rectangle[]}, ImageSize -> 20],
	0 -> Graphics[{White, Rectangle[]}, ImageSize -> 20],
	$Unknown-> Graphics[{GrayLevel[.90], Rectangle[]}, ImageSize -> 20]
};
$GridSpecs = Sequence[ItemSize -> {5/4,5/4}, Spacings -> {1/4, -1/4}];
(* ::Subsubsection:: *)
(*NonogramsShow*)
Options[]
NonogramsShow[t_, {cr_, cc_}] :=With[
	{lc = Max[Length/@cc], lr = Max[Length/@cr]},
	Grid[Join[
		Transpose@Join[ConstantArray["", {lr, lc}], (Style[#, Bold]& /@ PadLeft[#, lc, ""]& /@ cc)],
		MapThread[Join, {(Style[#, Bold]& /@ PadLeft[#, lr, ""]& /@ cr), (t /. $CellGraphics)}]
	],$GridSpecs]
];
NonogramsShow[t_] := Grid[t /. $CellGraphics, $GridSpecs];

(* ::Subsubsection:: *)
(*功能块 2*)
NonogramsClues[data_] :=(((Length /@ Select[Split[#], FreeQ[#, 0]&])& /@ #)& /@ {data, Transpose@data});
NonogramsMissing[goal_, partial_] := Intersection[Position[goal, #] , Position[partial, "-"]]& /@ {1, 0};
NonogramsHint[dims_] := ConstantArray[$Unknown, dims];
NonogramsHint[dims_, known_ ] := Module[{const = NonogramsHint[dims]},
	(const[[Sequence @@ #]] = 1)& /@ known[[1]]; (const[[Sequence @@ #]] = 0)& /@ known[[2]]; const
];


(* ::Subsubsection:: *)
(*NonogramsSolver*)
NonogramsSolver[clues_, given_] := Module[{poss = possibles[clues/.{}->{0}], sol},
	FixedPoint[(sol = Transpose@MapThread[constraint, {poss[[1]], #}];
	sol = Transpose@MapThread[constraint, {poss[[2]], sol}])&, given]];
NonogramsSolver[clues_] := NonogramsSolver[clues/.{}->{0}, NonogramsHint[Length /@ (clues/.{}->{0})]];

(* Generate a new row/column constraint from possible row/columns and an existing constraint. *)
constraint[_, const_] := const /; FreeQ[const, $Unknown];
constraint[poss_, const_] := Module[{constrainedPoss = Cases[poss, const /. $Unknown -> _]},
	Switch[#, Length[constrainedPoss], 1, 0, 0, _, $Unknown]& /@ (Thread[Total[#]&@constrainedPoss])];
(* TBD - This needs some fixing to be made clearer and more efficient *)
(* TBD - Can 'dim' be eliminated? *)
(* Generate all possible cells for a row/column from that row/column's clue and the dimension of the column/row *)
possible[clue_,dim_]:=Module[{spec},
	spec=Module[{specN},specN[n_]:=
		Switch[n,
			1,#,
			-1,Join[{0},
			#,{0}
		],0,{Append[#,0],Prepend[#,0]}]&/@(Union@@(Permutations/@(IntegerPartitions[dim-Plus@@clue,{Length[clue]+n}])));
	Riffle[#,clue]&/@Union[specN[-1],Union@@specN[0],specN[1]]];
	Flatten[{ConstantArray[0,#[[1]]],ConstantArray[1,#[[2]]]}&/@Partition[Append[#,0],2]]&/@spec];
possibles[clues_]:=With[
	{dims=Length/@clues},
	{possible[#,dims[[2]]]&/@clues[[1]],possible[#,dims[[1]]]&/@clues[[2]]}
];
(*Todo: 添加对非方阵的支持*)

(* ::Subsubsection:: *)
(*NonogramsSolver*)




(* ::Subsection::Closed:: *)
(*附加设置*)
End[] ;

EndPackage[];
