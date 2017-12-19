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
ExampleFunction::usage = "这里应该填这个函数的说明,如果要换行用\"\r\"\r就像这样";
(* ::Section:: *)
(*程序包正体*)
(* ::Subsection::Closed:: *)
(*主设置*)
Nonograms::usage = "程序包的说明,这里抄一遍";
Begin["`Private`"];
(* ::Subsection::Closed:: *)
(*主体代码*)
Nonograms$Version="V1.0";
Nonograms$LastUpdate="2017-12-18";
$Unknown = "-";
$CellGraphics = {
	1 -> Graphics[{Black, Rectangle[]}, ImageSize -> 20],
	0 -> Graphics[{White, Rectangle[]}, ImageSize -> 20],
	unknown-> Graphics[{GrayLevel[.90], Rectangle[]}, ImageSize -> 20]
};
$GridSpecs = Sequence[ItemSize -> {5/4,5/4}, Spacings -> {1/4, -1/8}];
(* ::Subsubsection:: *)
(*NonogramsShow*)
NonogramsShow[t_, {cr_, cc_}] :=
With[{lc = Max[Length/@cc], lr = Max[Length/@cr]},
	Grid[Join[
		Transpose@Join[ConstantArray["", {lr, lc}], (Style[#, Bold]& /@ PadLeft[#, lc, ""]& /@ cc)],
		MapThread[Join, {(Style[#, Bold]& /@ PadLeft[#, lr, ""]& /@ cr), (t /. $CellGraphics)}]
	],$GridSpecs]
];
NonogramsShow[t_] := Grid[t /. $CellGraphics, $GridSpecs];

(* ::Subsubsection:: *)
(*功能块 2*)
ExampleFunction[2]="我就是个示例函数,什么功能都没有";


(* ::Subsection::Closed:: *)
(*附加设置*)
End[] ;

EndPackage[];
