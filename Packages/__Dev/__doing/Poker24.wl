(* ::Package:: *)
(* ::Title:: *)
(*Example(样板包)*)
(* ::Subchapter:: *)
(*程序包介绍*)
(* ::Text:: *)
(*Mathematica Package*)
(*Created by Mathematica Plugin for IntelliJ IDEA*)
(*Establish from GalAster's template*)
(**)
(*Author:我是作者*)
(*Creation Date:我是创建日期*)
(*Copyright:CC4.0 BY+NA+NC*)
(**)
(*该软件包遵从CC协议:署名、非商业性使用、相同方式共享*)
(**)
(*这里应该填这个函数的介绍*)
(* ::Section:: *)
(*函数说明*)
BeginPackage["Example`"];
ExampleFunction::usage = "这里应该填这个函数的说明,如果要换行用\"\\r\"\r就像这样";
(* ::Section:: *)
(*程序包正体*)
(* ::Subsection::Closed:: *)
(*主设置*)
ExNumber::usage = "程序包的说明,这里抄一遍";
Begin["`Private`"];
(* ::Subsection::Closed:: *)
(*主体代码*)
Example$Version="V1.0";
Example$LastUpdate="2016-11-11";
(* ::Subsubsection:: *)
(*运算符重载,减枝*)
plus[a_,b_]:=Plus[a,b];
minus[a_,b_]:=Subtract[a,b];
times[a_,b_]:=Times[a,b];
div[a_,b_]:=Divide[a,b];
pow[a_,b_]:=Power[a,b];
log[a_,b_]:=Log[a,b];
root[a_,b_]:=Power[a,1/b];
aa[a_,b_]:=FactorialPower[a,b];
cc[a_,b_]:=Binomial[a,b];
cc[a_,b_/;a>5]:=If[a>100,Null,Binomial[a,b]];
aa[a_,b_/;a>5]:=If[a>20,Null,FactorialPower[a,b]];
pow[a_,pow[b_,c_]]:=$Failed;
pow[a_,b_]:=$Failed/;b>10;

(*
div[a_,0]:=$Failed;
div[a_,b_]:=$Failed/;b>10^3;
log[a_,b_]:=$Failed/;a<0;
root[a_,b_]:=$Failed;
*)






(* ::Subsubsection:: *)
(*卡特兰树*)
treeR[1]=n;
treeR[n_]:=treeR[n]=Table[o[treeR[a],treeR[n-a]],{a,1,n-1}];
treeC[n_]:=Flatten[treeR[n]//.{o[a_List,b_]:>(o[#,b]&/@a),o[a_,b_List]:>(o[a,#]&/@b)}];

(* ::Subsubsection:: *)
(*Poker24*)
Options[Poker24]={Number->24,Extension->Off};
Poker24[input_List,OptionsPattern[]]:=Block[
	{l=Length@input,ops,nn,oo,ff,filter,cal},
	ops=Switch[OptionValue[Extension],
		Off,{Plus,Subtract,Times,Divide},
		Min,{Plus,Subtract,Times,Divide,pow,log,root},
		Full,Return[Echo["","该功能未完成"]],
		test,{Plus,Subtract,Times,Divide,pow,log,root,Min,Max,aa,cc},
		__,OptionValue[Extension]
	];
	nn=Array[ToExpression["n"<>ToString@#]&,l];
	oo=Array[ToExpression["o"<>ToString@#]&,l-1];
	ff=ReplacePart[#,Thread[Position[#,n]->nn]~Join~Thread[Position[#,o]->oo]]&;
	filter=Function[Evaluate@Join[oo,nn],Evaluate[HoldForm/@Evaluate[ff/@treeC[l]]]];
	cal[nList_]:=DeleteDuplicatesBy[
		Cases[
			Outer[filter@@Join[#1,#2]&,Tuples[ops,l-1],Permutations[nList],1],
			e_/;Quiet@ReleaseHold@e===OptionValue[Number],{3}
		],ReleaseHold[#/.Thread[nList->nn]]&
	];
	cal[input]/.{pow->Power,log->Log,root->Surd}
];


(* ::Subsection::Closed:: *)
(*附加设置*)
End[] ;

EndPackage[];
