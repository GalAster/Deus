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
Poker24::usage = "";

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
cc[a_,b_/;a>5]:=If[a>100,$Failed,Binomial[a,b]];
aa[a_,b_/;a>5]:=If[a>20,$Failed,FactorialPower[a,b]];
pow[a_,pow[b_,c_]]:=$Failed;
pow[a_,b_/;b>10]:=$Failed;
pow[a_,b_/;b<0]:=If[a<=0,$Failed,Power[a,b]];
pow[a_,ComplexInfinity]:=$Failed;
div[a_,0]:=$Failed;
(*
div[a_,b_]:=$Failed/;b>10^3;
log[a_,b_]:=$Failed/;a<0;
root[a_,b_]:=$Failed;
*)
plus[a_,b_]:=Plus[a,b];
minus[a_,b_]:=Subtract[a,b];
times[a_,b_]:=Times[a,b];
div[a_,b_]:=Divide[a,b];
pow[a_,b_]:=Power[a,b];
log[a_,b_]:=Log[a,b];
root[a_,b_]:=pow[a,1/b];
aa[a_,b_]:=FactorialPower[a,b];
cc[a_,b_]:=Binomial[a,b];

name=Thread[{plus,minus,times,div,pow,log,root,aa,cc}->
  {Plus,Subtract,Times,Divide,Power,Log,Surd,FactorialPower,Binomial}];
(* ::Subsubsection:: *)
(*卡特兰树*)
treeR[1]=n;
treeR[n_]:=treeR[n]=Table[o[treeR[a],treeR[n-a]],{a,1,n-1}];
treeC[n_]:=Flatten[treeR[n]//.{o[a_List,b_]:>(o[#,b]&/@a),o[a_,b_List]:>(o[a,#]&/@b)}];
(* ::Subsubsection:: *)
(*Poker24*)
PokerFilter[l_Integer]:=Block[
	{nn,oo,ff,cal},
	nn=Array[ToExpression["n"<>ToString@#]&,l];
	oo=Array[ToExpression["o"<>ToString@#]&,l-1];
	ff=ReplacePart[#,Thread[Position[#,n]->nn]~Join~Thread[Position[#,o]->oo]]&;
	Function[Evaluate@Join[oo,nn],Evaluate[HoldForm/@Evaluate[ff/@treeC[l]]]]
];
Options[Poker24]={Number->24,Extension->Off};
Poker24[nList_List,OptionsPattern[]]:=Block[
	{l=Length@nList,ops,filter,pts,cas},
	ops=Switch[OptionValue[Extension],
		Off,{Plus,Subtract,Times,Divide},
		Min,{Plus,Subtract,Times,div,pow,log,root},
		Full,Return[Echo["","该功能未完成"]],
		test,{Plus,Subtract,Times,Divide,pow,log,root,Min,Max,aa,cc},
		__,OptionValue[Extension]
	];
	filter=PokerFilter[l];
	pts=Outer[filter@@Join[#1,#2]&,Tuples[ops,l-1],Permutations[nList],1];
	cas=Cases[pts,e_/;ReleaseHold@e===OptionValue[Number],{3}];
	DeleteDuplicatesBy[cas,ReleaseHold[#/.Thread[nList->CharacterRange[97,96+l]]]&]
]
(* ::Subsection::Closed:: *)
(*附加设置*)
End[] ;

EndPackage[];
