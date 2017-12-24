(* ::Package:: *)



Poker24::usage = "\:7ecf\:5178\:95ee\:9898,4\:5f20\:724c\:7b9724\:70b9";
Calculate100::usage = "\:7ecf\:5178\:95ee\:9898,9\:4e2a\:6570\:5b57\:6dfb\:52a0\:7b26\:53f7\:8ba1\:7b97100";
Proof1926::usage = "\:7ecf\:5178\:95ee\:9898, \:67d0\:4e24\:4e2a\:4eba\:7269\:751f\:65e5\:7684\:6570\:5b57\:8bba\:8bc1";


NumberMaster::usage = "\:7a0b\:5e8f\:5305\:7684\:8bf4\:660e,\:8fd9\:91cc\:6284\:4e00\:904d";
Begin["`NumberMaster`"];


NumberMaster$Version="V1.6";
NumberMaster$LastUpdate="2017-12-24";


div[a_,0]:=ComplexInfinity;
log[a_,b_]:=Log[a,b];
pow[a_,b_]:=Power[a,b];
root[a_,b_]:=pow[a,1/b];
reduceRule=log[a_,pow[b_,c_]]:>c+log[a,b];
(*
div[a_,b_]:=Indeterminate/;Abs[b]>10^10;
root[a_,b_]:=Indeterminate/;Abs[a]>10^10;
pow[a_,pow[b_,c_]]:=$Failed;
pow[a_,b_/;b>10]:=$Failed;
pow[a_,b_/;b<0]:=If[a<=0,$Failed,Power[a,b]];
pow[a_,ComplexInfinity]:=$Failed;
log[1,b_]:=$Failed;
log[a_,b_/;b<0]:=$Failed;
log[a_/;a<0,b_]:=$Failed;
root[a_,0]:=$Failed;
root[a_/;a<=0,b_]:=$Failed;
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
opsName=Thread[ {plus,minus,times,div,pow,log,root,aa,cc}->
				{Plus,Subtract,Times,Divide,Power,Log,Surd,FactorialPower,Binomial}
];


treeR[1]=n;
treeR[n_]:=treeR[n]=Table[o[treeR[a],treeR[n-a]],{a,1,n-1}];
treeC[n_]:=Flatten[treeR[n]//.{o[a_List,b_]:>(o[#,b]&/@a),o[a_,b_List]:>(o[a,#]&/@b)}];


PokerFilter[l_Integer]:=Block[
	{nn,oo,ff,cal},
	nn=Array[ToExpression["n"<>ToString@#]&,l];
	oo=Array[ToExpression["o"<>ToString@#]&,l-1];
	ff=ReplacePart[#,Thread[Position[#,n]->nn]~Join~Thread[Position[#,o]->oo]]&;
	Function[Evaluate@Join[oo,nn],Evaluate[HoldForm/@Evaluate[ff/@treeC[l]]]]
];
Options[Poker24Off]={Rule->{Plus,Subtract,Times,div}};
Poker24Off[nList_List,goal_Integer,OptionsPattern[]]:=Block[
	{l=Length@nList,ops,filter,pts,cas},
	ops=OptionValue[Rule];
	filter=PokerFilter[l];
	pts=Outer[filter@@Join[#1,#2]&,Tuples[ops,l-1],Permutations[nList],1];
	cas=Cases[pts,e_/;ReleaseHold@e===goal,{3}];
	DeleteDuplicatesBy[cas,ReleaseHold[#/.Thread[nList->CharacterRange[97,96+l]]]&]
];
Poker24Min[nList_List,goal_Integer]:=Block[
	{l=Length@nList,ops,filter,pts,ext,mc},
	ops={Plus,Subtract,Times,Divide,pow,log,root};
	filter=PokerFilter[l];
	pts=Outer[filter@@Join[#1,#2]&,Tuples[ops,l-1],Permutations[nList],1]/.reduceRule;
	mc[pt_]:=MemoryConstrained[Chop[pt-goal//N//ReleaseHold,10^(-9)],10^4];
	ext=AbortProtect@Extract[pts,Position[Map[mc,pts,{3}],0,{3}]];
	DeleteDuplicatesBy[ext,ReleaseHold[#/.Thread[nList->CharacterRange[97,96+l]]]&]
];
Poker24All[nList_List,goal_Integer]:=Block[
	{l=Length@nList,ops,filter,pts,ext,mc},
	ops={Plus,Subtract,Times,Divide,pow,log,root,FactorialPower,Binomial};
	filter=PokerFilter[l];
	pts=Outer[filter@@Join[#1,#2]&,Tuples[ops,l-1],Permutations[nList],1]/.reduceRule;
	mc[pt_]:=MemoryConstrained[Chop[pt-goal//N//ReleaseHold,10^(-9)],10^4];
	ext=AbortProtect@Extract[pts,Position[Map[mc,pts,{3}],0,{3}]];
	DeleteDuplicatesBy[ext,ReleaseHold[#/.Thread[nList->CharacterRange[97,96+l]]]&]
];
Poker24::memb = "\:8ba1\:7b97 `1` \:7684\:8fc7\:7a0b\:4e2d\:4e0d\:80fd\:542b\:6709 `1` !";
Options[Poker24]={Number->24,Extension->Off};
Poker24[input_,OptionsPattern[]]:=Block[
	{goal,ans},
	goal=OptionValue[Number];
	If[MemberQ[input, goal],Return@Message[Poker24::memb,goal]];
	ans=Quiet@Switch[OptionValue[Extension],
		Off,Poker24Off[input,goal],
		Min,Poker24Min[input,goal],
		All,Poker24All[input,goal],
		Max,Echo["","\:8be5\:51fd\:6570\:5c1a\:672a\:5b8c\:6210!"],
		__,Poker24Off[input,goal,Rule->OptionValue[Extension]]
	]/.opsName
];


next`ops=HoldForm/@{Plus,Times,Divide,Subtract};
(nextOp[#1]=#2)&@@@Most@Transpose@{next`ops,RotateLeft@next`ops};
next`children=True;
SetAttributes[{next`Plus,next`Times},Flat];
next[{i_}]:=False;
next[l_List]:=HoldForm[Plus][{l[[1]]},l[[2;;]]];
next[op_[arg1_,arg2_]]/;next`children:=With[{res=next[arg1]},op[res,arg2]/;res=!=False];
next[op_[arg1_,arg2_]]/;next`children:=With[{res=next[arg2]},op[arg1,res]/;res=!=False];
next[HoldForm[Subtract][arg1_,arg2:{_}]]:=False;
next[op_[arg1_,arg2_]]:=Block[
	{next`children=False},
	next[op[flatten@arg1,flatten@arg2]]
];
next[op_[arg1_List,{arg2_}]]:=nextOp[op][{arg1[[1]]},arg1[[2;;]]~Append~arg2];
next[op_[arg1_List,arg2_List]]:=op[arg1~Append~First@arg2,Rest@arg2];
flatten[exp_]:=Flatten@Cases[exp,{_},{0,Infinity}];
formattingRules={
	i:{__Integer}:>FromDigits@i,
	HoldForm[Plus]->next`Plus,
	HoldForm[Times]->next`Times,
	HoldForm[Subtract]->(next`Plus[#1,Times[-1,#2]]&),
	HoldForm[Divide]->next`Divide
};
formattingRev={next`Plus->Plus,next`Times->Times,next`Divide->Divide};
doMath[expr_]:=expr/.List->Composition[FromDigits,List]//ReleaseHold;
Calculate100[input_List,target_Integer]:=Block[
	{curr=input,ans={},dup},
	CheckAbort[Quiet@While[curr=!=False,
		If[doMath@curr==target,
			PrintTemporary[curr/.formattingRules];
			AppendTo[ans,curr/.formattingRules]];
			curr=next@curr
		];Echo["\:6240\:6709\:8ba1\:7b97\:5df2\:5b8c\:6210!","\:8fd0\:7b97: "], Echo["\:7528\:6237\:4e2d\:65ad\:4e86\:8ba1\:7b97!","\:8fd0\:7b97: "]];
	dup=ReleaseHold[#/.Thread[input->CharacterRange[97,96+Length@input]]]&;
	DeleteDuplicatesBy[HoldForm/@ans/.formattingRev,dup]
];


Options[Calculate100TC]={TimeConstraint->2};
Calculate100TC[input_List,target_Integer,OptionsPattern[]]:=Block[
	{curr=input,ans={}},
	CheckAbort[TimeConstrained[
		Quiet@While[curr=!=False,
			If[doMath@curr==target,AppendTo[ans,curr/.formattingRules]];
			curr=next@curr
		],OptionValue[TimeConstraint]],ans];
	DeleteDuplicatesBy[HoldForm/@ans/.formattingRev,ReleaseHold[#/.Thread[input->CharacterRange[97,96+Length@input]]]&]
];
he=HoldForm[a_]==HoldForm[b_]:>HoldForm[InputForm[a==b]];
Proof1926::ntime=" `1` s\:5185\:627e\:4e0d\:5230\:89e3, \:8bf7\:7ed9 TimeConstraint +1s.";
Options[Proof1926]={Number->Automatic,TimeConstraint->1};
Proof1926[input_,target_,OptionsPattern[]]:=Block[
	{inl=IntegerDigits@ToExpression[input],
		str=StringPartition[ToString[target],1],
		ops,tar,ans,out
	},
	ops=RandomChoice[{8,7,4,1}->{"+","-","*","^"},Length@str-1];
	tar=If[OptionValue[Number]===Automatic,ToExpression@StringJoin@Riffle[str,ops],OptionValue[Number]];
	ans=Calculate100TC[inl,tar,TimeConstraint->OptionValue[TimeConstraint]];
	err=If[#=={},Return@Message[Proof1926::ntime,OptionValue[TimeConstraint]],RandomChoice@#]&;
	If[OptionValue[Number]===Automatic,
		tar=ToExpression[StringJoin@Riffle[str,ops],StandardForm,HoldForm],
		tar=Calculate100TC[str//StringJoin//ToExpression//IntegerDigits,OptionValue[Number],TimeConstraint->OptionValue[TimeConstraint]];
		tar=err@tar;
	];
	out=err@ans==tar/.he;
	If[OptionValue[Number]===Automatic,Return[out],Return[out==OptionValue[Number]]]
];


End[] ;
SetAttributes[
	{Poker24,Calculate100,Proof1926},
	{Protected,ReadProtected}
];



