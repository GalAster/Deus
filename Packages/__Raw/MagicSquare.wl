(* ::Package:: *)
(* ::Title:: *)
(*MagicSquare*)
(* ::Subchapter:: *)
(*程序包介绍*)
(* ::Text:: *)
(*Mathematica Package*)
(*Created by Mathematica Plugin for IntelliJ IDEA*)
(*Establish from GalAster's template*)
(**)
(*Author:GalAster*)
(*Creation Date:2016-07-12*)
(*Copyright:CC4.0 BY+NA+NC*)
(**)
(*该软件包遵从CC协议:署名、非商业性使用、相同方式共享*)
(**)
(*这里应该填这个函数的介绍*)
(* ::Section:: *)
(*函数说明*)
BeginPackage["MagicSquare`"];
Magic::usage = "Magic[n]可以生成n×n的幻方.\r Magic[n,d]生成d维的n阶幻方.";
MagicQ::usage = "MagicQ[n]检测一个n×n幻方.\r MagicQ[n,3D]检测一个n×n×n幻立方.";
(* ::Section:: *)
(*程序包正体*)
(* ::Subsection::Closed:: *)
(*主设置*)
MagicSquare::usage = "幻方包提供了一系列幻方的算法.";
Begin["`Private`"];
(* ::Subsection::Closed:: *)
(*主体代码*)
MagicSquare$Version="V1.4.0";
MagicSquare$LastUpdate="2017-12-17";
(* ::Subsubsection:: *)
(*幻方选项代码*)
Magic::nosol="无解.";
Magic::nodef="无定义.";
Magic::novpn="数据库请求失败,你可能需要VPN,或者你要求的数据量太过巨大,可以使用 TimeConstraint 选项增加请求时长.";
Options[Magic]={Method->"Simple",TimeConstraint->5};
Magic[n_,d_,OptionsPattern[]]:=TimeConstrained[MagicLinker[n,d,OptionValue[Method]],OptionValue[TimeConstraint],Message[Magic::novpn]];
MagicLinker[n_,d_,p_]:=URLExecute["http://magichypercube.com/rest/hypercube/"<>p<>"/"<>ToString[n]<>"/"<>ToString[d]<>"/true","CSV"];
SetAttributes[{Magic,Magic3D},Listable];
(* ::Subsubsection:: *)
(*幻方生成核心代码*)
Magic[n_?OddQ]:=Block[{p},p=Range[n];
Outer[Plus,p,p-(n+3)/2]~Mod~n*n+Outer[Plus,p,2p-2]~Mod~n+1];
Magic[n_/;n~Mod~4==0]:=
Block[{J,K1,M},
	J=Floor[(Range[n]~Mod~4)/2.0];
	K1=Abs@Outer[Plus,J,-J]~BitXor~1;
	M=Outer[Plus,Range[1,n^2,n],Range[0,n-1]];
	M+K1(n*n+1-2M)
]//Experimental`CompileEvaluate;
Magic[2]:=Message[Magic::nosol];
Magic[n_?EvenQ]:=
Block[{p,M,i,j,k},
	p=n/2;
	M=Magic[p];
	M=ArrayFlatten@{{M,M+2p^2},{M+3p^2,M+p^2}};
	If[n==2,Return[M]];
	i=Transpose@{Range@p};
	k=(n-2)/4;
	j=Range[k]~Join~Range[n-k+2,n];
	M[[Flatten@{i,i+p},j]]=M[[Flatten@{i+p,i},j]];
	i=k+1;j={1,i};
	M[[Flatten@{i,i+p},j]]=M[[Flatten@{i+p,i},j]];
	M
];
Magic[x_]:=Message[Magic::nodef];
(* ::Subsubsection:: *)
(*3D幻方生成核心代码*)
Magic[n_,3]:=Magic3D[n];
Magic3D[n_?OddQ]:=Table[n^2Mod[i-j+k-1,n]+n Mod[i-j-k,n]+Mod[i+j+k-2,n]+1,{i,1,n},{j,1,n},{k,1,n}];
Magic3D[n_/;n~Mod~4==0]:=
Block[{QMagic,FMagic},
	QMagic[x_]:=If[1<=x<=n/2,0,1];
	FMagic[i_,j_,k_]:=Mod[i+j+k+QMagic[i]+QMagic[j]+QMagic[k],2];
	Table[If[FMagic[i,j,k]==1,(i-1)n^2+(j-1)n+k,1-k+n(1-j+n(1-i+n))],{i,1,n},{j,1,n},{k,1,n}]
];
Magic3D[2]:=Message[Magic::nosol];
Magic3D[n_?EvenQ]:=
Block[{QMagic,XMagic,d,uptab,downtab},
	QMagic[x_]:=If[1<=x<=n/2,0,1];
	XMagic[x_]:=Min[x,n+1-x];
	u=Mod[XMagic[i]-XMagic[j]+XMagic[k],n/2]+1;
	v=4QMagic[i]+2QMagic[j]+QMagic[k]+1;
	d[1,v_]:={7,3,6,2,5,1,4,0}[[v]];
	d[2,v_]:={3,7,2,6,1,5,0,4}[[v]];
	d[3,v_]:={0,1,3,2,5,4,6,7}[[v]];
	d[u_,v_]:=If[Mod[u,2]===0,v-1,8-v];
	uptab=Table[(n/2)^3d[u,v],{i,1,n},{j,1,n},{k,1,n}];
	downtab=Table[(n/2)^2Mod[i-j+k-1,n/2]+(n/2)Mod[i-j-k,n/2]+Mod[i+j+k-2,n/2]+1,{i,1,n},{j,1,n},{k,1,n}];
	uptab+downtab
];
Magic3D[x_]:=Message[Magic::nodef];
Magic3DShow[n_]:={
	Graph3D@GridGraph[{n,n,n},VertexLabels->Table[i->Flatten[Magic3D@n][[i]],{i,n^3}]],
	MatrixForm/@Magic3D[n]
};
(* ::Subsubsection:: *)
(*幻方判别过程*)
MagicQ[input_,3D]:=Magic3DQ[input];
MagicQ[matrix_]:=
Block[{SRow,SCol},
	Echo["该矩阵所有数字总和为"<>ToString@Total[Total/@matrix]];
	SRow=Total/@matrix;
	Echo["该矩阵各行和分别为"<>ToString@SRow];
	If[SameQ@@SRow,Echo["通过"],Return[False]];
	SCol=Total/@(Transpose@matrix);
	Echo["该矩阵各列和分别为"<>ToString@SCol];
	If[SameQ@@SCol,Echo["通过"],Return[False]];
	Echo["该矩阵主对角线和为"<>ToString@Tr@matrix<>",该矩阵主副角线和为"<>	ToString@Tr[Reverse/@matrix]];
	If[SameQ[Tr@matrix,Tr[Reverse/@matrix]],True,False]
];
Magic3DQ[x3d_]:=Block[{y3d,z3d,SF,LF,TF},
	Echo["该立方矩阵所有数字总和为"<>ToString@Total@Flatten@x3d];
	{y3d,z3d}={Transpose[x3d,{3,1,2}],Transpose[x3d,{2,3,1}]};
	SF={Total@Flatten@#&/@x3d,Total@Flatten@#&/@y3d,Total@Flatten@#&/@z3d};
	Echo["该立方矩阵各面和分别为"<>ToString@SF];
	If[SameQ@@Flatten@SF,Echo["通过"],Return[False]];
	LF={Map[Total,x3d,{2}],Map[Total,y3d,{2}],Map[Total,z3d,{2}]};
	Echo["该立方矩阵各列和分别为"<>ToString@LF];
	If[SameQ@@Flatten@LF,Echo["通过"],Return[False]];
	TF=Tr/@{x3d,y3d,z3d};
	Echo["该立方矩阵各对角线和分别为"<>ToString@TF];
	If[SameQ@@TF,True,False]
];
(* ::Subsection::Closed:: *)
(*附加设置*)
End[] ;
SetAttributes[
	{Magic,MagicQ},
	{Protected,ReadProtected}
];
EndPackage[];