(* ::Package:: *)



Magic::usage = "Magic[n]\:53ef\:4ee5\:751f\:6210n\[Times]n\:7684\:5e7b\:65b9.\r Magic[n,d]\:751f\:6210d\:7ef4\:7684n\:9636\:5e7b\:65b9.";
MagicQ::usage = "MagicQ[n]\:68c0\:6d4b\:4e00\:4e2an\[Times]n\:5e7b\:65b9.\r MagicQ[n,3D]\:68c0\:6d4b\:4e00\:4e2an\[Times]n\[Times]n\:5e7b\:7acb\:65b9.";


MagicSquare::usage = "\:5e7b\:65b9\:5305\:63d0\:4f9b\:4e86\:4e00\:7cfb\:5217\:5e7b\:65b9\:7684\:7b97\:6cd5.";
Begin["`MagicSquare`"];


MagicSquare$Version="V1.4.0";
MagicSquare$LastUpdate="2017-12-17";


Magic::nosol="\:65e0\:89e3.";
Magic::nodef="\:65e0\:5b9a\:4e49.";
Magic::novpn="\:6570\:636e\:5e93\:8bf7\:6c42\:5931\:8d25,\:4f60\:53ef\:80fd\:9700\:8981VPN,\:6216\:8005\:4f60\:8981\:6c42\:7684\:6570\:636e\:91cf\:592a\:8fc7\:5de8\:5927,\:53ef\:4ee5\:4f7f\:7528 TimeConstraint \:9009\:9879\:589e\:52a0\:8bf7\:6c42\:65f6\:957f.";
Options[Magic]={Method->"Simple",TimeConstraint->5};
Magic[n_,d_,OptionsPattern[]]:=TimeConstrained[MagicLinker[n,d,OptionValue[Method]],OptionValue[TimeConstraint],Message[Magic::novpn]];
MagicLinker[n_,d_,p_]:=URLExecute["http://magichypercube.com/rest/hypercube/"<>p<>"/"<>ToString[n]<>"/"<>ToString[d]<>"/true","CSV"];
SetAttributes[{Magic,Magic3D},Listable];


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
Magic[n_,"3D"]:=Magic3DShow@Magic3D[n];


MagicQ[input_,"3D"]:=Magic3DQ[input];
MagicQ[matrix_]:=
Block[{SRow,SCol},
	Echo["\:8be5\:77e9\:9635\:6240\:6709\:6570\:5b57\:603b\:548c\:4e3a"<>ToString@Total[Total/@matrix]];
	SRow=Total/@matrix;
	Echo["\:8be5\:77e9\:9635\:5404\:884c\:548c\:5206\:522b\:4e3a"<>ToString@SRow];
	If[SameQ@@SRow,Echo["\:901a\:8fc7"],Return[False]];
	SCol=Total/@(Transpose@matrix);
	Echo["\:8be5\:77e9\:9635\:5404\:5217\:548c\:5206\:522b\:4e3a"<>ToString@SCol];
	If[SameQ@@SCol,Echo["\:901a\:8fc7"],Return[False]];
	Echo["\:8be5\:77e9\:9635\:4e3b\:5bf9\:89d2\:7ebf\:548c\:4e3a"<>ToString@Tr@matrix<>",\:8be5\:77e9\:9635\:4e3b\:526f\:89d2\:7ebf\:548c\:4e3a"<>	ToString@Tr[Reverse/@matrix]];
	If[SameQ[Tr@matrix,Tr[Reverse/@matrix]],True,False]
];
Magic3DQ[x3d_]:=Block[{y3d,z3d,SF,LF,TF},
	Echo["\:8be5\:7acb\:65b9\:77e9\:9635\:6240\:6709\:6570\:5b57\:603b\:548c\:4e3a"<>ToString@Total@Flatten@x3d];
	{y3d,z3d}={Transpose[x3d,{3,1,2}],Transpose[x3d,{2,3,1}]};
	SF={Total@Flatten@#&/@x3d,Total@Flatten@#&/@y3d,Total@Flatten@#&/@z3d};
	Echo["\:8be5\:7acb\:65b9\:77e9\:9635\:5404\:9762\:548c\:5206\:522b\:4e3a"<>ToString@SF];
	If[SameQ@@Flatten@SF,Echo["\:901a\:8fc7"],Return[False]];
	LF={Map[Total,x3d,{2}],Map[Total,y3d,{2}],Map[Total,z3d,{2}]};
	Echo["\:8be5\:7acb\:65b9\:77e9\:9635\:5404\:5217\:548c\:5206\:522b\:4e3a"<>ToString@LF];
	If[SameQ@@Flatten@LF,Echo["\:901a\:8fc7"],Return[False]];
	TF=Tr/@{x3d,y3d,z3d};
	Echo["\:8be5\:7acb\:65b9\:77e9\:9635\:5404\:5bf9\:89d2\:7ebf\:548c\:5206\:522b\:4e3a"<>ToString@TF];
	If[SameQ@@TF,True,False]
];


End[] ;
SetAttributes[
	{Magic,MagicQ},
	{Protected,ReadProtected}
];



