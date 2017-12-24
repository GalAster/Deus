(* ::Package:: *)



HanoiGraph::usage = "HanoiGraph[n]\:7ed9\:51fan\:9636\:6c49\:8bfa\:56fe";
HanoiSteps::usage = "\:7ed9\:51fa\:6c49\:8bfa\:5854\:7684\:6700\:5c11\:79fb\:52a8\:6b65\:9aa4, n>4 \:7684\:60c5\:51b5\:5c1a\:672a\:8bc1\:660e.";
HanoiMove::usage = "\:7ed9\:51fa\:6c49\:8bfa\:5854\:72b6\:6001\:95f4\:7684\:6700\:4f18\:79fb\:52a8\:65b9\:5f0f";
HanoiShow::usage = "\:53ef\:89c6\:5316\:5706\:76d8\:7684\:79fb\:52a8\:8fc7\:7a0b";


HanoiTower::usage = "\:6c49\:8bfa\:5854\:7a0b\:5e8f\:5305, \:5305\:62ec\:53ef\:89c6\:5316\:548c\:6700\:77ed\:8def\:5f84\:7b49.";
Begin["`HanoiTower`"];


HanoiTower$Version="V1.0.2";
HanoiTower$LastUpdate="2017-12-17";


AddNewRing[ops_,rod_]:=Map[Append[#,rod]&,ops,{2}];
LargestRingMove[n_,{a_,b_,c_}]:=Append[Table[a,{n}],b]<->Append[Table[a,{n}],c];
HanoiStep[ops_]:=Block[{n=If[ops=={},0,Length@ops[[1,1]]],a,b,c},
	Flatten@{AddNewRing[ops,#]&/@{a,b,c},LargestRingMove[n,RotateLeft[{a,b,c},#]]&/@Range[3]}
];
HanoiGraph[n_,ops___]:=Graph[Nest[HanoiStep,{},n],ops];


Hanoi3Pillar[1,i_,j_]:={{i,j}};
Hanoi3Pillar[n_,i_,j_]:=Join[Hanoi3Pillar[n-1,i,6-i-j],{{i,j}},Hanoi3Pillar[n-1,6-i-j,j]];
Pillar3Disp[t_,{i_,j_}]:=Module[{q=t,d},
	d=First[q[[i]]];
	q[[i]]=Rest[q[[i]]];
	q[[j]]=Prepend[q[[j]],d];
	q
];
Options[HanoiMove]={Pillar->3};
HanoiMove[n_Integer,OptionsPattern[]]:=
If[OptionValue[Pillar]==3,
	FoldList[Pillar3Disp,{Range[n],{},{}},Hanoi3Pillar[n,1,3]],
	FrameStewartAlgorithm[n,OptionValue[Pillar]]
];
sumsP[s_,i_]:={} /; (s<i)||(i==0);
sumsP[s_,s_]:={Table[1,{s}]};
sumsP[s_,1]:={{s}};
sumsP[s_,i_]:=Module[{d},Flatten[Table[Map[Join[{d},#]&,sumsP[s-d,i-1]],{d,s-i+1}],1]];
hanoiP[n_, p_] := Join[Table[1, {n - 1}], Table[0, {p - n - 1}], {2*n - 1}] /; n < p - 1;
hanoiP[n_, p_] := hanoiP[n, p] =
    Module[{v,t,i},
	t = sumsP[n - 1, p - 2];
	v = (Join[#1, {2*Plus @@ Table[Last[hanoiP[#1[[i]], p - i + 1]], {i, p - 2}] +1}] & ) /@ t;
	First[Sort[v, Last[#2] > Last[#1] & ]]
];
GenernizedHanoi[{{d_},{a_,___,b_}}]:={d,a,b};
GenernizedHanoi[{tower_,pegs_}]:=
Module[{a,pat,lp=Length[pegs],n,ans={},i,p,spread,back},
	a=Drop[hanoiP[Length[tower],lp],-1];
	pat=Table[Take[tower,{1+Sum[a[[i]],{i,1,n-1}],Sum[a[[i]],{i,1,n}]}],{n,lp-2}];
	spread=Table[p=Drop[pegs,{2,n}];
		i=Last[p];
		p[[-1]]=p[[2]];
		p[[2]]=i;
		{pat[[n]],p},{n,lp-2}];
	spread=Cases[spread,{{__},_}];
	back=({First[#1],Join[{Last[Last[#1]]},Complement[Last[#1],{Last[Last[#1]],Last[pegs]}],{Last[pegs]}]}&)/@Reverse[spread];
	(AppendTo[ans,GenernizedHanoi[#1]]&)/@spread;
	AppendTo[ans,{Last[tower],First[pegs],Last[pegs]}];
	(AppendTo[ans,GenernizedHanoi[#1]]&)/@back;Partition[Flatten[ans],3]
];
FrameStewartAlgorithm[numDisks_,numPegs_]:=
Module[{t,sH=GenernizedHanoi[{Range[numDisks],Range[numPegs]}]},
	FoldList[(t=#1;t[[#2[[2]]]]=Rest[t[[#2[[2]]]]];t[[Last[#2]]]=
	Prepend[t[[Last[#2]]],First[#2]];t)&,Join[{Range[numDisks]},
	Table[{},{numPegs-1}]],sH]
];
HanoiNum2Abc[{A_,B_,C_}]:=Block[{a,b,c,i},
	Table[Piecewise[{{a,MemberQ[A,i]},{b,MemberQ[B,i]},{c,True}}],{i,Max[A,B,C]}]
];
HanoiAbc2Num[abc_]:=Block[{a,b,c},Flatten[Position[abc,#1]]&/@{a,b,c}];
HanoiMove[start_List,finish_List]:=
	Block[{enc,dec,input,s1,s2,path,states},
	enc=Thread[Flatten@Sort@#->Range@Length[#]]&[Flatten[start]];
	dec=Reverse/@enc;
	input={start,finish}/.enc;
	{s1,s2}=HanoiNum2Abc/@input;
	path=FindShortestPath[HanoiGraph[Length[Union[Flatten[input]]]],s1,s2];
	states=HanoiAbc2Num/@path/.dec
];


DrawBackground[firststate_,tablestyle_List,pillarstyle_List]:=
Block[{pn,gap,xs,HanoiTable,height,thickness,HanoiPillar,i},
		pn=Length@firststate;
		gap=1./GoldenRatio^2;
		xs=Table[(1+gap)i,{i,-#,#}]&[(pn-1)/2];
	HanoiTable=Rectangle[{-pn(gap+1)/2,0},{pn (gap+1)/2,-pn(gap+1)gap^2.5}];
		height=gap^2(Length@Flatten@firststate+1);
		thickness=height gap^2.5;
	HanoiPillar[x0_]:=Rectangle[{-thickness/2+x0,height},{thickness/2+x0,0}];
	Flatten[{
		tablestyle,HanoiTable,
		pillarstyle,HanoiPillar/@xs
	}]
];
(*y\[Equal]k x+b/.Solve[{1\[Equal]b+k max,1/3\[Equal]b+k min},{b,k}]*)
mapping[set_,x_]:=(Max[set]-3Min[set]+2x)/(3.0(Max[set]-Min[set]));
Options[HanoiShow]={
	TableStyle->{Brown},
	PillarStyle->{RGBColor[{205,79,18}/255]},
	DiskColor->ColorData[3]
};
HanoiShow[states_,OptionsPattern[]]:=
Block[{firststate,background,newstates,pn,gap,xs,HanoiDisk,DrawState,i},
	firststate=First@states;
	background=DrawBackground[firststate,OptionValue[TableStyle],OptionValue[PillarStyle]]//Graphics;
	newstates=Map[mapping[firststate,#]&,states,1];
	pn=Length@firststate;
	gap=1./GoldenRatio^2;
	xs=Table[(1+gap)i,{i,-#,#}]&[(pn-1)/2];
	HanoiDisk[width_,{x0_,y0_}]:={
		(*y\[Equal]k x+b/.Solve[{1\[Equal]b+k 1,0\[Equal]b+k 3/10},{b,k}]*)
		OptionValue[DiskColor][(10 width-3)/7],
		Rectangle[{-width/2+xs[[x0]],gap^2y0},{width/2+xs[[x0]],gap^2(y0-1)},RoundingRadius->0.05]
	};
	DrawState[state_]:= Block[{disks,color},
		disks=MapIndexed[HanoiDisk,Reverse/@state,{2}];
		Show[background, Flatten@disks//Graphics]
	];
	DrawState/@newstates
];


H3[n_]:=2^n-1;
H4[n_]:=Evaluate[2^t(2n-4-t^2+3t)/4+1/.t->Round@Sqrt[2n]];
H5[n_]:=Evaluate[2^t(12+6n-8t+3t^2-t^3)/12-1/.t->Floor@Root[#^3-#-6n&,1]];
FrameStewartSteps[1,k_]:= 1;
FrameStewartSteps[n_,3]:= H3[n];
FrameStewartSteps[n_,k_]:= FrameStewartSteps[n,k]= Min@Table[2FrameStewartSteps[m,k]+FrameStewartSteps[n-m,k-1],{m,1,n-1}];
HanoiSteps[n_Integer,1|2]:="\:8be5\:60c5\:51b5\:65e0\:89e3"!;
HanoiSteps[n_Integer]:=HanoiSteps[n,3];
HanoiSteps[n_Integer,p_Integer]:=
    Switch[p,
	    3,H3[n],
	    4,H4[n],
	    5,H5[n],
	    _,FrameStewartSteps[n,p]
];


End[];
SetAttributes[
	{HanoiTower,HanoiGraph,HanoiMove,HanoiShow,HanoiSteps},
	{Protected,ReadProtected}
];



