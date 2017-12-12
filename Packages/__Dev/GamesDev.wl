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
(*Author:GalAster*)
(*Creation Date:2016-12-06*)
(*Copyright:CC4.0 BY+NA+NC*)
(**)
(*该软件包遵从CC协议:署名、非商业性使用、相同方式共享*)
(**)
(*这里应该填这个函数的介绍*)
(* ::Section:: *)
(*函数说明*)
BeginPackage["OtherGames`"];
GraphRPS::usage="GraphRPS[n]给出n元猜拳的胜负判定图.";
MatrixRPS::usage="MatrixRPS[n]可以输出n元猜拳的关系矩阵.";
RPSQ::usage="RPSQ[k,{x,n}]判定n元猜拳中{x,n}的胜负向量,1为胜,0为平,-1为败.";
GameRPS::usage="GameRPS[],与Mathematica AI玩猜拳!你觉得猜拳有策略吗?";
(* ::Section:: *)
(*程序包正体*)
(* ::Subsection::Closed:: *)
(*主设置*)
OtherGames$Version="V0.2";
OtherGames$Environment="V11.0+";
OtherGames$LastUpdate="2016-12-19";
OtherGames::usage = "程序包的说明,这里抄一遍";
Begin["`Private`"];
(* ::Subsection::Closed:: *)
(*主体代码*)



(* ::Subsubsection:: *)
(*Todo: 整合猜拳程序块*)
RPSQ[k_?OddQ,{x_,n_}]:=If[x===n,{0,0},If[Mod[n+1,k]<=x<=Mod[n+(k-1)/2,k],{-1,1},{1,-1}]];
RPSQ[k_?EvenQ,{x_,n_}]:=If[x===n||x===Mod[n+k/2,k],{0,0},If[Mod[n+1,k]<=x<=Mod[n+k/2-1,k],{-1,1},{1,-1}]];
MatrixRPS[2]:="请输入大于2的整数";
MatrixRPS[n_?OddQ]:=MapIndexed[RotateRight,Table[Join[ConstantArray[1,(n-1)/2],ConstantArray[0,(n+1)/2]],{n}]];
MatrixRPS[n_?EvenQ]:=MapIndexed[RotateRight,Table[Join[ConstantArray[1,n/2-1],ConstantArray[0,n/2+1]],{n}]];
MatrixRPS[n_]:=MatrixRPS[2];
GraphRPS[2]:=MatrixRPS[2];
GraphRPS[3,"预设"]:=Graph[{"石头","剪刀","布"},{SparseArray[Automatic,{3,3},0,{1,{{0,1,2,3},{{2},{3},{1}}},Pattern}],Null},
  VertexLabels->Placed["Name",Center],
  VertexLabelStyle->Directive[24,Lighter@Blue,Bold],
  VertexSize->0.3,EdgeShapeFunction->GraphElementData["CarvedArrow","ArrowSize"->0.1],
  GraphLayout->"CircularEmbedding"];
GraphRPS[5,"预设"]:=Graph[{"石头","剪刀","蜥蜴","布","史波克"},
  {SparseArray[Automatic,{5,5},0,{1,{{0,2,4,6,8,10},{{2},{3},{3},{4},{4},{5},{1},{5},{1},{2}}},Pattern}],Null},
  VertexLabels->Placed["Name",Center],VertexLabelStyle->Directive[24,Lighter@Blue,Bold],
  VertexSize->0.3,EdgeShapeFunction->GraphElementData["CarvedArrow","ArrowSize"->0.06],
  GraphLayout->"CircularEmbedding"];
GraphRPS[7,"预设"]:=Graph[{"天枢","天璇","天玑","天权","玉衡","开阳","瑶光"},{SparseArray[Automatic,{7,7},
  0,{1,{{0,3,6,9,12,15,18,21},{{2},{3},{4},{3},{4},{5},{4},{5},{6},{5},{6},{7},{1},{6},{7},{1},{2},{7},{1},{2},{3}}},Pattern}],Null},
  VertexLabels->Placed["Name",Center],
  VertexLabelStyle->Directive[24,Lighter@Blue,Bold],
  VertexSize->0.3,EdgeShapeFunction->GraphElementData["CarvedArrow","ArrowSize"->0.04],
  GraphLayout->"CircularEmbedding",ImageSize->Large];
GraphRPS[n_?IntegerQ,size_:18]:=AdjacencyGraph[MatrixRPS[n],
  VertexLabels->Placed["Index",Center],
  VertexLabelStyle->Directive[size,Lighter@Blue,Bold],
  VertexSize->0.3,EdgeShapeFunction->GraphElementData["CarvedArrow","ArrowSize"->0.03],
  GraphLayout->"CircularEmbedding",ImageSize->Large];
GraphRPS[n_]:=MatrixRPS[2];
Needs["ExData`"];
GameRPS=DynamicBlock[{history={},hLast=0,cLast=0,hScore=0,
  cScore=0,message=""},Panel@Column[{Dynamic@Grid[{{Column[MapIndexed[Button[#,cLast=chooseGo2[history];
hLast=#2[[1]];
message=Switch[winTest[hLast,cLast],"Win",hScore++;"Youwin","Lose",cScore++;"Youlose",_,"Draw"];
AppendTo[history,{hLast,cLast}];]&,{"Rock","Paper","Scissors"}]],displayPlay[hLast,"You"],Style[message,20],
  displayPlay[cLast,Style["Mathematica",Italic]]},
  {PieChart[{hScore,cScore},ChartLegends->SwatchLegend[{Row[{"You:",hScore}],Row[{Style["Mathematica:",Italic],cScore}]}],
    ChartStyle->{Darker@Green,Darker@Red},ImageSize->110],SpanFromLeft,
    ListLinePlot[Prepend[MeanFilter[(winTest@@@history)/.{"Win"->1,"Lose"->-1,"Draw"->0},
      Ceiling[Length[history]/5]],0],ImageSize->130,
      PlotLabel->"Winrate",AxesOrigin->{0,0},
      PlotRange->{-0.5,0.5}],SpanFromLeft}},ItemSize->{{{Scaled[0.24]}},Automatic},Background->White,Alignment->Center],
  Item[Row[{"",Button["Resethistory",history={};hScore=0;cScore=0;
  message="";cLast=0;hLast=0;]}],Alignment->Right]}],
  Initialization:>(displayPlay[play_,name_]:=Column[{Text[Style[name,18]],
    Show[If[name === "You", ImageReflect[#, Left], #] &@Switch[play,
      1, ExData`Private`石头,
      2, ExData`Private`剪刀,
      3, ExData`Private`布,
      _, Graphics[{}]],ImageSize->{109,70}]},Alignment->Center];
  winTest[p1_,p2_]:=Switch[Mod[p1-p2,3],
    0,"Draw",
    1,"Win",
    2,"Lose"];
  historySeek[history_,n_Integer,col_]:=If[n>Length[history]-1,{},If[#==={},{},#[[1,All,1]]]&
  [Reap[Do[If[history[[i;;(i+n-1),col]]===history[[-n;;,col]],Sow[history[[i+n]]]],
      {i,Length[history]-n}]][[2]]]];prediction[pastChoices_List]:={If[Length[pastChoices]<2,1,
    DistributionFitTest[pastChoices,DiscreteUniformDistribution[{1,3}]]],
    RandomChoice[Commonest[pastChoices/.{}->{1,2,3}]]};
  bestGuess[{}]:=RandomInteger[{1,3}];
  bestGuess[data_]:=Block[{max=Length[data]},Sort[Flatten[Outer[prediction[historySeek[data,#1,#2]]&,Range[max],{1,2,All}],{1,2}]]][[1,-1]];
  chooseGo2[data_]:=Mod[bestGuess[data]+1,3,1];)];



(* ::Subsubsection:: *)
(*Todo: 完成数独程序块*)
SetAttributes[{SudokuFX3,SudokuExc},HoldAll];
(*本程序包数独的标准格式是一个9×9的矩阵,里面只能填0到9,0表示待解*)
(*Coded by Mr. Wizard's
Thanks for Morphie's commit,it's helpful
but it's still very hard to understand.*)
(*SudokuLinked returns a list of the values at all locations that constrain the passed location,Delete is used*)
(*to ensure that the value of the passed location is not included in the list*)
SudokuLinked[m_][u_,v_,r_,c_]:={m[[u,All,r]]~Delete~{v,c},m[[All,v,All,c]]~Delete~{u,r},m[[u,v]]~Delete~{r,c}};
(*SudokuPof2 is 1|2|4|8|...|256*)
SudokuPof2=Alternatives@@(2^Range[0,8]);
SudokuFX1=BitOr@@Cases[#,SudokuPof2,-1]&;
SudokuFX2=BitOr[#,#2]-#&;
(*SudokuFX1 is used to find all the values that are already claimed by SudokuLinked locations*)
(*SudokuFX2 then used to subtract out those cases from the unsolved locations*)
(*note HoldAll attr above:SudokuFX3 and SudokuExc are the only two functions which directly modify the board*)
SudokuFX3[m_]:=(m[[##]]=SudokuFX2[SudokuFX1@SudokuLinked[m][##],m[[##]]])&;
SudokuExc[m_][c__]:=If[#!={},{m[[c]]}=#,""]&@Cases[SudokuFX2[BitOr@@#,m[[c]]]&/@Flatten/@SudokuLinked[m]@c,Except[0],1,1];
(*between them SudokuLoop and SudokuSplit combine to solve the sudoku puzzle*)
(*each puzzle location is used as a bitvector to show possibles,i.e.511 means all are possible,*)
(*256 means only 8 is possible,1 means only 0 is possible (we're using 0-8,instead of 1-9)*)
(*FixedPoint applies SudokuFX3 and SudokuExc repeatedly to all locations that are not yet solved (=to a SudokuPof2) "good" is*)
(*a list of these positions,m (the modified board) is returned*)
SudokuLoop[board_]:=Block[{m=board},(FixedPoint[Block[{good=Position[m,Except[SudokuPof2,_Integer],{-1}]},SudokuFX3[m]@@@good;SudokuExc[m]@@@good]&,{}];m)];
(*returns the position of the element {w,x,y,z} that is most nearly a power of 2,for example 48 has only*)
(*two possibles:4 or 5 (48=2^4+2^5)*)
SudokuNear=Position[#,Min@#,-1,1][[1]]&[Map[#~Count~1&,IntegerDigits[#,2,9],{-2}]/.1->10]&;
(*bad puzzle boards (those with a 0=no possibles) are replaced with an empty sequence,which effectively*)
(*backtracks,going on to the next ReplacePart substitution (see below),solved boards are Throw'n,*)
(*and the rest are recursively solved.Solving involves working on locations that are nearly solved,*)
(*ReplacePart replaces those locations with all possible values,lowest to highest,for example*)
(*48 (possibles=4 or 5) would be replaced by 16 (2^4),then 32 (2^5)*)
SudokuSplit[board_]/;MemberQ[board,0,{-1}]:=Sequence[];
SudokuSplit[board_]/;MatchQ[Flatten@board,{SudokuPof2..}]:=Throw[board];
SudokuSplit[board_]:=With[{c=SudokuNear@board},SudokuSplit@SudokuLoop@ReplacePart[board,c->#]&/@First/@(2^(Reverse@IntegerDigits[board~Extract~c,2]~Position~1-1))];
(*0's become 511,everything else becomes 2^(n-1) and the puzzles are partitioned into blocks of 3*)
SudokuPrep=2^(#~Partition~{3,3}-1)/.1/2->511&;
(*SudokuSolverFast is the main solving function,upon completion we have a list of lists of the digits to be summed*)
(*the base 2 Log and "+1" are there because all work is done with numbers 2^(n-1)*)
(*"Catch" should get the complete boards Throw'n in SudokuSplit above*)
SudokuSolverFast=2~Log~Catch[SudokuSplit@SudokuLoop@SudokuPrep@#]+1&;

ShowMarking[marking_]:=Block[{},Graphics[
  Table[{EdgeForm[Thin],If[EvenQ[Floor[(j-1)/3]+Floor[(i-1)/3]*3],
    Lighter[Gray,0.5],White],Rectangle[{i,j},{i+1,j+1}],Black,
    If[KeyExistsQ[marking,{i,10-j}],
      Text[Style[marking[{i,10-j}],Large],{i+0.5,j+0.5}]]},{i,1,9},{j,1,9}]]];
initialHard=<|{1,6}->6,{1,8}->5,{2,1}->2,{2,3}->7,{2,5}->8,{3,3}->4,
  {4,2}->6,{4,6}->5,{5,3}->8,{5,5}->4,{5,7}->1,{6,4}->3,{6,8}->9,
  {7,7}->7,{8,5}->1,{8,7}->8,{8,9}->4,{9,2}->3,{9,4}->2|>;
ShowMarking[initialHard](*需要写一个转换器*)



(* ::Subsubsection:: *)
(*Todo: 完成扫雷程序块*)
MineLayout[{m_,n_,k_}]:=Block[{M,foo,bar,list},
  M=ConstantArray[0,{m+2,n+2}];
  foo[{x_,y_}]:=M[[x-1;;x+1,y-1;;y+1]]+=1;
  bar[{x_,y_}]:=M[[x,y]]=10;
  list=RandomSample[Tuples[{Range[2,m+1],Range[2,n+1]}]][[1;;k]];
  Do[foo@list[[i]],{i,k}];bar/@list;M[[2;;-2,2;;-2]]];
MineDistribution[m_,n_,k_,p_]:=Transpose@{Range[0,10],BinCounts[Flatten[MineLayout/@ConstantArray[{m,n,k},p]],{-0.5,10.5,1}]/p+0.0};

(* ::Subsubsection:: *)
(*Todo: 完成点灯程序块*)
Begin["`LightOut`"]
(*已废弃,速度太慢,可读性太差
var[i_,j_]:=x[i,j];
variable[i_,j_,w_,h_]:=If[i>=1&&i<=w&&j>=1&&j<=h,var[i,j],0];
GetSquareValue[i_,j_,w_,h_,gs_]:=If[i>=1&&i<=w&&j>=1&&j<=h,gs[[i,j]],0];
GetShowValue[i_,j_,w_,h_,gs_]:=GetSquareValue[i,j,w,h,gs]+GetSquareValue[i,j+1,w,h,gs]+GetSquareValue[i,j-1,w,h,gs]+GetSquareValue[i-1,j,w,h,gs]+GetSquareValue[i+1,j,w,h,gs];
AllVariables[w_,h_]:=Flatten[Table[var[i,j],{i,1,w},{j,1,h}]];
OnSolver[M_?MatrixQ]:=Block[{w,h,OnEquation,OnSol},
	{w,h}=Length/@{M,Transpose@M};
	OnEquation[i_,j_,w_,h_,gs_]:=GetShowValue[i,j,w,h,gs]==1+variable[i,j,w,h]+variable[i-1,j,w,h]+ variable[i,j-1,w,h]+variable[i+1,j,w,h]+variable[i,j+1,w,h];
	OnSol=Quiet[Solve[Flatten[Table[OnEquation[i,j,w,h,M],{i,1,w},{j,1,h}]],AllVariables[w,h],Modulus->2]];
	Partition[Flatten[OnSol/.(x[a_,b_]->c_)->c],w]/.C[1]->0/.C[2]->0];
OffSolver[M_?MatrixQ]:=Block[{w,h,OffEquation,OffSol},
	{w,h}=Length/@{M,Transpose@M};
	OffEquation[i_,j_,w_,h_,gs_]:=GetShowValue[i,j,w,h,gs]==variable[i,j,w,h]+variable[i-1,j,w,h]+variable[i,j-1,w,h]+variable[i+1,j,w,h]+variable[i,j+1,w,h];
	OffSol=Quiet[Solve[Flatten[Table[OffEquation[i,j,w,h,M],{i,1,w},{j,1,h}]],AllVariables[w,h],Modulus->2]];
	Partition[Flatten[OffSol/.(x[a_,b_]->c_)->c],w]/.C[1]->0/.C[2]->0];*)

M=AdjacencyMatrix@GridGraph[{n,n}]+SparseArray[{Band[{1,1}]->1},{n^2,n^2}]
ArrayPlot@Partition[LinearSolve[M,ConstantArray[1,n^2],Modulus->2],n]
res=RowReduce[Transpose@Join[Transpose@M,{ConstantArray[1,n^2]}],Modulus->2];
ArrayPlot@Partition[res[[All,-1]],n];
End[];

(* ::Subsection::Closed:: *)
(*附加设置*)
End[] ;

EndPackage[];