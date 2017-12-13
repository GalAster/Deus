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
(*Creation Date:2016-11-16*)
(*Copyright:CC4.0 BY+NA+NC*)
(**)
(*该软件包遵从CC协议:署名、非商业性使用、相同方式共享*)
(**)
(*这里应该填这个函数的介绍*)
(* ::Section:: *)
(*函数说明*)
BeginPackage["WordGames`"];
WordSquare::usage = "";
WordCube::usage = "";
BoggleSolver::usage = "";
(* ::Section:: *)
(*程序包正体*)
(* ::Subsection::Closed:: *)
(*主设置*)
WordGames$Version="V0.1";
WordGames$Environment="V11.0+";
WordGames$LastUpdate="2016-11-16";
WordGames::usage = "程序包的说明,这里抄一遍";
Begin["`Private`"];
(* ::Subsection::Closed:: *)
(*主体代码*)



(* ::Subsubsection:: *)
(*词方*)
Options[WordSquare]={Language->"English",Dimensions->2};
WordSquare[len_,pattern_:All,OptionsPattern[]]:=Block[{alln,goal},
  alln=Characters/@Select[DictionaryLookup[{OptionValue[Language],All}],StringLength[#1]==len&];
  nmatch[patt_]:=nmatch[Verbatim[patt]]=Length[Cases[alln,patt]];
  findCompletions[m_]:=Block[{nn,ur},
    {ur,nn}=NestWhile[{fixone[#1[[1]],First[#1[[2]]]],Rest[#1[[2]]]}&,
      {m,Ordering[nmatch/@m]},Length[#1[[2]]]>0&&nmatch[#1[[1,#1[[2,1]]]]]==1&];If[Length[nn]==0,{ur},With[{n=First[nn]},
      (ReplacePart[ur,n->#1]&)/@Cases[alln,ur[[n]]]]]];
  findCompletionsOriented[m_]:=Block[{osc},
    osc=findCompletions/@Union[{m,Transpose[m]}];
    osc[[First[Ordering[Length/@osc,1]]]]];
  fixone[ml_,nl_]:=If[FreeQ[ml[[nl]],Verbatim[_]],ml,ReplacePart[ml,nl->First[Cases[alln,ml[[nl]]]]]];
  goal=If[pattern===All,{Nest[ConstantArray[#1,len]&,_,OptionValue[Dimensions]]},pattern];
  FixedPoint[Union[Join@@findCompletionsOriented/@#1]&,goal]];



(* ::Subsubsection:: *)
(*词立方*)
Options[WordCube]={Random->True,Language->"English",Dimensions->3};
WordCube[len_Integer,op:OptionsPattern[]]:=WordCube[ConstantArray[_,ConstantArray[len,OptionValue[Dimensions]]],op];
WordCube[mat_,OptionsPattern[]]:=Block[{$random=If[OptionValue[Random],RandomSample,Identity],
  $RecursionLimit=Infinity,$IterationLimit=Infinity,dictCache},Catch[inspect[mat]=!=Null,tag]];
dict[numLetters_]:=Replace[dictCache[numLetters],_dictCache:>
    (dictCache[numLetters]=DictionaryLookup[{OptionValue[Language],Repeated[_,{numLetters}]}])];
inspect[mat_]:=inspect[mat,nextIndices[mat]];
nextIndices[mat_]/;FreeQ[mat,Verbatim[_]]:=True;
nextIndices[mat_]:=nextIndices[mat,indices[mat]];
i:indices[wordLen_,dims_]:=i=(Permutations[Append[#1,All]]&)/@DeleteDuplicates[Sort/@Tuples[Range[wordLen],{dims-1}]];
indices[mat_]:=indices[Length[mat],Length[Dimensions[mat]]];
nextIndices[mat_,indices_]:=nextIndices[mat,indices,1+LengthWhile[
  indices[[All,1]],FreeQ[mat[[Sequence@@#1]],Verbatim[_]]&]];
nextIndices[mat_,indices_,nextIndex_]:=Extract[indices,nextIndex];
inspect[mat_,True]:=Throw[mat,tag];
inspect[mat_,indices_]:=Scan[Function[word,inspect[change[mat,word,indices]]],
  (Characters[$random[Pick[#1,StringMatchQ[#1,StringExpression@@mat[[Sequence@@First[indices]]]]]]]&)[dict[Length[mat]]]];
change[mat_,word_,indices_]:=Block[{newMat=mat},Scan[Function[wordPos,newMat[[Sequence@@wordPos]]=word],indices];newMat];



(* ::Subsubsection:: *)
(*Boggle*)
makeTree[wrds:{__String}]:=makeTree[Characters[wrds]];
makeTree[wrds_/;MemberQ[wrds,{}]]:=Prepend[makeTree[DeleteCases[wrds,{}]],{}->{}];
makeTree[wrds_]:=Reap[(If[#1=!={},Sow[Rest[#1],First[#1]]]&)/@wrds,_,#1->makeTree[#2]&][[2]];
getLetterAndAdjacencyRules[(letterMatrix_)?(MatrixQ[#1,StringQ]&)]:=
    Block[{a,lrules,p,adjRules},lrules=(Thread[Range[Length[#1]]->#1]&)[
      Flatten[letterMatrix]];p=ArrayPad[Partition[Array[a,Length[lrules]],
      Last[Dimensions[letterMatrix]]],1];
    adjRules=Flatten[ListConvolve[{{1,1,1},{1,2,1},{1,1,1}},p]/.Plus->List/.
        {left___,2*(v_),right___}:>{v->{left,right}}/.a[x_]:>x];
    Dispatch/@{lrules,adjRules}];
getVertexSequences[adjrules_,letterRules_,allTree_,n_]:=
    Block[{subF,f,getWordsForStartingVertex},
      subF[v_,tree_]:=With[{letter=v/.letterRules},With[{res=letter/.tree},
        res/;res=!=letter]];subF[_,_]:={};
      f[vvlist_,{{}->{},rest___}]:=f[Sow[vvlist],{rest}];f[_,{}]:=Null;
      f[vvlist:{last_,prev_List},subTree_]:=
          Scan[f[{#1,vvlist},subF[#1,subTree]]&,Complement[last/.adjrules,Flatten[vvlist]]];
      getWordsForStartingVertex[v_]:=(If[#1==={},#1,Reverse[Flatten/@First[#1],2]]&)[
        Reap[f[{v,{}},subF[v,allTree]]][[2]]];Flatten[getWordsForStartingVertex/@Range[n],1]];
wordsFromVertexSequences[vseqs_List,letterRules_]:=StringJoin/@(vseqs/.letterRules);
GetWordTree[minLen_Integer:1,maxLen:_Integer|Infinity:Infinity]:=
    makeTree[Select[ToLowerCase[DictionaryLookup["*"]],minLen<=StringLength[#1]<=maxLen&]];
BoggleSolver[board_String,wordTree_]:=BoggleSolver[ToLowerCase[ImportString[board]],wordTree];
BoggleSolver[lboard_,wordTree_]:=Block[{lrules,adjrules},
  {lrules,adjrules}=getLetterAndAdjacencyRules[lboard];
  wordsFromVertexSequences[getVertexSequences[adjrules,lrules,wordTree,Times@@Dimensions[lboard]],lrules]];


(* ::Subsection::Closed:: *)
(*附加设置*)
End[] ;

EndPackage[];