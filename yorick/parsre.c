
/* ------------------------------------------------------------------------ */

/* this is a clumsy hack to make the parser reentrant
 * -- do it better next time */

/* returns current parser state, sets new state (or zeroes)
 * eventually, must swap back and call YpParseInit */
extern void *yp_swap_hi_state(void *new_state);

struct yp_hi_state_t {
  int ypErrors;
  HashTable literalTable;
  long *literalTypes;
  Symbol *constantTable;
  long maxConstants;
  long nConstants;
  Instruction *vmCode;
  long vmCodeSize;
  long nextPC;
  VMaction *previousOp;
  int wasUndecided;
  long *variableRefs, *constantRefs, *gotoTargets;
  long maxVariableRefs, nVariableRefs;
  long maxConstantRefs, nConstantRefs;
  long maxGotoTargets, nGotoTargets;
  int nPos, nKey;
  long hasPosList;
  int nLocal, nTarget;
  int stackDepth, maxStackDepth, didMaxDepth;
  Instruction *incCode;
  long incCodeSize;
  long nextInc;
  int loopDepth;
  BreakStack *breakStack;
  long breakStackSize;
  long nextBSP;
  MatrixMarker *matrixMarkers;
  int nMatrixMarkers, maxMatrixMarkers;
  HashTable quineTable;
  int *nQuinedArgs;
  int insideFunc;
  Function *reparsing;
  long *ypReList, nYpReList;
  int ypReMatch;
  Literal currentDataType;
};

void *
yp_swap_hi_state(void *new_state)
{
  if (!p_signalling) {
    struct yp_hi_state *to= new_state;
    struct yp_hi_state *from= 0;

    if (literalTable.maxItems || literalTypes || constantTable ||
        nextPC || stackDepth) {
      from= p_malloc(sizeof(struct yp_hi_state));
      from->ypErrors= ypErrors;
      from->literalTable= literalTable;
      from->literalTypes= literalTypes;
      from->constantTable= constantTable;
      from->maxConstants= maxConstants;
      from->nConstants= nConstants;
      from->vmCode= vmCode;
      from->vmCodeSize= vmCodeSize;
      from->nextPC= nextPC;
      from->previousOp= previousOp;
      from->wasUndecided= wasUndecided;
      from->variableRefs= variableRefs;
      from->maxVariableRefs= maxVariableRefs;
      from->nVariableRefs= nVariableRefs;
      from->constantRefs= constantRefs;
      from->maxConstantRefs= maxConstantRefs;
      from->nConstantRefs= nConstantRefs;
      from->gotoTargets= gotoTargets;
      from->maxGotoTargets= maxGotoTargets;
      from->nGotoTargets= nGotoTargets;
      from->nPos= nPos;  from->nKey= nKey;
      from->hasPosList= hasPosList;
      from->nLocal= nLocal;  from->nTarget= nTarget;
      from->stackDepth= stackDepth;  from->maxStackDepth= maxStackDepth;
      from->didMaxDepth= didMaxDepth;
      from->incCode= incCode;
      from->incCodeSize= incCodeSize;  from->nextInc= nextInc;
      from->loopDepth= loopDepth;
      from->breakStack= breakStack;
      from->breakStackSize= breakStackSize;  from->nextBSP= nextBSP;
      from->matrixMarkers= matrixMarkers;
      from->nMatrixMarkers= nMatrixMarkers;
      from->maxMatrixMarkers= maxMatrixMarkers;
      from->quineTable= quineTable;
      from->nQuinedArgs= nQuinedArgs;
      from->insideFunc= insideFunc;
      from->reparsing= reparsing;
      from->ypReList= ypReList;
      from->nYpReList= nYpReList;
      from->ypReMatch= ypReMatch;
      from->currentDataType= currentDataType;
    }

    if (to) {
      ypErrors= to->ypErrors;
      literalTable= to->literalTable;
      literalTypes= to->literalTypes;
      constantTable= to->constantTable;
      maxConstants= to->maxConstants;
      nConstants= to->nConstants;
      vmCode= to->vmCode;
      vmCodeSize= to->vmCodeSize;
      nextPC= to->nextPC;
      previousOp= to->previousOp;
      wasUndecided= to->wasUndecided;
      variableRefs= to->variableRefs;
      maxVariableRefs= to->maxVariableRefs;
      nVariableRefs= to->nVariableRefs;
      constantRefs= to->constantRefs;
      maxConstantRefs= to->maxConstantRefs;
      nConstantRefs= to->nConstantRefs;
      gotoTargets= to->gotoTargets;
      maxGotoTargets= to->maxGotoTargets;
      nGotoTargets= to->nGotoTargets;
      nPos= to->nPos;  nKey= to->nKey;
      hasPosList= to->hasPosList;
      nLocal= to->nLocal;  nTarget= to->nTarget;
      stackDepth= to->stackDepth;  maxStackDepth= to->maxStackDepth;
      didMaxDepth= to->didMaxDepth;
      incCode= to->incCode;
      incCodeSize= to->incCodeSize;  nextInc= to->nextInc;
      loopDepth= to->loopDepth;
      breakStack= to->breakStack;
      breakStackSize= to->breakStackSize;  nextBSP= to->nextBSP;
      matrixMarkers= to->matrixMarkers;
      nMatrixMarkers= to->nMatrixMarkers;
      maxMatrixMarkers= to->maxMatrixMarkers;
      quineTable= to->quineTable;
      nQuinedArgs= to->nQuinedArgs;
      insideFunc= to->insideFunc;
      reparsing= to->reparsing;
      ypReList= to->ypReList;
      nYpReList= to->nYpReList;
      ypReMatch= to->ypReMatch;
      currentDataType= to->currentDataType;
    } else {
      ypErrors= 0;
      literalTable.nItems= literalTable.maxItems= literalTable.nSlots= 0;
      literalTable.names= 0;
      literalTable.items= 0;
      literalTypes= 0;
      constantTable= 0;
      maxConstants= nConstants= 0;
      vmCode= 0;
      vmCodeSize= nextPC= 0;
      previousOp= 0;
      wasUndecided= 0;
      variableRefs= 0;
      maxVariableRefs= nVariableRefs= 0;
      constantRefs= 0;
      maxConstantRefs= nConstantRefs= 0;
      gotoTargets= 0;
      maxGotoTargets= nGotoTargets= 0;
      nPos= nKey= 0;
      hasPosList= 0;
      nLocal= nTarget= 0;
      stackDepth= maxStackDepth= didMaxDepth= 0;
      incCode= 0;
      incCodeSize= nextInc= 0;
      loopDepth= 0;
      breakStack= 0;
      breakStackSize= nextBSP= 0;
      matrixMarkers= 0;
      nMatrixMarkers= maxMatrixMarkers= 0;
      quineTable.nItems= quineTable.maxItems= quineTable.nSlots= 0;
      quineTable.names= 0;
      quineTable.items= 0;
      nQuinedArgs= 0;
      insideFunc= 0;
      reparsing= 0;
      ypReList= 0;
      nYpReList= 0;
      ypReMatch= 0;
      currentDataType= 0;
    }
    return from;
  } else {
    p_abort();
    return 0;
  }
}


/*--------------------------------------------------------------------------*/

struct yp_lo_state {
  int yp_keybd_input, yp_prev_state, yp_did_prompt;
  char *curLine, *nextChar;
  int curChar;
  char *prevToken;
  int prevWasSemi, prevWasComma, noInputYet;
  char *quote, *quineBegin, *quineEnd, *quineText;
  int nOpenCS, parenDepth, braceDepth, needOperand, supressSemi;
  int needBody, keyCount, nQuines, quining;
  int yynerrs, yyerrflag, yychar;
  short *yyssp, *yyss;
  YYSTYPE *yyvsp, *yyvs, yyval, yylval;
  long yystacksize;
  int yydebug;
};

void *
yp_swap_lo_state(void *new_state)
{
  struct yp_lo_state *to= new_state;
  struct yp_lo_state *from= 0;
  if (curLine) {
    from= p_malloc(sizeof(struct yp_lo_state));
    from->yp_keybd_input= yp_keybd_input;
    from->yp_prev_state= yp_prev_state;
    from->yp_did_prompt= yp_did_prompt;
    from->curLine= curLine;
    from->nextChar= nextChar;
    from->curChar= curChar;
    from->prevToken= prevToken;
    from->prevWasSemi= prevWasSemi;
    from->prevWasComma= prevWasComma;
    from->noInputYet= noInputYet;
    from->quote= quote;
    from->quineBegin= quineBegin;
    from->quineEnd= quineEnd;
    from->quineText= quineText;
    from->nOpenCS= nOpenCS;
    from->parenDepth= parenDepth;
    from->braceDepth= braceDepth;
    from->needOperand= needOperand;
    from->supressSemi= supressSemi;
    from->needBody= needBody;
    from->keyCount= keyCount;
    from->nQuines= nQuines;
    from->quining= quining;
    from->yynerrs= yynerrs;
    from->yyerrflag= yyerrflag;
    from->yychar= yychar;
    from->yyssp= yyssp;
    from->yyss= yyss;
    from->yyvsp= yyvsp;
    from->yyvs= yyvs;
    from->yyval= yyval;
    from->yylval= yylval;
    from->yystacksize= yystacksize;
    from->yydebug= yydebug;
  }
  if (to) {
    yp_keybd_input= to->yp_keybd_input;
    yp_prev_state= to->yp_prev_state;
    yp_did_prompt= to->yp_did_prompt;
    curLine= to->curLine;
    nextChar= to->nextChar;
    curChar= to->curChar;
    prevToken= to->prevToken;
    prevWasSemi= to->prevWasSemi;
    prevWasComma= to->prevWasComma;
    noInputYet= to->noInputYet;
    quote= to->quote;
    quineBegin= to->quineBegin;
    quineEnd= to->quineEnd;
    quineText= to->quineText;
    nOpenCS= to->nOpenCS;
    parenDepth= to->parenDepth;
    braceDepth= to->braceDepth;
    needOperand= to->needOperand;
    supressSemi= to->supressSemi;
    needBody= to->needBody;
    keyCount= to->keyCount;
    nQuines= to->nQuines;
    quining= to->quining;
    yynerrs= to->yynerrs;
    yyerrflag= to->yyerrflag;
    yychar= to->yychar;
    yyssp= to->yyssp;
    yyss= to->yyss;
    yyvsp= to->yyvsp;
    yyvs= to->yyvs;
    yyval= to->yyval;
    yylval= to->yylval;
    yystacksize= to->yystacksize;
    yydebug= to->yydebug;
  } else {
    yp_keybd_input= yp_prev_state= 0;
    yp_did_prompt= 1;
    curLine= nextChar= 0;
    curChar= 0;
    prevToken= 0;
    prevWasSemi= prevWasComma= noInputYet= 0;
    quote= quineBegin= quineEnd= quineText= 0;
    nOpenCS= parenDepth= braceDepth= needOperand= supressSemi= 0;
    needBody= keyCount= nQuines= quining= 0;
    yynerrs= yyerrflag= yychar= 0;
    yyssp= yyss= 0;
    yyvsp= yyvs= 0;
    yyval= yylval= 0;
    yystacksize= 0;
    yydebug= 0;
  }
  return from;
}
