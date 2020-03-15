/*
generated at Sun Mar 15 16:47:22 2020
by $Id: pburg.c,v 2.5 2017/11/16 09:41:42 prs Exp $
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define PBURG_PREFIX "yy"
#define PBURG_VERSION "2.5"
#define MAX_COST 0x7fff
#if defined(__STDC__) || defined(__cplusplus)
#define YYCONST const
#else
#define YYCONST
#endif

#line 1 "code.brg"

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include "node.h"
#include "tabid.h"
#include "postfix.h"

static int sameVar(NODEPTR_TYPE p) {
  return strcmp(LEFT_CHILD(p)->value.s, LEFT_CHILD(RIGHT_CHILD(p))->value.s) ? 0x7fff : 2;
}

#define TRACE
static void yytrace(NODEPTR_TYPE p, int eruleno, int cost, int bestcost);

extern FILE *yyout;
extern int lbl;
extern char *mklbl(int n);
