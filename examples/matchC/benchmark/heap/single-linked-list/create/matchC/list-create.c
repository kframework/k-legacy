#include <stdlib.h>
#include <stdio.h>

struct listNode {
  int val;
  struct listNode *next;
};

struct listNode* create()
//@ rule <k> $ => return ?x; </k> <heap_> . => list(?x)([5, 6, 7]) <_/heap>
{
  struct listNode *x;
  struct listNode *y;
  x = (struct listNode*)malloc(sizeof(struct listNode));
  x->val = 7;
  x->next = 0;
  y = (struct listNode*)malloc(sizeof(struct listNode));
  y->val = 6;
  y->next = x;
  x = y;
  y = (struct listNode*)malloc(sizeof(struct listNode));
  y->val = 5;
  y->next = x;
  x = y;
  return x;
}

void destroy(struct listNode* x)
//@ rule <k> $ => return; </k><heap_> list(x)(A) => . <_/heap>
{
	struct listNode *y;
	
	//@ inv <heap_> list(x)(?A) <_/heap>
	while(x)
	{
		y = x->next;
		free(x);
		x = y;
	}
}


void print(struct listNode* x)
/*@ rule <k> $ => return; </k>
 <heap_> list(x)(A) <_/heap>
 <out_> epsilon => A </out> */
{
	/*@ inv <heap_> lseg(old(x),x)(?A1), list(x)(?A2) <_/heap> <out_> ?A1 </out>
	 /\ A = ?A1 @ ?A2 */
	while(x)
	{
		printf("%d ",x->val);
		x = x->next;
	}
	printf("\n"); 
}

int main()
{
  struct listNode *x;
  x = create();
  print(x);
  destroy(x);
  return 0;
}


//@ var A : Seq
