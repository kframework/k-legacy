#include <stdlib.h>
#include <stdio.h>


struct listNode {
  int val;
  struct listNode *next;
};


struct listNode *copy(struct listNode *x)
// rule <k> = return ?y; </k> <heap_> list(x)(A) => list(x)(A), list(?y)(A) <_/heap>
{
  struct listNode* y;
  struct listNode* ix;

  if (x == 0)
    return 0;
	else {
		y = (struct listNode *)malloc(sizeof(struct listNode));
		y->val = x->val;
		y->next = copy(x->next);
	}

  return y;
}

struct listNode* create(int n)
{
	struct listNode *x;
	struct listNode *y;
	x = 0;
	while (n)
	{
		y = x;
		x = (struct listNode*)malloc(sizeof(struct listNode));
		x->val = n;
		x->next = y;
		n -= 1;
	}
	return x;
}

void destroy(struct listNode* x)
//@ rule <k> $ => return; </k><heap_> list(x)(A) => . <_/heap>
{
	//@ inv <heap_> list(x)(?A) <_/heap>
	while(x)
	{
		struct listNode *y;
		
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
	/*@ inv <heap_> lseg(old(x), x)(?A1), list(x)(?A2) <_/heap> <out_> ?A1 </out>
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
  struct listNode *y;

  x = create(5);
  print(x);
  y = copy(x);
  print(y);
  destroy(x);
  destroy(y);
  return 0;
}


//@ var n : Int
//@ var A, B, C : Seq
//@ var H : MapItem
