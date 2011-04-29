#include <stdlib.h>
#include <stdio.h>


struct listNode {
  int val;
  struct listNode *next;
};

int length(struct listNode* x)
//@ rule <k> $ => return len(A); </k> <heap_> list(x)(A) <_/heap>
{
	int l;
	
	l = 0;
	/*@ inv <heap_> lseg(old(x), x)(?A1), list(x)(?A2) <_/heap> 
	 /\ A = ?A1 @ ?A2 /\ l = len(?A1) */
	while (x) {
		l += 1;
		x = x->next ;
	}
	
	return l;
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
  int n;

  x = create(5);
  //@ assert <heap_> list(x)([1, 2, 3, 4, 5]) <_/heap>
  n = length(x);
  //@ assert <heap_> list(x)([1, 2, 3, 4, 5]) <_/heap>
  destroy(x);
  //@ assert <heap_> . <_/heap>
  return 0;
}


//@ var n : Int
//@ var A, B, C : Seq

