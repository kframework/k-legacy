#include <stdlib.h>
#include <stdio.h>

struct listNode {
  int val;
  struct listNode *next;
};

struct listNode* swap(struct listNode* x)
//@ rule <k> $ => return ?x; </k> <heap_> list(x)([v1] @ [v2] @ A) => list(?x)([v2] @ [v1] @ A) <_/heap> 
{
  struct listNode* p;
  p = x;
  x = x->next;
  p->next = x->next;
  x->next = p;
  return x;
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


/*@ verify */
int main()
{
  struct listNode *x;
  struct listNode *y;
  x = create(5);
  print(x);
  x->next = x->next;
  x->next->next = x->next->next;
  x = swap(x);
  print(x);
  
  return 0;
}

//@ var A : Seq
//@ var v : Int
