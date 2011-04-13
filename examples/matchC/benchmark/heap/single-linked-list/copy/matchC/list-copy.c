#include <stdlib.h>
#include <stdio.h>


struct listNode {
  int val;
  struct listNode *next;
};


struct listNode *copy(struct listNode *x)
//@ rule <k> $ => return ?y ; </k> <heap_> list(x)(A) => list(x)(A), list(?y)(A) <_/heap>

{
  struct listNode* y;
  struct listNode* ix;
  struct listNode* iy;

  if (x == 0) return 0;
  else 
  {
	ix = x->next;
	if (ix) 
	{
		y = (struct listNode *)malloc(sizeof(struct listNode));
		y->val = x->val;
		iy = (struct listNode *)malloc(sizeof(struct listNode));
		iy->val = ix->val;
		y->next = iy;
		ix = ix->next;
		//@ inv <heap_> lseg(old(x),ix)(?A), list(ix)(?B), lseg(y,iy)(?A) <_/heap> /\ A = (?A @ ?B)
		while(ix) {
			iy->next = (struct listNode *)malloc(sizeof(struct listNode));
			iy = iy->next;
			iy->val = ix->val;
			ix = ix->next;
		}
	}
	else
	{
		y = (struct listNode *)malloc(sizeof(struct listNode));
		y->val = x->val;
		y->next = 0;
	}
	  
	  return y;
  }
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
  struct listNode *y;

  x = create(5);
  print(x);
  //@ assert <heap> list(x)([1, 2, 3, 4, 5]) </heap>
  y = copy(x);
  //@ assert <heap> list(x)([1, 2, 3, 4, 5]), list(y)([1, 2, 3, 4, 5]) </heap>
  print(y);
  destroy(x);
  //@ assert <heap> list(y)([1, 2, 3, 4, 5]) </heap>
  destroy(y);
  //@assert <heap> . </heap>
  return 0;
}


//@ var n : Int
//@ var A, B, C : Seq
