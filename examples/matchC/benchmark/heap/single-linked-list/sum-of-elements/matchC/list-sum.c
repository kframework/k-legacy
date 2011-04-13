#include <stdlib.h>
#include <stdio.h>


struct listNode {
  int val;
  struct listNode *next;
};

int summ(struct listNode* a)
//@ rule <k> $ => return thesum(A); </k> <heap_> list(a)(A) => list(a)(A) <_/heap>
{
  int s;
  struct listNode* x;
  x = a;
  s = 0;
//@ inv <heap_> lseg(old(a),x)(?A), list(x)(?X) <_/heap> /\ (?A @ ?X) = A /\ (s = thesum(?A))
  while (x != 0) {
    s = s + x->val;
    x = x->next;
  }
  return s;
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
  int s;
  struct listNode* x;
  struct listNode* y;
  x = create(5);
  s = summ(x);
  printf("%d\n", s);
  // assert <out> [content] </out>
  return 0;
}

//@ var s : Int
//@ var A, X : Seq
