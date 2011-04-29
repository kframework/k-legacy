#include <stdlib.h>
#include <stdio.h>


struct listNode {
  int val;
  struct listNode *next;
};

struct listNode* append(struct listNode *x, struct listNode *y)
/*@ rule <k> $ => return x1; </k>
 <heap_> list(x)(A), list(y)(B) => list(x1)(A @ B) <_/heap> */
{
	struct listNode *p;
	if (x == 0)
		return y;
	
	p = x;
	/*@ inv <heap_> lseg(x, p)(?A1), list(p)(?A2) <_/heap> 
	 /\ A = ?A1 @ ?A2 /\ ~(p = 0) /\ y = !y */
	while (p->next)
		p = p->next;
	p->next = y;
	
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


int main()
{
  struct listNode *x;
  struct listNode *y;

  x = create(4);
  printf("x: ");
  print(x);
  //@ assert <heap_> list(x)(!A1) <_/heap>
  y = create(3);
  printf("y: ");
  print(y);
  //@ assert <heap_> list(x)(!A1), list(y)(!A2) <_/heap>
  x = append(x, y);
  printf("append(x, y): ");
  print(x);
  //@ assert <heap_> list(x)(!A1 @ !A2) <_/heap>
  destroy(x);
  //@ assert <heap_> . </heap>
  
  return 0;
}


//@ var n : Int
//@ var A, B, C : Seq
//@ var H : MapItem

