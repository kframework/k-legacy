#include <stdlib.h>
#include <stdio.h>

struct listNode {
  int val;
  struct listNode *next;
};


struct listNode* insert(struct listNode* x, int i)
/*@ rule <k> $ => return ?x; </k> <heap_> list(x)(A) => list(?x)(?A) <_/heap> if (seq2mset(A) = seq2mset(?A @ [i])) */
{
  struct listNode* iNode;
	if (x) {
		if (x->val > i) {
			iNode = (struct listNode*)malloc(sizeof(struct listNode));
			iNode->val = i;
			iNode->next = x->next;
			x->next = iNode;
		}
		else
		{
			iNode = x->next;
			if (iNode) {
				insert(iNode,i);
			}
			else {
				iNode = (struct listNode*)malloc(sizeof(struct listNode));
				iNode->val = i;
				iNode->next = 0;
				x->next = iNode;
			}
			
		}
	}
	else {
		iNode = (struct listNode*)malloc(sizeof(struct listNode));
		iNode->val = i;
		iNode->next = 0;
		x = iNode;
	}
	
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
	x = create(2);
  print(x);
  // assert <heap_> list(x)([1, 2]) <_/heap>
  x = insert(x,8) ;
  x = insert(x,3) ;
  x = insert(x,6) ;
  print(x);
  // assert <heap_> list(x)([1, 2, 3, 6, 8]) <_/heap>
  return 0;
}

//@ var i, v1, v2 : Int
//@ var A, B, C, D : Seq
