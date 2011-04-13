#include <stdlib.h>
#include <stdio.h>


struct listNode {
  int val;
  struct listNode *next;
};


struct listNode* reverse(struct listNode *x)
/*@ rule <k> $ => return ?p; </k>
 <heap_> list(x)(A) => list(?p)(rev(A)) <_/heap> */
{
  struct listNode *p;
  struct listNode *y;

  p = 0 ;
  //@ inv <heap_> list(p)(?B), list(x)(?C) <_/heap> /\ A = rev(?B) @ ?C
  while(x) {
    y = x->next;
    x->next = p;
    p = x;
    x = y;
  }

  return p;
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
  //@ assert <heap> list(x)([1, 2, 3, 4, 5]) </heap>
  x = reverse(x);
  //@ assert <heap> list(x)([5, 4, 3, 2, 1]) </heap>
  destroy(x);
  //@ assert <heap> . </heap>
  return 0;
}


//@ var n : Int
//@ var A, B, C : Seq

