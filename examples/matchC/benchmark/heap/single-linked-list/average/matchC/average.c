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

int summ(struct listNode* a)
//@ rule <k> $ => return thesum(A); </k> <heap_> list(a)(A) <_/heap>
{
	int s;
	struct listNode* x;
	x = a;
	s = 0;
	//@ inv <heap_> lseg(old(a), x)(?A), list(x)(?X) <_/heap> /\ (?A @ ?X) = A /\ (s = thesum(?A))
	while (x != 0) {
		s = s + x->val;
		x = x->next;
	}
	return s;
}

int average(struct listNode* a)
//@ rule <k> $ => return theavg(A); </k> <heap_> list(a)(A) <_/heap>
{
  int s;
  int l;
  
  s = summ(a);
  
  l = length(a);
  s = s / l;
  return s;
}

int main()
{
  struct listNode* x;
  struct listNode* y;
  int s;
  x = (struct listNode*)malloc(sizeof(struct listNode));
  x->val = 5;
  x->next = 0;
  y = (struct listNode*)malloc(sizeof(struct listNode));
  y->val = 4;
  y->next = x;
  x = y;
  y = (struct listNode*)malloc(sizeof(struct listNode));
  y->val = 3;
  y->next = x;
  x = y;
  s = average(x);
  printf("%d\n", s);
  return 0;
}
  
  
//@ var s, l : ?Int
//@ var A, X : Seq
