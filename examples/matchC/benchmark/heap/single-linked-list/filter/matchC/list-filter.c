struct listNode {
  int val;
  struct listNode *next;
};


struct listNode* filter(struct listNode* x, int i)
//@ rule <k> $ => return ?x; </k> <heap_> list(x)(A) => list(?x)(?A) <_/heap> if ~(contain(?A, i))
{
	struct listNode* y;
	struct listNode* z;
	y = x;
  
	if (x != 0)
	{
//@ inv <heap_> list(x)(?A) <_/heap>
	while ((y->val == i) && (y != 0))
	{
		x = y->next;
		free(y);
		y = x;
	}
	z = y;
	y = y->next;
  
/*@ inv <heap_> lseg(x,z)(?A), lseg(z,y)([?v]), list(y)(?B) <_/heap> /\
		~(contain(?A, i)) /\ ~(?v = i) /\ ~(z = 0)  */
	while(y != 0)
	{
		if(y->val == i)
		{
			z->next = y->next;
			free(y);
			y = z->next;
		}
		else
		{
			z = y;
			y = y ->next;
		}
	}
	return x;
	}
	else {
		return 0;
	}

}


int main()
{
	return 0;
}

//@ var A, B : Seq
//@ var v : Int

