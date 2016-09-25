int main ()
{
  int x, y=8, z=0, w=0, *r, **k;
  if (x)
    z = y + 1;
  else
    w = y + 1;
  assert (z == 7 || w == 9, "", HALT);
  return (z+w);
}
