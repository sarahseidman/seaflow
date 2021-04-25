
void array_concat(void **f, void **s, void **dest)
{
  void **src_curr, **dest_curr;
  int i, j;

  int flen = **(int **)f;
  int slen = **(int **)s;

  int len = slen + flen;

  for(i = 1; i <= flen; i++){
    src_curr = f + i;
    dest_curr = dest + i;

    int **src = (int **) src_curr;
    int **dest = (int **) dest_curr;

    *dest = *src;

  }

  j = flen + 1;
  for (i = 1 ; i <= slen; i++) {
    src_curr = s + i;
    dest_curr = dest + j++;

    int **src = (int **) src_curr;
    int **dest = (int **) dest_curr;

    *dest = *src;
  }
  
}