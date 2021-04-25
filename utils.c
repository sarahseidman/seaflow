
void array_concat_int(void **f, void **s, void **d)
{
  void **src_curr, **dest_curr;
  int i, j, **src, **dest;

  int flen = **(int **)f;
  int slen = **(int **)s;

  int len = slen + flen;

  for(i = 1; i <= flen; i++){
    src_curr = f + i;
    dest_curr = d + i;

    src = (int **) src_curr;
    dest = (int **) dest_curr;

    *dest = *src;

  }

  j = flen + 1;
  for (i = 1 ; i <= slen; i++) {
    src_curr = s + i;
    dest_curr = d + j++;

    int **src = (int **) src_curr;
    int **dest = (int **) dest_curr;

    *dest = *src;
  }
  
}

void array_concat_char(void **f, void **s, void **d)
{
  void **src_curr, **dest_curr;
  int i, j;
  char **src, **dest;

  int flen = **(int **)f;
  int slen = **(int **)s;

  int len = slen + flen;

  for(i = 1; i <= flen; i++){
    src_curr = f + i;
    dest_curr = d + i;

    src = (char **) src_curr;
    dest = (char **) dest_curr;

    *dest = *src;

  }

  j = flen + 1;
  for (i = 1 ; i <= slen; i++) {
    src_curr = s + i;
    dest_curr = d + j++;

    src = (char **) src_curr;
    dest = (char **) dest_curr;

    *dest = *src;
  }
  
}