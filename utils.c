#include <stdio.h>


void array_concat_int(void **f, void **s, void **d)
{
  void **src_curr, **dest_curr;
  int i, j, flen, slen, len, **src, **dest;

  flen = **(int **)f;
  slen = **(int **)s;
  len = slen + flen;

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

    src = (int **) src_curr;
    dest = (int **) dest_curr;
    *dest = *src;
  }
}

void array_concat_char(void **f, void **s, void **d)
{
  void **src_curr, **dest_curr;
  int i, j, len, flen, slen;
  char **src, **dest;

  flen = **(int **)f;
  slen = **(int **)s;
  len = slen + flen;

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

void print_string(void **s) {
  int len = **(int **)s;

  for(int i = 1; i <= len; i++){
    char **curr = (char **) s + i;
    printf("%c", **curr);
  }
  printf("\n");
}

int print_float(double d) {
  printf("%lf\n", d);
  return 0;
}

int print_char(char c) {
  printf("%c\n", c);
  return 0;
}

int print_int(int i) {
  printf("%d\n", i);
  return 0;
}