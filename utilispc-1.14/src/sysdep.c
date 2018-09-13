#include "constdef.h"

#if !defined(HAVE_MEMMOVE) && !defined(HAVE_BCOPY)
void *memmove(void *s1, const void *s2, size_t n){
  char *cs1=(char *)s1, *cs2=(char *)s2;
  int i;
  if(cs1<cs2){
    for(i=0;i<n;i++) cs1[i]=cs2[i];
  }
  else{
    for(i=n-1;i>=0;i--) cs1[i]=cs2[i];
  }
}
#endif
