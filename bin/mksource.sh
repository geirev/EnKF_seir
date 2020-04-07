#!/bin/ksh
ls mod_*.F90 m_*.F90 2>/dev/null |\
awk  '{
    CC[NR]=$1
}
END { 
    if (NR>0) {
       printf("MODULES = \\\n")
       for (i = 1; i < NR; i++) printf("\t%s\\\n", CC[i])
       printf("\t%s\n\n", CC[NR])
    }
}'

ls mod_*.F m_*.F 2>/dev/null |\
awk  '{
    CC[NR]=$1
}
END { 
    if (NR>0) {
       printf("MODULES77 = \\\n")
       for (i = 1; i < NR; i++) printf("\t%s\\\n", CC[i])
       printf("\t%s\n\n", CC[NR])
    }
}'

ls *.H  2>/dev/null |\
awk  '{
    CC[NR]=$1
}
END { 
    if (NR > 0) {
       printf("INC1 = \\\n")
       for (i = 1; i < NR; i++) printf("\t%s\\\n", CC[i])
       printf("\t%s\n\n", CC[NR])
    }
}'


ls *.F90  2>/dev/null | sed -e '/^mod_/d' -e '/^m_/d' -e '/^p_/d' |\
awk  '{
    CC[NR]=$1
}
END { 
    if (NR>0) {
       printf("F90FILES = \\\n")
       for (i = 1; i < NR; i++) printf("\t%s\\\n", CC[i])
       printf("\t%s\n\n", CC[NR])
    }
}'


ls  *.F  2>/dev/null | sed -e '/^mod_/d' -e '/^m_/d' -e '/^p_/d' |\
awk  '{
    CC[NR]=$1
}
END { 
    if (NR>0) {
       printf("F77FILES = \\\n")
       for (i = 1; i < NR; i++) printf("\t%s\\\n", CC[i])
       printf("\t%s\n\n", CC[NR])
    }
}'

