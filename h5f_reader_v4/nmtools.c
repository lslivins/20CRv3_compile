// program nmtools.c
// implementation file for nmtools.h
//
// April 5, 2006
//
// Nobuki Matsui
// PSD1 ESRL 
//
//
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include <regex.h>
#include "nmtools.h"
#define IGREG 2299161

// function prototypes
//int binarysearch(int data[], int target, int big, int small);
//void quicksort(int data[], const int n);
//void partition(int data[], const int n, int *pivot_index);
//time_t make_time(const int yr, const int mo, const int dy, const int hr, const int mn, const int sc);
//int diff_time(const time_t td, const time_t yd);
//void int2stringid(char *str, int dat);

// Functions----------------------------------------------------------------
// int binarysearch(int data[], int target, int big,  int small){
// Implementation of binary search algorithm.
// PRECONDITION
// 1. array has been sorted.
// 2. big is the largest index of array, small is the smallest index of array 
// 3. target is some integer
// POSTCONTION
// if found, it returns the pos, otherwise it returns negative value
int binarysearch(long data[], long target, int big,  int small){
     while (small <= big){
        int mid = (small + big) /2;
        if(target > data[mid])
            small = mid + 1;
        else if (target < data[mid])
            big = mid - 1;
        else 
            return mid;
    } 
    return -(small + 1);
  
}

// void quicksort(int data[], const int n){
// PRECONDITION 
// 1. data is an int array
// 2. n is the number of elements in the array
// POSTCONDITION
// the array has been sorted.
void quicksort(long data[], const int n){
    int pivot_index;
    int n1;
    int n2;

    if(n > 1) {
        partition(data,n,&pivot_index);
         
        n1 = pivot_index;
        n2 = n - n1 - 1;

        quicksort(data, n1);
        quicksort((data + pivot_index + 1), n2);
    }
}

// void partition(int data[], const int n, int *pivot_index){
// Helper function for quicksort
// PRECONDITION
// 1. data hasn't been sorted.
// 2. n is the number of elements in the array
// POSTCONDITION
// 1. sorting has occured.
// 2. pivot_index points to where too_small_index and too_big_index
// cross.
void partition(long data[], const int n, int *pivot_index){
    long pivot = data[0];
    int too_big_index = 1;
    int too_small_index = n-1;
    long swap;

    while (too_big_index <= too_small_index) {
        while(too_big_index != (int)n && data[too_big_index] <= pivot)
           too_big_index++;
        while(data[too_small_index] > pivot)
           too_small_index--;
        if(too_big_index < too_small_index){
           swap = data[too_small_index];
           data[too_small_index] = data[too_big_index];
           data[too_big_index] = swap; 
        }
    } 
    *pivot_index = too_small_index;
    data[0] = data[*pivot_index];
    data[*pivot_index] = pivot;
}

void add_zero(char* ans, const int n){
     char text[1];

     if(n < 10)
         strcat(ans,"0");
 
     sprintf(text, "%i", (int)n);
     strcat(ans,text); 
}

void add_zeros(char* ans, const int n){
     char text[1];

     if(n < 10)
         strcat(ans,"000000");
     else if(n < 100)
         strcat(ans,"00000");
     else if(n < 1000)
         strcat(ans,"0000");
     else if(n < 10000)
         strcat(ans,"000");
     else if(n < 100000)
         strcat(ans,"00");
     else if(n < 1000000)
         strcat(ans,"0");
 
     sprintf(text, "%i", (int)n);
     strcat(ans,text); 
}

void int2stringid(char *str, int dat) {

    int base = 1000000;
    int i;
    int digi, diff, remainder;

    char text[1];


    for(i=0;i<7;i++){
        remainder = dat % base;
        diff = (dat - remainder);
        if (diff > 0)
            digi = diff /base;
        else
            digi = 0;

        base *= 1/10;
        dat = remainder;
        sprintf(text, "%i", digi);

        *str = *text;
        str++;
     }
     // terminate the string
     str[7] = '\0';
}

// int get_intvalue(const char c[], const int start, const int last){
// INPUT  c     char array
//        start start pos 
//        end   end pos
// OUTPUT corresponding int value
//
// LOGIC behind
// Character to Numeric Conversion Functions
// -i.e.
//  character position 012
//  numerical value    364
//  breakdowns         3 x 100 + 6 x 10 + 4 x 1  
//  the last exponent is expressed as n^0 = 1
//
// NOTE
// arrays in function arg in c are pass by reference by definition
int get_intvalue(const char c[], const int start, const int last){
    int answer;
    int i;
    int negative = 0;

    answer = 0;

    for (i = (int)start; i < (int)last; i++) { 
         if((int) c[i] < 45 || (int) c[i] > 57 || (int) c[i] == 47) answer *= 1; // if it's non-numeric value
         else if((int) c[i] == 45) negative = 1;                  // if it's negative sign  
         else  answer = (answer * 10) + (c[i] - '0');
    }

    if(negative == 1) answer *= -1;

    return answer;
}

// float get_floatvalue(const char c[], const int start, const int last){
// float version
float get_floatvalue(const char c[], const int start, const int last){
    float answer;
    int  i, j;
    int negative = 0;
    int decimal = 0;
    float mantissa;

    answer = 0.0;
    mantissa = 0.0;
    j = 1;
  
    for (i = (int)start; i < (int)last; i++) { 
         if((int) c[i] < 45 || (int) c[i] > 57 || (int) c[i] == 47) answer *= 1.0; // if it's non-numeric value
         else if((int) c[i] == 45) negative = 1;                   // if it's negative sign     
         else if ((int) c[i] == 46 )  decimal = 1; 
         // we have to take a decimal point into consideration, thus conditional
         else if(decimal == 1) { 
             mantissa = mantissa + ((float) (c[i] - '0'))* fpow(0.1, j); 
             j++; 
         }
         else  answer = (answer * 10) + (c[i] - '0');
    }

    answer += mantissa;
    if(negative == 1)  answer *= -1;   

    return answer;
}

double get_doublevalue(const char c[], const int start, const int last){
    double answer;
    int  i, j;
    int negative = 0;
    int decimal = 0;
    double mantissa;

    answer = 0.0;
    mantissa = 0.0;
    j = 1;
  
    for (i = (int)start; i < (int)last; i++) { 
         if((int) c[i] < 45 || (int) c[i] > 57 || (int) c[i] == 47) answer *= 1.0; // if it's non-numeric value
         else if((int) c[i] == 45) negative = 1;                   // if it's negative sign     
         else if ((int) c[i] == 46 )  decimal = 1; 
         // we have to take a decimal point into consideration, thus conditional
         else if(decimal == 1) { 
             mantissa = mantissa + ((double) (c[i] - '0'))* fpow(0.1, j); 
             j++; 
         }
         else  answer = (answer * 10) + (c[i] - '0');
    }

    answer += mantissa;
    if(negative == 1)  answer *= -1;   

    return answer;
}

// Recursive Functions
// define power function on my own to use int and float instead of double
float fpow(float input, int n) {

     if(n == 0) return 1;
     else return fpow(input, n-1) * input;
} 

// define power function on my own to use int and long instead of double
long lpow(long input, int n) {

     if(n == 0) return 1;
     else return lpow(input, n-1) * input;
} 

float string2float (char* digit) {
    int result = 0;
    int mantissa = 0;
    int negative = 0;
    float answer = 0;
    int i;

    while ( (*digit >= '0' && *digit <= '9')|| *digit == '-' || *digit == '.' || *digit == ' ') {
           if( *digit == '-') { 
               negative = 1; 
           }
           else if ( *digit == '.' ) { 
               digit++;  // compenstaiont: otherwise it'll get to the next iteration without incrementing digit
               break; 
           }
           else if ( *digit == ' ') result *= 1;
           else  result = (result * 10) + (*digit - '0');
           digit++; 
    }
   
    for (i = 0; i < 2; i++){
       if(i == 1 && *digit == ' ') mantissa = (mantissa * 10); 
       else mantissa = (mantissa * 10 ) + (*digit - '0');
       digit++;
    }  

    answer = result + mantissa * 0.01;
    if (negative == 1) answer *= -1; 

    return answer; 
}
int string2int (char* digit) {
    int result = 0;
    int negative = 0;

    while ( (*digit >= '0' && *digit <= '9')|| *digit == '-' ) {
           if( *digit == '-') { 
               negative = 1; 
           }
           else  result = (result * 10) + (*digit - '0');
           digit++; 
    }
   
    if (negative == 1) result *= -1; 

    return result; 
}

long string2long (char* digit) {
    long result = 0;
    long negative = 0;

    while ( (*digit >= '0' && *digit <= '9')|| *digit == '-' ) {
           if( *digit == '-') { 
               negative = 1; 
           }
           else  result = (result * 10) + (*digit - '0');
           digit++; 
    }
   
    if (negative == 1) result *= -1; 

    return result; 
}

double estring2double (char* digit) {
    int result = 0;
    int ex = 0;
    int negative = 0;
    int neg = 0;
    double answer = 0;
    int i;
    int e = 0;

    while ( (*digit >= '0' && *digit <= '9')|| *digit == '-' || *digit == '.' || *digit == ' ' || *digit == 'E' || *digit == '+') {
        if( *digit == 'E') e = 1;
        else if ( e == 1) {
            if( *digit == '-')  neg = 1;
            else if( *digit == '+')  ;
            else  ex = (ex * 10) + (*digit - '0');
        }
        else {
            if( *digit == '-') {
                negative = 1;
	    }
	    else if ( *digit == '.' ) result *= 1;
	    else if ( *digit == ' ') result *= 1;
	    else  result = (result * 10) + (*digit - '0');
        }
        digit++;

    }

    if(neg == 1) ex *= -1;
    answer = ( (double) result / (double) 1000 ) * pow((double) 10, (double)ex);
    if(negative == 1) answer *= -1;

    return answer;

}


// void stringcopy (const char* a, char* b, const int start, const int end) {
// INPUT  a    pointer to char array
//        start start pos 
//        end   end pos
// OUTPUT b    pointer to char array
//  
// The function takes care of string input
void stringcopy (const char* a, char* b, const int start, const int end) {
    int counter;
    int i, diff;
    counter = start;

    //a += counter;
    diff = end - start;

    for (i = 0; i < diff; i++) { 
           b[i] = a[counter+i];
     //   *b = *a; 
     //   a += sizeof(char);
     //   b += sizeof(char);
    }
    b[diff] = '\0';
}

int num_days(const int y, const int m){
    int days[12] = {31,28,31,30,31,30,31,31,30,31,30,31};

    if((y%4 == 0 && y% 100 != 0) || y%400 == 0)
        days[1] = 29;
  
    return days[m];
}

long julday(int mm, int id, int iyyyy){
    long jul;
    int ja,jy=iyyyy,jm;
    
    if(jy == 0) fprintf (stderr,"julday: there is no year zero.");
    if (jy < 0 ) ++jy;
    if (mm > 2) {
        jm=mm+1;
    } 
    else {
        --jy;
        jm=mm+13;
    }

    jul = (long) (floor(365.25*jy)+floor(30.6001*jm)+id+1720995);
    if(id+31L*(mm+12L*iyyyy) >= IGREG) {
        ja=(int)(0.01*jy);
        jul += 2-ja+(int) (0.25*ja);
    }
    return jul;
}  

void caldat(long julian, int *mm, int *id, int *iyyy){
    long ja, jalpha, jb,jc,jd,je;
 
    if (julian >= IGREG) {
        jalpha=(long)(((double) (julian-1867216)-0.25)/36524.25);
        ja=julian+1+jalpha-(long) (0.25*jalpha);
    }
    else if ( julian < 0 ) {
        ja=julian+36525*(1-julian/36525);
    }
    else {
        ja=julian;
    }
    
    jb=ja+1524;
    jc=(long)(6680.0+((double) (jb-2439870) - 122.1) / 365.25);
    jd=(long)( 365 * jc + (0.25 * jc));
    je=(long)((jb-jd)/30.6001);
    *id=jb-jd-(long) (30.6001*je);
    *mm-je-1;
    if (*mm > 12 ) *mm -= 12;
    *iyyy=jc-4715;
    if (*mm > 2) --(*iyyy);
    if (*iyyy <= 0) --(*iyyy);
    if (julian < 0) *iyyy -= 100*(1-julian/36525);
}         

long fulljulday(int mm, int id, int iyyyy, int ihr, int imn){
    return julday(mm,id, iyyyy) * 24 * 3600 + (long) ((((ihr - 12) * 60) + imn)*60);
}

int match(const char *string, char *pattern, int *is, int *ie){
    int status;
    regex_t re;
    regmatch_t* rp;
    // regmatch_t
    //  regoff_t        rm_so  
    //  regoff_t        rm_eo
    //  typedef regoff_t int 

    rp = malloc(sizeof(regmatch_t));
    if (regcomp(&re, pattern, REG_EXTENDED) != 0) {
        return(0);      /* report error */
    }
    //status = regexec(&re, string, (size_t) 0, NULL, 0);
    status = regexec(&re, string, (size_t) 1, rp, 0);
    //printf ("%i %i\n", rp->rm_so, rp->rm_eo);
    *is = rp->rm_so;
    *ie = rp->rm_eo; 
    regfree(&re);
    free(rp);

    // The following demonstrates how the REG_NOTBOL flag could  be
    // used  with  regexec()  to find all substrings in a line that
    // match a pattern supplied by a user. (For simplicity  of  the
    // example, very little error checking is done.)
    // (void) regcomp (&re, pattern, 0);
    /* this call to regexec() finds the first match on the line */
    // error = regexec (&re, &buffer[0], 1, &pm, 0);
    // while (error == 0) {     /* while matches found */
    //        /* substring found between pm.rm_so and pm.rm_eo */
    //        /* This call to regexec() finds the next match */
    //        error = regexec (&re, buffer + pm.rm_eo, 1, &pm, REG_NOTBOL);
    // }
    // Standard C Library Functions                          regcomp(3C)

    if (status != 0) {
        return(0);      /* report error */
    }
    return(1);
}


int carriage_match(const char *string){
    int status;
    regex_t re;
    regmatch_t* rp;
    int answer;

    rp = malloc(sizeof(regmatch_t));

    if (regcomp(&re, "\015", REG_EXTENDED) != 0) {
        return(0);      /* report error */
     }

     status = regexec(&re, string, (size_t) 1, rp, 0);
     answer = rp->rm_so;
     regfree(&re);
     free(rp);

    if (status != 0) {
        return(0);      /* report error */
    }

    return (answer);
}

double read_edata(char* data){
// -.661E+00
//0123456789

    float f;
    int   i = 0;
    int   negative_exp = 1;

    f =  get_floatvalue(data,0,6);
    if((int) data[7] == 45)
        negative_exp = -1;

    i = get_intvalue(data,8,10);
    // printf("%lf %lf\n",  f,  ( (double) f ) * pow(10, (i * negative_exp) ));

    return   ( (double) f ) * pow(10, (i * negative_exp) );
}

