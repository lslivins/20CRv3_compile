// nmtools.h 
// header file for nmtools.c 
// nmtools are a collection of misc tools. 
//
#ifndef NMTOOLS_H
#define NMTOOLS_H

// int binarysearch(int data[], int target, int big, int small);
// binary search algorithm implementation
// data array to be sorted
// target: int to look for
// big: largest index of array
// small: smallest index of array
// PRECONDITION: data array has been sorted
// POSTCONDITION: it will return 1 if the target was found
// otherwise it'll return 0 
int binarysearch(long data[], long target, int big, int small);

// void quicksort(int data[], const int n);
// quicksort algorithm implementation 
// this function recursively partition the array and sort the elements
// POSTCONDITION: data has been sorted
void quicksort(long data[], const int n);

// void partition(int data[], const int n, int *pivot_index);
// helper function for quicksort
// POSTCONDITION: pivot_index has been placed where
// two indexes from smallest and largest array elements meet
// and array has been sorted
void partition(long data[], const int n, int *pivot_index);

// time_t make_time(const int yr, const int mo, const int dy, const int hr, const int mn, const int sc);
// this function create time_t expression
// time_t return value is an time_t var made out of yr mo dy hr mn sc values

// int diff_time(const time_t td, const time_t yd);
// calculate difference in seconds  td-yd 
// POSTCONDITION: difference in seconds has been returned

void add_zero(char* ans, const int n);
void add_zeros(char* ans, const int n);

// Recursive Functions
// define power function on my own to use int and float instead of double
float fpow(float input, int n);
// define power function on my own to use int and long instead of double
long lpow(long input, int n);

// float string2float (char* digit);
// it returns float value from string
float string2float (char* digit);

int string2int (char* digit);

long string2long (char* digit);

// E notation (string) to double conversion
double estring2double (char* digit); 

// int get_intvalue(const char c[], const int start, const int last);
// it returns int from char array pos start to pos last
int get_intvalue(const char c[], const int start, const int last);
float get_floatvalue(const char c[], const int start, const int last);
double get_doublevalue(const char c[], const int start, const int last);
//void stringcopy (const char* a, char* b, const int start, const int end);
// copy string from position start to position end
void stringcopy (const char* a, char* b, const int start, const int end);
// unsigned int num_days(const unsigned int y, const unsigned int m);
// Given year and month, it returns the number of days
int num_days(const int y, const int m);
long julday(int mm, int id, int iyyyy);
void caldat(long julian, int*nn, int *id, int *iyyyy);
long fulljulday(int mm, int id, int iyyyy, int ihh, int imn);

// If it finds pattern then it returns the positions (beginning and ending) of the first occurrence
int match(const char *string, char *pattern, int *is, int *ie);
// If it finds "\015" then it returns the position of the first occurrence
// otherwise it returns 0.
int carriage_match(const char *string);

double read_edata(char* data);
#endif
