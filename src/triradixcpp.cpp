#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
int getMax(IntegerVector arr, int n)
{
  int mx = arr[0];
  for (int i = 1; i < n; i++)
    if (arr[i] > mx)
      mx = arr[i];
    return mx;
}

// A function to do counting sort of arr[] according to
// the digit represented by exp.
// [[Rcpp::export]]
void countSort(IntegerVector arr, int n, int exp)
{
  IntegerVector output(n); // output array
  int i, count[10] = { 0 };

  // Store count of occurrences in count[]
  for (i = 0; i < n; i++)
    count[(arr[i] / exp) % 10]++;

  // Change count[i] so that count[i] now contains actual
  //  position of this digit in output[]
  for (i = 1; i < 10; i++)
    count[i] += count[i - 1];

  // Build the output array
  for (i = n - 1; i >= 0; i--) {
    output[count[(arr[i] / exp) % 10] - 1] = arr[i];
    count[(arr[i] / exp) % 10]--;
  }

  // Copy the output array to arr[], so that arr[] now
  // contains sorted numbers according to current digit
  for (i = 0; i < n; i++)
    arr[i] = output[i];
}

// The main function to that sorts arr[] of size n using
// Radix Sort
// [[Rcpp::export]]
IntegerVector radixsort(IntegerVector arr)
{
  int n = arr.size();

  // Find the maximum number to know number of digits
  int m = getMax(arr, n);

  // Do counting sort for every digit. Note that instead
  // of passing digit number, exp is passed. exp is 10^i
  // where i is current digit number
  for (int exp = 1; m / exp > 0; exp *= 10)
    countSort(arr, n, exp);

  return arr;
}

//Code for the decimal
// Function to get a specific digit of a number
// [[Rcpp::export]]
int get_digit(int num, int d) {
  return((num % int(pow(10, d))) / int(pow(10, d - 1)));
}

// Counting Sort function to sort the input array based on a specific digit
// [[Rcpp::export]]
NumericVector counting_sort_by_digit(NumericVector arr, int digit_position) {
  int n = arr.size();
  NumericVector output(n);
  NumericVector count(10);
  for (int i = 0; i < n; i++) {
    int index = get_digit(arr[i], digit_position) + 1;
    count[index - 1]++;
  }
  for (int i = 1; i < 10; i++) {
    count[i] += count[i - 1];
  }
  for (int i = n - 1; i >= 0; i--) {
    int index = get_digit(arr[i], digit_position) + 1;
    output[count[index - 1] - 1] = arr[i];
    count[index - 1]--;
  }
  return output;
}

// Radix Sort function
// [[Rcpp::export]]
NumericVector radix_sort_rcpp(NumericVector arr) {
  int max_num = Rcpp::max(arr);
  int num_digits = floor(log10(max_num)) + 1;
  for (int i = 1; i <= num_digits; i++) {
    arr = counting_sort_by_digit(arr, i);
  }
  return arr;
}

