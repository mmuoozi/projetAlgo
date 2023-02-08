#include <Rcpp.h>
#include <cmath>
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
// [[Rcpp::export]]
Rcpp::NumericVector radix_sort(Rcpp::NumericVector arr) {
  int n = arr.size();
  Rcpp::NumericVector output(n);
  Rcpp::NumericVector count(10);

  // Counting sort
  for (int i = 0; i < n; i++) {
    int index = floor(arr[i] / 1);
    count[(index % 10) + 1] = count[(index % 10) + 1] + 1;
  }

  for (int i = 1; i < 10; i++) {
    count[i] = count[i] + count[i - 1];
  }

  for (int i = n - 1; i >= 0; i--) {
    int index = floor(arr[i] / 1);
    output[count[(index % 10) + 1] - 1] = arr[i];
    count[(index % 10) + 1] = count[(index % 10) + 1] - 1;
  }

  return output;
}

// Main function to perform radix sort on decimal numbers
// [[Rcpp::export]]
Rcpp::NumericVector radix_sort_rcpp(Rcpp::NumericVector arr) {
  double decimal_number = max(arr);
  int whole_number_part = floor(decimal_number);
  double decimal_part = decimal_number - whole_number_part;

  arr = arr * pow(10, decimal_part);
  arr = floor(arr);
  arr = radix_sort(arr);
  arr = arr / pow(10, decimal_part);

  return arr;
}

