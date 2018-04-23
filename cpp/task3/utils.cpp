#include "utils.hpp"
#include <iostream>

bool isPermutation(int* perm, int count) {

  bool is_permutation = false;

  for (int i = 0; i < count; i++) {
    is_permutation = false;
    for (int j = 0; j < count; j++) {
      if (perm[j] == i)
        is_permutation = true;
    }
    if (!is_permutation)
      return false;
  }
  return true;
}

bool isSorted(double* data, int dataCount, int* perm) {
  for (int i = 0; i < dataCount - 1; i++) {
    if (data[perm[i]] > data[perm[i+1]])
      return false;
  }
  return true;
}
