#include "utils.hpp"

#include <iostream>
#include <sstream>
#include <string>
#include <cstring>

#define ARR_SIZE 8

/* todo:
 *      * read doubles from cin and put them into an array until something that
 *        is not a number is entered. (std::cin.fail() is true)
 *      * increase the array's size if necessary
 *      * reset cin (see clear() and ignore())
 *      * read in the same amount of integers
 *      * implement functions in utils.cpp
 *      * test given input with isPermutation() and isSorted()
 *      * clean up all dynamically allocated memory
 */
template <class T>
void print_array(T* array, int size) {
  std::cout << "[";
  for (int i = 0; i < size; i++) {
    std::cout << array[i] << ", ";
  }
  std::cout << array[size] << "]" << std::endl;
}

void resize(double** array, int new_size, int copy_size) {
  //std::cout << "Resizing to: " << new_size << std::endl;
  double* new_array = new double[new_size]();
  std::memcpy(new_array, *array, copy_size * sizeof(double));
  delete[] *array;
  *array = new_array;
}

int main(int, char**) {

  double d_input;
  int input_count = 0;
  
  //initialuze array and store its current size
  double *input_array = new double[ARR_SIZE]();
  int current_size = ARR_SIZE;
  int resize_factor = 2;

  while (1) {
    //read inputs
    std::cin >> d_input;
    if (std::cin.fail()) {
      //clear cin on fail and break loop
      std::cin.clear();
      std::cin.ignore();
      break;
    }
    //resizing shit
    if (input_count >= current_size) {
      resize(&input_array, resize_factor * ARR_SIZE, current_size);
      current_size = resize_factor * ARR_SIZE;
      resize_factor += 1;
    }
    input_array[input_count] = d_input;
    input_count++;
  }
  
  print_array(input_array, input_count-1);
  
  //init array for permutation
  int *permutation_array = new int[input_count]();
  int i_input;
  int i_input_count = 0;

  //read as many numbers as entered before
  while (i_input_count < input_count) {
    std::cin >> i_input;
    //error handling
    if (std::cin.fail()) {
      std::cout << "...really?" << std::endl;
      std::cin.clear();
      std::cin.ignore();
      continue;
    } else {
      permutation_array[i_input_count] = i_input;
      i_input_count++;
    }
  }
  
  print_array(permutation_array, input_count-1);
  
  if (isPermutation(permutation_array, i_input_count))
    std::cout << isSorted(input_array, input_count, permutation_array) << std::endl;

  delete[] permutation_array;
  delete[] input_array;
}
