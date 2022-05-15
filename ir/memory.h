
// From "Crafting Interpreters"
//
// I want to add a simple garbage collector and CI has one.
// 
// All "object" type values in RCI will use reallocate() to allocate and free
// memory.
#include <stdlib.h>
#include "memory.h"

void* reallocate(void* pointer, size_t oldSize, size_t newSize) {
  if (newSize == 0) {
    free(pointer);
    return NULL;
  }

  void* result = realloc(pointer, newSize);
  return result;
}

