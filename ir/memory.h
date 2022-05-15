
// From "Crafting Interpreters"
//
// I want to add a simple garbage collector and CI has one.
// 
// All "object" type values in RCI will use reallocate() to allocate and free
// memory.
#include "value.h"
#include <stdlib.h>


void* reallocate(void* pointer, size_t oldSize, size_t newSize) {
	  if (newSize == 0) {
		free(pointer);
		return NULL;
	  }

	void* result = realloc(pointer, newSize);
	if (result == NULL){  printf("Can't allocate memory.\n"); exit(1);}

  return result;
}

rci_object * allocateObject(size_t size,rci_object_type  type) ;

#define ALLOCATE_OBJECT(type, objectType) \
    (type*)allocateObject(sizeof(type), objectType)

rci_object * allocateObject(size_t size,rci_object_type  type) {
  rci_object* object = (rci_object*)reallocate(NULL, 0, size);
  object->type = type;
//> Garbage Collection init-is-marked
  object->isMarked = false;
//< Garbage Collection init-is-marked
//> add-to-list
  /*
  object->next = vm.objects;
  vm.objects = object;
  */
//< add-to-list
//> Garbage Collection debug-log-allocate

#ifdef DEBUG_LOG_GC
  printf("%p allocate %zu for %d\n", (void*)object, size, type);
#endif

//< Garbage Collection debug-log-allocate
  return object;
}

