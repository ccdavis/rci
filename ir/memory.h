#include "tgc.h"
// From "Crafting Interpreters"
//

// 
// All "object" type values in RCI will use reallocate() to allocate and free
// memory.
#include "value.h"
#include <stdlib.h>
//#define DEBUG_STRESS_GC
//#define DEBUG_LOG_GC

#include <stdio.h>

void* reallocate(void* pointer, size_t oldSize, size_t newSize) {
	
	  if (newSize == 0) {
		tgc_free(&gc, pointer);
		return NULL;
	  }

	//void* result = realloc(pointer, newSize);
	void* result = tgc_realloc(&gc,pointer, newSize);
	if (result == NULL){  printf("Can't allocate memory.\n"); exit(1);}

  return result;
}

rci_object * allocateObject(size_t size,rci_object_type  type) ;

#define ALLOCATE_OBJECT(type, objectType) \
    (type*)allocateObject(sizeof(type), objectType)

#define ALLOCATE(type, count) \
    (type*) reallocate(NULL, 0, sizeof(type) * (count))

#define FREE(type, pointer) reallocate(pointer, sizeof(type), 0)
//< free



#define GROW_ARRAY(type, pointer, oldCount, newCount) \
    (type*)reallocate(pointer, sizeof(type) * (oldCount), \
        sizeof(type) * (newCount))
//> free-array

#define FREE_ARRAY(type, pointer, oldCount) \
    reallocate(pointer, sizeof(type) * (oldCount), 0)



rci_object * allocateObject(size_t size,rci_object_type type) {
  rci_object* object = (rci_object*) reallocate(NULL, 0, size);
  object->type = type;
  
//> Garbage Collection init-is-marked
  object->on_heap =  true;
  
#ifdef DEBUG_LOG_GC
  printf("%p allocate %zu for %d\n", (void*)object, size, type);
#endif

  return object;
}



