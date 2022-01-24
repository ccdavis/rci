# rci

Building a statically typed language using Crafting Interpreters as a launching pad. 

The language somewhat resembles "Lox" from the book, but with explicit types and user definable types.

This is a two-part project. First, develop the language with the tree-walking interpreter to play around with different ideas for the language. Second, implement a compiler when the language has been decided on.

The main idea is to try out some familiar language features combined in new ways. The target lies somewhere between ___Go___, ___Python___ and ___Nim___ with some elements of ___Object Pascal___.

### Current Language Proposal

* Functions, including nesting functions. Function declarations capture their environment as closures.
* Function overloading both with arity and parameter types. This can occasionally make hard to debug code but allows for interesting types of polymorphism without  dynamic dispatch or inheritance.
* Variable declarations with 'var' and 'val' types as in Scala: 'val' types are immutable.
* Static typing with reasonable type inference during variable declaration and initialization.
* Function parameters default to 'val' type, but can be 'var' if specified in the definition, or 'cpy' if the argument value needs to be mutable but cause no side-effects. Only 'cpy' causes pass-by-value, only 'var' can alter the passed-in value.
* Actual newtype types created by a 'type' declaration. This is different from 'type' in ___Rust___ or 'typedef' in ___C___ which simply aliases the type name. To do this well the language has to allow common operators to work on directly derived types but prevent automatic operation between say 'kilometers' and 'int64' -- but allow them when explicit casts (but only if the newtype derives from the exact same built-in type.)
* Enumeration types with ordering and name assignments
* Set type, along with dynamic arrays and hashtables.
* Set operators on all collection types
* Pattern matching case statements
* Record types that ensure field order and always support serialization
* Table types that always support sorting, serialization and a few key functional-style methods like 'map', 'select', 'filter', 'reduce'. This is not a Pandas clone. The main difference between a 'Table' type and an array of Record instances is that a 'Table' is guaranteed to contain Record type data. It is such a common use-case that it deserves its own type: It can represent result sets from a database query, a CSV or Parquet file or JSON objects with a consistent schema. Ideally table data could be stored with an efficient memory layout like Arrow.
* No inheritance for table and record types. This helps to keep the mismatch between relational data (in a database) and in the program smaller.
* Heap allocations kept to a minimum. When needed, they should be handled with reference counting, or a garbage collector as an option to avoid the cycle problem of ref-counting.

```
fun fib(n: int64): int64 {
	if n = 0 or n = 1 { return n; }
	return fib(n - 1) + fib(n - 2);
}
```

```
val escape_radius = 2.0;

// Check a point in the complex plane for membership
fun in_mandelbrot(c_x:flt64, c_iy:flt64): boolean {

	fun not_escaped(x:flt64, iy:flt64): boolean {
		return (x*x + iy*iy) < 2*escape_radius; 	
	}

	var iterations = 0;
	var zn_x = 0.0;
	var zn_iy = 0.0;	
	var tmp_zx = 0.0;
	while iterations < max_iterations and not_escaped(zn_x, zn_iy) {					
		tmp_zx := c_x + (zn_x * zn_x - zn_iy * zn_iy);
		zn_iy := c_iy + 2.0 * zn_x * zn_iy;    
		zn_x := tmp_zx;
		iterations := iterations + 1;
	}
	return iterations < max_iterations;
}
```

```
type meters = flt64;
type liters = flt64;
type cm3 = flt64;
type kg = flt64;
type pound = flt64;
type metal: enum  = [ gold, platinum, lead, silver, copper, unknown];
type color:enum = [ yellow, orange, silver];

fun metal_test(mass: grams, volume: cm3): metal {	
	when mass /volume is
		8.94..8.96 then { return copper;},
		19.28..19.31 then { return gold; },
		21.45..21.53 then { return platinum; },
		11.28..11.32 then { return lead; },
		10.4..10.6 then  { return silver; }
		else { return unknown; }	
}

fun metal_color(this_metal: metal): color {
	// If the 'when' is exhaustive you don't need 'else'; the 
	// compiler should produce an error if you don't cover all cases of an enum.
	// All other types require 'else'.
	when metal is 
		gold then { return yellow;},
		silver::metal then { return silver::color; },
		platinum then { return silver::color; },
		lead then { return silver::color; },
		copper then { return orange; }		
}
```

### State of the Interpreter

The interpreter has a static type checker that runs before executing code; there are some additional and redundant type checks during execution. 

The type checker uses symbol tables collected during parsing. These are placed in the AST nodes for statements which introduce new environments, namely blocks and functions. The interpreter dynamically creates environments that mirror these symbol tables. Currently these are independent processes but I'd like to improve the interpreter by using the symbol tables as the templates for the interpreter environments. This could improve performance and simplify the interpreter.


### Language to-do list:

* Add classes following the book
* Support set and enumeration types
* A few more basic statements like 'case' or 'switch' and 'for'.
* File I/O for lines of text (strings) and directly to and from other built-in types
* Standard containers: vectors and hash tables, maybe ordered maps, arrays and matrices
* Devise a few statements that take and produce the container types, like map, for-each
* Simple record /struct types
* Table types made of records supporting special table-like operations (think Pandas-light)
* Add user-definable types including newtype types

### Engineering to-do list

* Clean up the error message types by consolidating into one main error type. This would remove the need for re-enclosing messages into different types.
* Clean up error reporting to always include the location in the source; the capability exists, I was just lazy about crafting the error messages in places.
* Add a basic module system to load libraries.
* Expand standard library
* Auto formatter
* Compile to bytecode


## Performance

The tree-walk approach won't ever produce great results. But I was interested in how to get something decent before moving on to building the compiler.  I was curious about various approaches to using Rust to mimic what had been done in Java for the book. 

One version closely following the book's architecture that can't run the Mandelbrot program, but can run recursive fib took three times longer to compute fib(25) compared to RCI. It has a bug with variable scoping.

I compared several implementations of Lox in Rust. At least two of them are broken for any real programs so I couldn't test

Using a naive Mandelbrot set renderer as a benchmark:

Performance with 1600 x 1600 50 iterations

	RCI  -- strictly typed Lox -- (mine) 162 seconds

Other versions of Lox with the same program:
	
	Rust Tree-walk version: Took 331 seconds. The bytecode compiled version didn't work.  :  https://github.com/tdp2110/crafting-interpreters-rs
	
	Rust tree-walk interpreter: this one took 74 seconds : https://github.com/julioolvr/rlox
	
	JLox: 30.8 seconds This is on Java, right from the Crafting Interpreters book

Other languages. All were similar naive approaches with single threading.

	Similar Ruby  program on JRuby 9000: 48 seconds
	
	Python 2 and Ruby 1.x would probably be similar to the JRuby results
	
	Ruby 2.7: 6.5 seconds	
	Similar Rust: 0.1 seconds
	Free Pascal 0.33 seconds
//
Just as a way to learn more about ___Rust___ it would be interesting to profile my ___RCI___ interpreter to see if there are any easy wins. I suspect it's just that I'm using clone() too much and that those are difficult to remove without switching to unsafe ___Rust___ or introducing complex lifetime annotations.
 
Language:
============

Expand this into a very informal  language specification.

### Program

program ->  STATEMENT-LIST

### Statements

#### Block

left-brace STATEMENT-LIST right-brace

#### If

if EXPR BLOCK else BLOCK
if EXPR BLOCK


#### While

while EXPR BLOCK

#### Var, Val

```
var name = expr;
var name: data-type = expr;
val name = expr;
val name: data-type = expr;
```

#### functions

```
fun name(param1: data-type, param2: data-type... paramn: data-type): return-type BLOCK
```
#### type  declarations

```
type name = data-type;
```

#### Records

```
type my_data = record( 
	field1_name: data-type,
	field2_name:  data-type,
	...
);
```

#### Arrays
```
type my_dataset_type = array<my_data>;
```

### Expressions

Expressions are standard with 'and' and 'or' and 'not' for boolean operators, <code> <> </code> for not-equal, and assignment is an expression: ':='. Equality is with '='. We avoid the whole '=' vs '==' mess. Otherwise precedence is as you'd expect. I would do bitwise operations with keywords like 'shl' and 'shr'.

### Special Symbols

There are symbols left for special uses: "!","?", "->", "&", "@", "*". The plan is for "!" to be used to indicate mutation on a record.



#### Parameter passing semantics:

The caller doesn't need to annotate arguments.  Function declarations dictate how values get sent to the function.

fun my_func(DECL-TYPE name: TYPE, ...): TYPE {
	...
	return EXPRESSION;
}

The DECL-TYPE may have three values: 'var', 'val' or 'cpy'. Parameters are 'val' by default and it doesn't need to be specified.

A 'val' parameter is immutable. A 'cpy' parameter is mutable within the function scope but changes to it doen't have side-effects outside the function. A 'var' parameter is mutable and changes will be reflected outside the function scope.

Most languages have 'cpy' as the default type of parameter. A 'const' parameter type -- or "const &" in C++ -- is most similar to 'val'. A 'var' parameter type is a pass-by-reference non-constant type.

The idea to use 'val' as the default parameter type is to guide the program writer to a more efficient and functional style. A good compiler for the language should optimize 'val' parameters to behave like const reference parameters. The 'cpy' parameter passing style should be explicit in cases where the values are large.  Possibly it makes sense to make number and boolean types 'cpy' by default since there's no overhead in passing them by value; then again having a set of immutable variables  as arguments may lead to fewer mistakes.



## RCI Language examples

Strings are immutable , function parameters are always explicitly typed and functions must have a return type.  The expression taken by the "return" statement must match the return type. 
```
fun hello(message: string, times: number) : string {
	if times > 1 {
		message = message + hello(message, times - 1);
		message = message +" ";
	}
	return message;	
}

print hello("Hello, World!", 5);
```

Variables, immutable values can be passed around. There's limited type-inference for vars and vals allowing you to skip declaring the type of a var or val, but it's always allowed. Declarations use '=' to initialize values. Use ':=' to mutate a value. Only 'var' types are mutable, while 'val' can't be changed.

```
var running_total:number = 0;
val total = 25;
val pi:number = 3.14159;

fun circle_area(radius:number): number {	
	return pi * radius ^2;
}

fun ratio_of_area(circles:number, width:number, height:number): number {
	var min_dimension = width;
	if width > height {
		min_dimension := height;
	}

	var circle_ct = 0;
	while circles < total {
		val radius = random(min_dimension);
		running_total := running_total + circle_area(radius);				
		circle_ct := circle_ct + 1;
	}
	
	val avg_circle_area = running_total / circles;	
	val area = height * width;	
	val circle_pct = (avg_circle_area/ area) * 100;
	return circle_pct;	
}

val width = 50;
val height = 25;

val circles = total;
print "Average area inside one of " + 
	circle_ct + 
	" circles: " + 
	ratio_of_area(circle_s, width, height);

```

Parameters are 'val' declarations within the function body by default. If the function declaration needs to change parameters within the function but not let those changes take effect outside the function, use a 'cpy' declaration type. To allow a function to have a side-effect of altering arguments, use 'var'. 

In this example 'cpy' is a convenience and clarifying bit of syntax; you could do the same thing by passing in the 'text' variable by default 'val' type, then making a copy with "var copied_text = text;". 

```
fun  sort_words(var text: string): number {
	var words:array of string = split(text," ");
	sort!(words);	
	// Turn the array of word strings back into one string
	text := join(words," ");
	return length(words);
}

fun  new_sorted_string(cpy text: string): string {
	val total_words = sort_words(text);
	print "There are " + total_words + " in " + length(text) + " letters.";
	return text;
}

val new_str = new_sorted_string("The quick brown fox");

```








