# rci

Building a statically typed language using Crafting Interpreters as a launching pad. 

The language somewhat resembles "Lox" from the book, but with explicit types and user definable types.

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

###Engineering to-do list:

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
 
Language:
============

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








