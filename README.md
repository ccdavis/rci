# rci

Building a statically typed language using Crafting Interpreters as a launching pad. 

Originally the language somewhat resembled "Lox" from the book, but with explicit types and user definable types.

This branch removes the interpreter.

The main idea behind RCI is to try out some familiar language features combined in new ways. The target lies somewhere between ___Go___, ___Python___ and ___Nim___ with some elements of ___Object Pascal___.


### State of the Project

A compiler with C as the target language now supports most of the interpreted language.  The parameter passing semantics are properly modeled with good compiler errors to explain this. Enum types and arrays are implemented; records ("structs") are a work in progress.

On my PC, the Mandelbrot sample program compiles and runs with TCC or GCC as the C-compiler. With TCC the benchmark that took 162 seconds to run on the interpreter took 9 seconds. With no optimizations it runs in 4.5 seconds with GCC, and 0.5 seconds with -O3 optimization level.

## Current Language Proposal

### Features and Syntax

A program consists of declarations and imperatives. Functions, variables and types can be declared in a program followed by a block of statements (the imparitives using those declarations.)

Type names must begin with upper-case. Variable names and function names must begin with lower-case.

Lists in declarations are surrounded by  `{`, `}` separated by commas or newlines for values, or `;` or newlines for statements (in function definitions.)


```scala
// Values in declarations can be separated by commas or newlines
type WeekendDays = Enum { Saturday, Sunday }

// Newlines can replace commas
type WeekDays = Enum {
	Monday
	Tuesday
	Wednesday
	Thursday
	Friday
}

// You can specify a string conversion for an enum value
type Days = Enum { 
	Monday: 	"Mon" 
	Tuesday: 	"Tue"
	Wednesday: 	"Wed"
	Thursday: 	"Thu"
	Friday: 	"Fri"
	Saturday: 	"Sat"
	Sunday: 	"Sun"
}

type Color = Rec { red: Int, blue: Int, green: Int}

type Customer = Rec {
	name: Str
	id: Int
	phone: Str	
	balance: Int
	reminder: WeekDay
}

// Simple functions can be one-liners:
fun display(cust: Customer) = print name,": ",reminder.str()

// Functions can be associated with Rec types
// A 'Customer' instance is available as 'this'
fun Customer.reward(this,day): Bool = {
	return day = this.reminder and this.balance > 250 
}

// The customer instance (this) can't be changed
fun Customer.notify(this, today: WeekDay): Bool = {	
	val success = text(cust.phone, "Congratulations, you got", 5, "rewards points!")			
	if not success {
		log(cust.name, "contact failed.")
	}
	return success
}

//   You must name any function that mutates its associated record data with "!".
// The 'this' parameter must also have the "var"  modifier as usual to allow mutation.
fun Customer.apply_reward!(var this, today: WeekDay): Bool = {
	if this.reward(today) {
		this.balance := balance +  5
		print Notified", display(this),": ",this.notify(today) ,"\n"					
		return true
	} else {
		return false
	}	
}

{
	// Every Rec has a default constructor
	var cust = Customer(name: "John Smith", id: 123, phone: "555-5555", balance: 0)
	val today = WeekDays.Wednesday
	cust.apply_rewards!(today)

}


// You can associate functions with built-in or existing types in the same way, but
// can't replace existing associated functions.
function Int.is_even(this): bool = return this % 2 = 0


function Str.words(this): Int = {
	val chars = this.len()
	var spaces = 0
	// only works on ASCII strings
	while c < chars {
		if " " = this[c] {
			spaces := spaces + 1
		}
	}
	return spaces + 1
}
```


#### Matching

```
type Metal = Enum { copper, gold, silver,  platinum, lead, unknown}

fun metal_test(mass: Grams, volume: Cm3): Metal= {	
	when mass /volume is
		8.94..8.96 then { return copper}
		19.28..19.31 then { return gold}
		21.45..21.53 then { return platinum}
		11.28..11.32 then { return lead}
		10.4..10.6 then  { return silver }
		else { return unknown }	
}

fun Metal.color(this): Color= {
	// If the 'when' is exhaustive you don't need 'else'; the 
	// compiler should produce an error if you don't cover all cases of an enum.
	// All other types require 'else'.
	when this is 
		gold then { return yellow}
		Metal.silver then { return Color.silver }
		// Qualify enum values with their type to disambiguate 
		platinum then { return Color.silver }
		lead then { return Color.silver }
		copper then { return orange }
}


```

### Design

* Variable declarations with 'var' and 'val' types as in Scala: 'val' types are immutable.
* Static typing with reasonable type inference during variable declaration and initialization.
* Function parameters default to 'val' type, but can be 'var' if specified in the definition, or 'cpy' if the argument value needs to be mutable but cause no side-effects. Only 'cpy' causes pass-by-value, only 'var' can alter the passed-in value. Expressions may be passed either as 'val' or 'cpy', only named variables can be passed as 'var'.
* Function overloading both with arity and parameter types. This can occasionally make hard to debug code but allows for interesting types of polymorphism without  dynamic dispatch or inheritance.
* Range types for ordinals (Int..Int) is the literal. It is a special case of Set types with all the set operations  supported.
* For loops operate on ranges and sets.
* Actual newtype types created by a 'type' declaration. This is different from 'type' in ___Rust___ or 'typedef' in ___C___ which simply aliases the type name. To do this well the language has to allow common operators to work on directly derived types but prevent automatic operation between say 'kilometers' and 'int64' -- but allow them when explicit casts (but only if the newtype derives from the exact same built-in type.)
* Enumeration types with ordering and name assignments
* Set type, along with dynamic arrays and hashtables.
* Arrays, like tables have an index type: data: array<Integer, String>. In most languages the type of the index is implied but is some scalar type. Allow enumerations or sets of enumeration types to be the index type of an array. This way, all "look-up" types have the same calling conventions and same declaration syntax.
* Set operators on all collection types
* Pattern matching case statements
* Record types that ensure field order and always support serialization
* Table types that always support sorting, serialization and a few key functional-style methods like 'map', 'select', 'filter', 'reduce'. This is not a Pandas clone. The main difference between a 'Table' type and an array of Record instances is that a 'Table' is guaranteed to contain Record type data. It is such a common use-case that it deserves its own type: It can represent result sets from a database query, a CSV or Parquet file or JSON objects with a consistent schema. Ideally table data could be stored with an efficient memory layout like Arrow.
* No inheritance for table and record types. This helps to keep the mismatch between relational data (in a database) and in the program smaller.
* Heap allocations kept to a minimum. When needed, they should be handled with reference counting, or a garbage collector as an option to avoid the cycle problem of ref-counting.





### Language to-do list:

* A few more basic statements like 'for, 'map', 'filter'.
* Support chaining functions associated with types
* File I/O for lines of text (strings) and directly to and from other built-in types
* containers: Maps, arrays with typed indices
* Table types made of records supporting special table-like operations (think Pandas-light)


### Engineering to-do list

* Garbage collector
* (mostly done) Clean up the error message types by consolidating into one main error type. This would remove the need for re-enclosing messages into different types.
* (mostly done) Clean up error reporting to always include the location in the source; the capability exists, I was just lazy about crafting the error messages in places.
* Add a basic module system to load libraries.
* Expand standard library
* Look for back-end (tcc, gcc, linkers) and organize intermediate and binary output files (mostly done)
* Support creating a project work area, like ___Rust's___ `cargo new` and doing builds in that area
* Auto formatter
* Compile to bytecode


## Performance

### Compiler

RCI compiles to C. With `tcc` as a target you get pretty fast binaries and lightening fast compilation times. Everything compiles as one C compilation unit, so `gcc` is pretty fast also.

Running time is something like this on my PC:

Using the `mandelbrot`  benchmark at 1600 x 1600 and 50 iterations.

tcc: 9 seconds
gcc: 0.5 seconds

This isn't too different from ___Nim___ or ___Free Pascal___. The `gcc` optimizer is doing a lot of heavy lifting.


### Original Interpreter 

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
type Name = data-type;
```

#### Records

```
type My_data = Rec  {
	field1_name: data-type
	field2_name:  data-type
	...
}
```

#### Arrays
```
type My_dataset_type = array<my_data>
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



