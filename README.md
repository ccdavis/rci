# rci

Building a statically typed language using Crafting Interpreters as a launching pad. 

The main idea behind RCI is to try out some familiar language features combined in new ways. The target lies somewhere between ___Go___, ___Python___ and ___Nim___ with some elements of ___Object Pascal___. The executables produced are intended to run fast, on par with ___Nim___ or ___Crystal___. As of now the compiler targets C89, but I'm planning to add a QBE target as well, and maybe an interpreted byte code target to make for a better REPL.

## State of the Project

### Near term engineering to-do list

* Improve error types (make them idiomatic Rust style) and capturing error messages
* Improve error reporting by adding source code with a pointer in the error output
* Improve build system to allow module imports and build more than one source file at a time
* Redo the code generation with an actual code generation library --It should be mostly SSA form.
* Enhance the standard library with some file support

### Near term language to-do list

* Add more built in types: Int, Flt
* Nail down type coercion -- remove most of it.
* Add some basic string functions to stdlib
* Associate functions with Rec or Enum types

### Latest language updates

* Modules and the beginning of a standard library in the 'std' module

Use `module module-name { ... }` to define them, use `module_name@name` to access things declared in a module. You can access functions, types and variables. Right now modules are primarily a name-spacing mechanism and there's no import system but that's coming.

* Colorized and cleaned up compiler output
* A minimal program builder: Within the repo you can do a "cargo run ./samples/program-name" and you'll get a binary in the directory where the source was. It has to be in the repo's directory tree for now. 

Currently working on full build system for (1) installing the compiler in the user's directory and (2) single-file local builds or project builds with config files (like Cargo.toml) and multi-file, multi-module builds.

Also currently WIP: String versions of enum values, enums and recs as member types of ___Rec___ These will work sort of like Python's StrEnums or IntEnums (but not using inheritance.)


### Modules example:

```scala
module math {
	fun square(x: Num): Num {
		return x * x
	}
	
	val pi = 3.14159
	
	fun circle_area(r: Num): Num {
		return pi * square(r)
	}
}

val x = 5
val x_squared = math@square(x)
val area = math@circle_area(x)

```


### Building

Programs build with ___tcc___ as the default linker. The intermediate target language is C89. You can build the compiler output with ___gcc___ as well for more optimized executables. The C compiler target includes a small runtime that's under development.

### Sample Code

Sorry this isn't more inspiring. I need to add some metaprogramming (planned) and standard library functions to make it exciting.

```scala
type Colors = { Red, Blue, Green}
type Days = {Mon, Tue, Wed, Thu, Fri, Sat, Sun}

fun daily_discount(day: Days): Num {
	if day = Wed { return 0.95 } else
	if day = Fri { return 0.90 } else
	if day = Mon { return 0.95} else
		return 1.0
}

type Customer = Rec {
	name: Str
	balance: Num,
	limit: Num,
	rewards: Bool,
}

fun apply_discount(cust: Customer, price: Num, day: Days): Num {
	if cust.rewards {
		return price * daily_discount(day)
	} else {
		return price
	}
}

fun charge(var cust: Customer, day: Days, price: Num):Bool{
	val new_charge = apply_discount(cust, day, price)
	if not new_charge + cust.balance > cust.balance {
		cust.balance := cust.balance + new_charge
		return true
	} else {
		return false
	}
}

// main
{
	var cust1 = Customer(name: "John Smith", balance: 0.0, limit: 500.0, rewards: false)
	var cust2 = Customer(name: "Jane Doe", balance: 5.0, limit: 800.0, rewards: true)
	
	print "charged: ",cust1.name," ",charge(cust1,Fri, 25.0)
	print "Charged",cust2,cust2.name," ", charge(cust2, Sat, 800.0)
}
	
```

See below for full description of the language's syntax and future plans.


### Performance 

On my PC, the Mandelbrot sample program compiles and runs with TCC or GCC as the C-compiler. With TCC the benchmark that took 162 seconds to run on the interpreter took 9 seconds. With no optimizations it runs in 4.5 seconds with GCC, and 0.5 seconds with -O3 optimization level.

### History 

The interpreter has been removed; it was a tree-walk style that was hard to maintain in parallel with the compiler. I may add in a byte code target at some point as a learning exercise.

Originally the language somewhat resembled "Lox" from the book, but with explicit types and user definable types.  I followed along with the book and implemented the first interpreter in ___Rust___. After getting the interpreter running I added static types and made substantial changes to the language. Building the second interpreter -- the byte code compiler and VM -- looked quite challenging with ___Rust___, at least if you want good performance. Instead of implementing it I chose to build a regular compiler.


## Current Language Proposal

### Features and Syntax Currently Implemented

* A program consists of declarations and imperatives. Functions, variables and types can be declared in a program followed by a block of statements (the imparitives using those declarations.)
* Type names must begin with upper-case. Variable names and function names must begin with lower-case.
* Lists in declarations are surrounded by  `{`, `}` separated by commas or newlines for values, or `;` or newlines for statements (in function definitions.)
* Logical operators are 'and' and 'not' and 'or'
* functions
* Parameters are passed as immutable by default ('val') Add 'var' for mutability In either case variables are passed by reference; use 'cpy' to pass by value.
* Control flow: "IF" and "WHILE", no for-loops yet.
* Simple enumeration types
* Rec types (like structs) implemented except cannot take Enum types or other Rec types as member types yet
* Fixed-length arrays

###  Features in development

Add a few more fundamental types: Int, Flt (currently both in Num), Chr and Byte. Add a bit more string functionality to the standard lib. Also add a few basic I/O features like standard in and files.This stuff isn't hard, I've just been putting it off.

WIP to add string versions of enums:

```scala

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
```

Set types.
```scala
type  Weekdays = Set<Day> = {Mon, Tue, Wed, Thu, Fri}
```

And runtime sets:
```scala
{
	var days_in_office = Set<Day>(Mon, Wed, Fri)
	
	// If it's a var, we can change it
	days_in_office.add!(Tue)
	val days_at_home = days_in_office - weekdays
}

```

Unions:

```scala
type RuntimeError = Rec { col: Num, ln: Num, msg: Str }

fun err(msg: Str): RuntimeError {
	return RuntimeError(col: FILE.column, ln: FILE.line, msg: msg)
}

type LoginResult  = Union { ok: Bool, error: RuntimeError } 

fun login(user: Str, pwd: Str): LoginResult {
	if login_service.connected {
		return login_service.accept(user,pwd) 
	} else {
		return err("Can't connect, login impossible.")
	}
}
```

Special Result type generator based on unions for error handling:
```scala
fun login(username: Str, pwd: Str): Result<ok: Bool, error: RuntimeError> {
	....
}


```

More user definable types -- allow all types .
```scala
type Meters = Int
type Email = Str
type Line = Array<Int,Chr>(65)
```

Maps ("lookups") A map will take an key / index type and a value type as usual. What's a bit different here is that the "Array" type also takes an index type. If you use an enum type as the index the Array is automatically sized and you can use it as a fast version of a map.  If you only partially initialize an array such as this you get errors when retrieving unset values / locations, but these errors can be caught before happening with ___contains()___.

```scala
type Color = Enum{ Red, Blue, White, Yellow, Green, Orange, Black, Gray}
type ColorCodes = Array<Color, Str>
type FavoriteColors = Map<Str, Color>


{
	var codes = ColorCodes(Red: "ff0000", Green: "00ff00", Blue: "0000ff")
	var favs = FavoriteColors("John": Gray, "Susan": Yellow)
	
	favs["Mark"] := Black
	val b_code = codes[Black]
	val y_code = codes[favs["Susan"]]
}
```
One-liner function declarations

```scala
// Simple functions can be one-liners:
fun display(cust: Customer) = print name,": ",reminder.str()
```

Associate functions with ___Rec___ types, similar to ___Rust___ struct impls.

```scala
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
```
Associate functions to existing types.
```scala

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

Matching

```scala
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
* Static typing with reasonable type inference during variable declaration and initialization. An initializer is required, you can't have a bare declaration.
* Function parameters default to 'val' type, but can be 'var' if specified in the definition, or 'cpy' if the argument value needs to be mutable but cause no side-effects. Only 'cpy' causes pass-by-value, only 'var' can alter the passed-in value. Expressions may be passed either as 'val' or 'cpy', only named variables can be passed as 'var'.
* Function overloading both with arity and parameter types. This can occasionally make hard to debug code but allows for interesting types of polymorphism without  dynamic dispatch or inheritance.
* Range types for ordinals (Int..Int) is the literal. It is a special case of Set types with all the set operations  supported.
* For loops operate on ranges and sets.
* Actual newtype types created by a 'type' declaration. This is different from 'type' in ___Rust___ or 'typedef' in ___C___ which simply aliases the type name. To do this well the language has to allow common operators to work on directly derived types but prevent automatic operation between say 'kilometers' and 'int64' -- but allow them when explicit casts (but only if the newtype derives from the exact same built-in type.) Getting this right may be difficult.
* Enumeration types with ordering and name assignments
* Set type, along with dynamic arrays and hashtables.
* Arrays, like tables have an index type: data: array<Integer, String>. In most languages the type of the index is implied but is some scalar type. Allow enumerations or sets of enumeration types to be the index type of an array. This way, all "look-up" types have the same calling conventions and same declaration syntax.
* Set operators on all collection types
* Pattern matching case statements
* Record types that ensure field order and always support serialization
* Table types that always support sorting, serialization and a few key functional-style methods like 'map', 'select', 'filter', 'reduce'. This is not a Pandas clone. The main difference between a 'Table' type and an array of Record instances is that a 'Table' is guaranteed to contain Record type data. It is such a common use-case that it deserves its own type: It can represent result sets from a database query, a CSV or Parquet file or JSON objects with a consistent schema. Ideally table data could be stored with an efficient memory layout like Arrow.
* No inheritance for table and record types. This helps to keep the mismatch between relational data (in a database) and in the program smaller.
* Automatic deep copies with a 'cpy' unary operator; the default is immutable pass-by-reference
* GC memory management


### Some more ideas

* A few more basic statements like 'for, 'map', 'filter'.
* Support chaining functions associated with types. This should mostly come for free.
* File I/O for lines of text (strings) and directly to and from other built-in types
* Table types made of records supporting special table-like operations (think Pandas-light)


### Engineering to-do list

* Garbage collector
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

The point of using 'val' as the default parameter type is to guide the program writer to a more efficient and functional style. A good compiler for the language should optimize 'val' parameters to behave like const reference parameters. The 'cpy' parameter passing style should be explicit in cases where the values are large.  Possibly it makes sense to make number and boolean types 'cpy' by default since there's no overhead in passing them by value; then again having a set of immutable variables  as arguments may lead to fewer mistakes.

