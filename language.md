

```
type UserId = Int
type User = Rec ( id: UserId, first_name: Str, last_name: Str, )

//  Default first arg named 'this', the decl type is 'val' -- immutable
fun User.print() {
	print this.id,":  ",this.last_name,", ", this.first_name
}

// The '!' makes the decl type of 'this' a 'var' (mutable.  )
fun User.update_surname!(new_surname: Str): bool {
	if new_surname == this.last_name {
		return false
	} else {
		this.last_name := new_surmane	
		return true
	}
}



type Days = Enum(Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday)
fun Days.happy() -> Str {
	return "Happy " + this.to_str()  + "!"
}

// Tuples
type Color = Tuple(Int, Int, Int)

// You can add type-bound functions to existing built-in types:
fun Flt.abs(): Flt {
	if this < 0 {
		return  this * -1
	} else {
		return this
	}
}

fun Int.power(exp: Flt) -> Flt {
	if exp < 0 {	
		return 1 / this.pow(exp.abs())
	}
	
	if exp == 0 {
		return 1
	}
	
	if exp == 1 {
		return this
	} else {
		return this *  this.power(exp - 1)
	}	
}


// Literals

var u: User = User(id: 123, first_name: "John", last_name: "Smith")
val background: Color = Color(0,0,0)

val weekend: Days = Saturday
val start_work = Days.Monday



```