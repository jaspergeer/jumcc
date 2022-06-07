# jumcc
`jumcc` is a compiler for um-C, a small C-like language I created, that targets a toy assembly language detailed [here](https://www.cs.tufts.edu/comp/40-2011f/handouts/umasm.html) which is used to teach assembly programming to students in Tufts University's CS40 course.

## Installation
To compile `jumcc` from source, install [Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/).

Then, change to the `jumcc` source directory and and issue the following commands:
```
stack setup
stack install
```
`stack setup` will download the ghc compiler if you don't have it and `stack install` will install the `jumcc` executable to `~\.local\bin` which you should add to your `PATH`.

## Usage
By default, `jumcc` takes the name of a um-C source file `[name].umc` and creates a file called `[name].ums` to which it sends output.
```
jumcc src.umc
```
To specify an output file name, use the -o option.
```
jumcc -o out.ums src.umc
```
To generate an executable `.um` binary, the resulting `.ums` file must be linked with `umcrtn.ums` and `umcrt1.ums`, which are found in the `umcrt` directory, using the UM Macro Assembler `umasm` program (as far as I know this is only available to those with a Tufts EECS account). I have also supplied a toy standard library in the `stdlib` directory that you may compile which implements simple versions of `puts` and `gets`.
```
umasm umcrtn.ums stdlib.ums [your-file].ums ... umcrt1.ums
```
I've included two sample programs in the `sample` directory that you can compile and assemble/link. Both depend on `stdlib.umc`

## um-C
um-C is nearly a strict subset of C with a few minor differences. However, it should be relatively easy to modify a um-C program to be compiled by gcc or clang. I haven't written a grammar yet but here is a general overview of the language:

There are 2 primitive types: `char` and `int`. Both are unsigned and stored in memory as 32-bit values but `char` values are truncated to between 0 and 255 when accessed. These can be declared like so:
```
int a;
int b = 1 + 2;
char c;
char d = 'a' + b;
```

um-C supports 1-dimensional arrays of primitive types which can be declared like so:

```
int[10] a;
char[5] str = "four";
int[3] ints = {1, 2, 3};
```
The size of an array must be specified with an integer constant.

`jumcc` does not include comprehensive type checking, so it is imperative when declaring and initializing arrays that the number of elements is the greater than or equal to the declared array size (in the case of brace-enclosed lists) or the number of 
characters + 1 (in the case of a string literal).

Pointers are also supported, and a variable can have a type of pointer to pointer, pointer to array, or pointer to primitive type. They are declared like so:
```
int *a;
char *b;
int *c[];
```

Programs consist of a series of function definitions, written as in C, each of which consist of a type, identifier, and parameters like so:
```
int func(int a) {
    return a;
}
```
Functions must terminate with a `return` statement. The compiler does not enforce this but failing to do so will result in unintended behavior.

The `main` function is the entry point into a program, and there can be only one across all the `.umc` files you intend to compile and link together to create your program.
```
int main() {
    ...
    return 0;
}
```

Unlike C, function declarations are not necessary or permitted, but the number and type of arguments are not checked. Be careful!

Functions contain a series of statements. They can be variable declarations as seen before or:

Function calls:
```
puts("hello world");
sum(1, 2);
gets(str, 10);
```

Assignment:
```
*str = 'a';
arr[5] = (10 + b);
```

Return:
```
return 100;
return b[5];
```
Or one of the two control flow structures in um-C:

While loops:
```
while (x > 10) {
    x = x - 1;
    puts("thats crazy\n");
}
```
If statments
```
if (x == 10) {
    puts("x is 10\n");
}
```

The `outb` I/O primitive
```
outb('a');
```

Available for use in expressions are the following:

The `inb` I/O primitive
```
char c = inb();
```
Function calls
```
int a = sum(1, 2) + 3;
```

Relops `1`, `<=`, `>`, `>=`, `!=`, `==`, `&&`, `||`. These all function the same way as they do in C. Note that compound expressions on either side of a `&&` or `||` operator must be in parentheses
```
char tru = 1 && 1;
```

Binops `+`, `-`, `*`, `/`, `&`, `|`,  `^`, `%`.
```
int inplusone = inb() + 1;
```

Unary (prefix) operators `-`, `!`, `~`, `\*`, `&`
```
*(str + 3) = *(str + 2);
```
Note that only pointer and array type values may be dereferenced.

Unary (postfix) operator `[]` used for array access.
```
str[3] = str[2];
```

## Notes
* Initializing arrays of size greater than ~3000 can cause extremely long compile times.
* The default stack size for a um-C program is 100000 32-bit words, but you can increase this by changing the value in umcrtn.ums

## Built With
* [Haskell](https://www.haskell.org/)
* [parsec](https://hackage.haskell.org/package/parsec)
* [VSCode](https://code.visualstudio.com)

## Contributors
* [Jasper Geer](https://github.com/jaspergeer)

## License
MIT (c) Jasper Geer
