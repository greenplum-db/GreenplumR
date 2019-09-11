# GreenplumR coding style
This coding style is partially similar with [linux kernel coding style](https://www.kernel.org/doc/html/v4.10/process/coding-style.html)
### Indentation
All indentations are 4 spaces.
### Breaking long lines and strings
The limit on the length of lines is 80 columns and this is a strongly preferred limit.
### Braces and Spaces
Put the opening brace last on the line. The exception is the top level function. Note the opening brace of a nested function puts the opening brace last on the line.
### Spaces
* Use a space after these keywords:
  > `if, while, for, return`
* Use one space around binary operators, such as
  > `+ - * / = < > >= <= == != <- -> <<-`
* No space between the function name and its open parentheses, such as
  > `a <- b (2)` is bad
* No space around(inside) parenthesized expression, such as
  > `( a )` is bad
* No space between the expression and the next comma, such as
  > `a <- list(a , b)` is bad
### Commenting
The sharp sign(#) should be aligned to the following block, such as
```r
    # This is a comment
    if (a) {
        ....
    }
```
Put one space after the sharp sign, such as
  > `#This is a comment` is bad
### Quote
Single quoted string or double quoted string are both OK. But you can't mix two style in one line.
### Naming

