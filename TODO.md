## TODOs

A list of features to be implemented in the language interpreter further down the road.

### Further Language Features (Suggested by the Book)

- [ ] **Multiline Comments**: Support for `/* ... */` style comments that can span multiple lines.
- [ ] **Comma Operator**: Allows multiple expressions to be evaluated in a single statement, returning the last expression's value.
- [ ] **Error for Binary Operator without left Operand**: Enforce syntax rules to prevent binary operators from being used without a left operand.
- [ ] **Ternary Operator**: A concise way to perform conditional expressions, e.g., `condition ? expr1 : expr2`.
- [ ] **Implicit Type Conversion**: Automatic conversion between types (e.g., string to number) during operations.
- [ ] **Runtime Error: Variable access before assignment**: Throw an error if a variable is accessed before it has been assigned a value (no implicit nil).
- [ ] **Introduce break and continue statements**: Add support for `break` and `continue` statements to control loop execution.
- [ ] **Add lambda functions**: Support for anonymous functions (lambdas) for more functional programming styles.
- [ ] **Add better resolver errors**: Improve error messages related to variable resolution for better debugging (self-reference, top-level return, unused variables, etc).

### Optional Language Features (Nice to Have)

- [ ] **Exponentiation Operator**: Support for the `**` operator to perform exponentiation, e.g., `2 ** 3` equals `8`.
- [ ] **Increment/Decrement Operators**: Support for `++` and `--` operators to increase or decrease a variable's value by one.
- [ ] **Arithmetic Assignment Operators**: Implement operators like `+=`, `-=`, `*=`, and `/=` for shorthand arithmetic operations.
- [ ] **Typesafe Equality Operators**: Implement `===` and `!==` for strict equality checks without type coercion.
- [ ] **Ternary Operator**: Implement the ternary conditional operator `? :` for concise conditional expressions.
- [ ] **Better Error Messages**: Improve error messages to be more descriptive and user-friendly.
- [ ] **Add Array and Tuple support**: Implement array and tuple data structures for more complex data handling.
