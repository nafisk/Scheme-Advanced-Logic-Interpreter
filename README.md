# Scheme List Interpreter

The Scheme List Interpreter is a comprehensive package designed for handling propositional logic using Scheme's functions, syntax, and primitives. It is particularly adept at dealing with propositional expressions that need simplification and boolean evaluation.

## Features

- **Propositional Logic Handling**: Extensively utilizes lists to handle complex propositional expressions.
- **Simplification**: Capable of simplifying expressions into fundamental logical operators.
- **Boolean Evaluation**: Evaluates expressions based on given boolean values.
- **Support for Various Input Formats**: Works with both concrete and abstract representation of inputs.
- **In-depth Logical Operations**: Includes operations like AND, OR, NOT, and IMPLIES.

## How to Run

1. **Prerequisites**: Ensure that you have a Scheme interpreter installed on your system.

2. **Installation**: Clone the repository or download the source code of the Scheme List Interpreter.

    ```
    git clone https://github.com/nafisk/Scheme-List-Interpreter
    ```

3. **Running the Interpreter**:
   
   - Open the Scheme interpreter on your system.
   - Load the source file of the interpreter into your Scheme environment.

    ```scheme
    (load "path-to-scheme-list-interpreter.scm")
    ```

4. **Usage**:

    - The interpreter can be invoked with the function `interpreter`, which takes two arguments: a propositional statement and an association list of boolean values for variables.
    
    ```scheme
    (interpreter 'your-propositional-statement '((variable1 boolean-value1) (variable2 boolean-value2) ...))
    ```

    - Replace `'your-propositional-statement'` with your propositional logic statement and the association list with your variable-boolean value pairs.

5. **Example**:

    ```scheme
    (interpreter '(x v y) '((x #f) (y #t)))
    ```

    This command will evaluate the expression `(x v y)` with the values `x = false` and `y = true`.

## Testing

The package includes a series of tests to ensure the reliability and correctness of the interpretations. Users can run these tests to verify the functionality of the interpreter.

## Contributors

- Nafis Khan (Email: nkhan014@citymail.cuny.edu)
- Deepankar Chakraborty (Email: dchakra001@citymail.cuny.edu)

---

Note: This README provides a basic guide for the Scheme List Interpreter. For detailed documentation, refer to the comments and documentation within the source code. 

© 2023 Nafis Khan. All Rights Reserved.
