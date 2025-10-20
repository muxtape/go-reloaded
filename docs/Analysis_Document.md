# Go-reloaded Analysis Document

## General Description

The main function of the program is the transformation of the data of an input.txt file, to another set of data depending on a set of rules already present in the input.txt file, and then outputting the transformed data to an output.txt file.

Our program will receive as arguments the input.txt file and a string that will be the name of output.txt file.


## Data Transformation Ruleset

Our program will transform the data of the input file according to the following rules:

1) Wherever we find the `(hex)` instance inside the input.txt we should replace the word before that instance with the decimal representation of that word(that word will always be a hexadecimal number). `(Ex: "The last 14 (hex) days" -> "the last 20 days")`

2) Wherever we find the `(bin)` instance inside the input.txt we should replace the word before that instance with the decimal representation of that word(that word will always be a binary number). `(Ex:"After 101 (bin) long years" -> "After 5 long years")`

3) Wherever we find the `(up)` instance inside the input.txt we should replace the word before that instance with the Uppercase representation of that word. `(Ex:"Everybody, stop (up)!" -> "Everybody, STOP!")`

4) Wherever we find the `(low)` instance inside the input.txt we should replace the word before that instance with the Lowercase representation of that word. `(Ex:"Put the KOT (low) down, slowly." -> "Put the kot down, slowly.")`

5) Wherever we find the `(cap)` instance inside the input.txt we should replace the word before that instance with the capitalized representation of that word. `(Ex:"The fog was thick before entering kilkis (cap)." -> "The fog was thick before entering Kilkis.")`
    - The `(low)`, `(up)` and `(cap)` function instances can be accompanied by a number like `(up, <number>)`. In that instance we must apply the functionality of that function to the specified number of words preceding the function`(Ex:"Welcome to nea ionia (cap, 2)!" -> "Welcome to Nea Ionia!")`.

6) Wherever we find the following punctuations `.`, `,`, `!`, `?`, `:` and `;`, they should be close to the previous word and have a space to the next one.`(Ex:"Creativity depleted ,i must go home." -> "Creativity depleted, i must go home.")`
    - If we have groups of punctuations such as `...` or `!?`, then they should be close to the word before.`(Ex:"Beep ... The line closed abruptly." -> "Beep... The line closed abruptly.")`

7) Wherever we find the punctuation mark `'`, we must find the other instance of it and they should be placed to the left and to the right of the word they enclose without any spaces. `(Ex:"Do not look at me like ' that'." -> "Do not look at me like 'that'.")`
    - If there are multiple words between the two `'  '` marks, we should place the marks next to those words.`(Ex:"She said: ' Drop the act '!" -> "She said: 'Drop the act'!")`

8) Wherever we find the article `a`, we should modify to `an` if the following word begins with a vowel (`a`, `e`, `i`, `o`, `u`) or an `h`. `(Ex:"It was a amazing dinner." -> "It was an amazing dinner.")` 


## Pipeline vs Streaming Finite State Machine Architecture Models

Our problem can be written using either the Pipeline software architecture model or the streaming FSM(Finite State Machine) model. Each model has its own advantages and disadvantages.

The Pipeline approach breaks the problem into a series of independent and sequential steps. Each step performs a transformation on the data according to the ruleset, and passes it to the next step.

The Streaming FSM approach processes the input token by token, using a state machine to decide what to do as data flows in. It doesn't need to load the whole file, as it maintains state to apply the transformations when commands are encountered.

| Feature                   |  **Pipeline**                              |  **Streaming FSM**                                    |
| ------------------------- | ------------------------------------------ | ----------------------------------------------------- |
| **Processing style**      | Batch / staged                             | Incremental / token by token                          |
| **Structure**             | Sequential filters                         | State machine (Reading / Applying / etc.)             |
| **Data handling**         | Works on entire dataset at each stage      | Processes as data flows in                            |
| **Memory usage**          | Needs to hold all tokens                   | Keeps only recent tokens                              |
| **Speed for large files** | Slower — multiple passes                   | Faster — single pass, streaming                       |
| **Complexity**            | Simple, modular                            | More complex — requires state design                  |
| **Best for**              | Smaller datasets, clear step-by-step tasks | Continuous streams, large files, real-time transforms |


I will be experimenting with the streaming FSM architecture because it can handle all sizes of input files even though it can become quite complex in its design if we need to implement extra transformation functions down the road.