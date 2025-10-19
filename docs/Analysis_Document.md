# Go-reloaded Analysis Document

## General Description
The general idea of the program is the transformation of the data of an input.txt file, to another set of data depending on a set of rules, and then outputting the transformed data to an putput.txt file.

Our program will recieve as arguments the input.txt file and a string that will be the name of output.txt file.


## Data Transformation Ruleset

Our program will transform the data of the input file according to the following rules:

1) Wherever we find the ``` (hex) ``` instance inside the input.txt we should replace the word before that instance with the decimal representation of that word(that word will always be a hexadecimal number). `(Ex: "The last 14 (hex) days" -> "the last 20 days")`

2) Wherever we find the ``` (bin) ``` instance inside the input.txt we should replace the word before that instance with the decimal representation of that word(that word will always be a binary number). `(Ex:"After 101 (bin) long years" -> "After 5 long years")`

3) Wherever we find the ``` (up) ``` instance inside the input.txt we should replace the word before that instance with the Uppercase representation of that word. `(Ex:"Everybody, stop (up)!" -> "Everybody, STOP!")`

4) Wherever we find the ``` (low) ``` instance inside the input.txt we should replace the word before that instance with the Lowercase representation of that word. `(Ex:"Put the KOT (low) down, slowly." -> "Put the kot down, slowly.")`

5) Wherever we find the ``` (cap) ``` instance inside the input.txt we should replace the word before that instance with the capitalized representation of that word. `(Ex:"The fog was thick before kilkis (cap)." -> "The fog was thick before Kilkis.")`

6) Wherever we find the following punctuations ```.```, ```,```, ```!```, ```?```, ```:``` and ```;```, they should be close to the previous word and have a space to the next one.`(Ex:"Creativity depleted ,i must go home." -> "Creativity depleted, i must go home.")`
    - If we have groups of punctuations such as ```...``` or ```!?```, then they should be close to the word before.`(Ex:"Beep ... The line closed abruptly." -> "Beep... The line closed abruptly.")`

7) Wherever we find the punctuation mark ```'```, we must find the other instance of it and they should be placed to the left and to the right of the word they enclose without any spaces. `(Ex:"Do not look at me like ' that'." -> "Do not look at me like 'that'.")`
    - If there are multiple words between the two ```'  '``` marks, we should place the marks next to those words.(Ex:"She said: ' Drop the act ')