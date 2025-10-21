# Go-reloaded Golden Test Cases Document

## Examples of program usage

1) Given a sample.txt that contains the following text:
    ```
    If I make you BREAKFAST IN BED (low, 3) just say thank you instead of: how (cap) did you get in my house (up, 2) ?
    ```

    Running the following commands in the terminal:
    ```
    $ go run . sample.txt result.txt
    $ cat result.txt
    ```
    We will have the following output:
    ```
    If I make you breakfast in bed just say thank you instead of: How did you get in MY HOUSE?
    ```

2) Given a sample.txt that contains the following text:
    ```
    I have to pack 101 (bin) outfits. Packed 1a (hex) just to be sure
    ```
    Running the following commands in the terminal:
    ```
    $ go run . sample.txt result.txt
    $ cat result.txt
    ```
    We will have the following output:
    ```
    I have to pack 5 outfits. Packed 26 just to be sure
    ```

3) Given a sample.txt that contains the following text:
    ```
    Don not be sad ,because sad backwards is das . And das not good
    ```
    Running the following commands in the terminal:
    ```
    $ go run . sample.txt result.txt
    $ cat result.txt
    ```
    We will have the following output:
    ```
    Don not be sad, because sad backwards is das. And das not good
    ```

4) Given a sample.txt that contains the following text:
    ```
    harold wilson (cap, 2) : ' I am a optimist ,but a optimist who carries a raincoat . '
    ```
    Running the following commands in the terminal:
    ```
    $ go run . sample.txt result.txt
    $ cat result.txt
    ```
    We will have the following output:
    ```
    Harold Wilson: 'I am an optimist, but an optimist who carries a raincoat.'
    ```


## Examples of tricky input texts

1) `"This is a tricky sample (cap, 2) (low) text" -> "This is a Tricky sample text"`

2) `"This is a ' tricky (up) ' sample text" -> "This is a 'TRICKY' sample text"`

3) `"This is a A0 (hex) tricky sample text" -> "This is a 160 tricky sample text"`

4) `"This is a . (up) . . tricky sample text" -> "This is A... tricky sample text"`

5) `"This is a B (hex) (bin) tricky sample text" -> "This is a 3 tricky sample text"`


## Example of a bigger text

The input text:
```
"Lorem ipsum dolor (up, 2) sit amet, consectetur adipiscing elit, sed do eiusmod (cap), 11 (bin) tempor incididunt ut labore et dolore magna aliqua. Ut 'enim (up)' ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea ' commodo consequat'. Duis (low) aute irure dolor in reprehenderit in voluptate B (hex) velit esse cillum dolore eu fugiat nulla pariatur. . . Excepteur sint a occaecat cupidatat non proident, sunt in (cap, 3) (low, 2) culpa qui officia deserunt mollit anim id est laborum."
```
The output text:
```
"Lorem IPSUM DOLOR sit amet, consectetur adipiscing elit, sed do Eiusmod, 3 tempor incididunt ut labore et dolore magna aliqua. Ut 'ENIM' ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea 'commodo consequat'. duis aute irure dolor in reprehenderit in voluptate 11 velit esse cillum dolore eu fugiat nulla pariatur... Excepteur sint an occaecat cupidatat non Proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
```

