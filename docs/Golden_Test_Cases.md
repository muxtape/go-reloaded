# Go-reloaded Golden Test Cases Document

## Examples of program usage

1) Given a sample.txt that contains the following text:
    ```
    it (cap) was the best of times, it was the worst of times (up) , it was the age of wisdom, it was the age of foolishness (cap, 6) , it was the epoch of belief, it was the epoch of incredulity, it was the season of Light, it was the season of darkness, it was the spring of hope, IT WAS THE (low, 3) winter of despair.
    ```

    Running the following commands in the terminal:
    ```
    $ go run . sample.txt result.txt
    $ cat result.txt
    ```
    We will have the following output:
    ```
    It was the best of times, it was the worst of TIMES, it was the age of wisdom, It Was The Age Of Foolishness, it was the epoch of belief, it was the epoch of incredulity, it was the season of Light, it was the season of darkness, it was the spring of hope, it was the winter of despair.
    ```

2) Given a sample.txt that contains the following text:
    ```
    Simply add 42 (hex) and 10 (bin) and you will see the result is 68.
    ```
    Running the following commands in the terminal:
    ```
    $ go run . sample.txt result.txt
    $ cat result.txt
    ```
    We will have the following output:
    ```
    Simply add 66 and 2 and you will see the result is 68.
    ```

3) Given a sample.txt that contains the following text:
    ```
    There is no greater agony than bearing a untold story inside you.
    ```
    Running the following commands in the terminal:
    ```
    $ go run . sample.txt result.txt
    $ cat result.txt
    ```
    We will have the following output:
    ```
    There is no greater agony than bearing an untold story inside you.
    ```

4) Given a sample.txt that contains the following text:
    ```
    Punctuation tests are ... kinda boring ,what do you think ?
    ```
    Running the following commands in the terminal:
    ```
    $ go run . sample.txt result.txt
    $ cat result.txt
    ```
    We will have the following output:
    ```
    Punctuation tests are... kinda boring, what do you think?
    ```


## Examples of tricky input texts

1) `"This is a tricky sample (cap, 2) (low) text" -> "This is a Tricky sample text"`

2) `"This is a ' tricky (up) ' sample text" -> "This is a 'TRICKY' sample text"`

3) `"This is a A0 (hex) tricky sample text" -> "This is a 160 tricky sample text"`

4) `"This is a . (up) . . tricky sample text" -> "This is A... tricky sample text"`

5) `"This is a B (hex) (bin) tricky sample text" -> "This is a 3 tricky sample text"`