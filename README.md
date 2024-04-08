# bank-report-racket

## How to run
A default function call is already at the bottom of the program to run with `ACCOUNTS.TXT`, `TRANSACTIONS.TXT`, and `STATEMENTS.TXT`, so simply running the program will get a result.

To customize the input files, the function definition is `generate-report -> String String String Void` and takes arguments in the order listed above (i.e. `(generate-report "accounts.txt" "transactions.txt" "output.txt")`)

## Sources
### LLMs
I tried using ChatGPT 3.5 to help with some regex and Typed Racket details, but it was more or less completely useless. I don't believe I actually used anything from the prompts I asked it.

### Other
I didn't use code from any sources, but I did look at examples from StackOverflow, Reddit threads, and, of course, the Racket Documentation.

## Notes
I tried using Typed Racket for the first time on this project. I learned very quickly that it adds a lot of safey (a.k.a complexity). 

Because of this, a lot of the functions in this project are much larger and messier than I would have made them in untyped Racket.
