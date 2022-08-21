# Parser for WhatsApp chat dumps

```
cabal build
{program_loc} < chat_file.txt > chat_output.csv
```
Written as an exercise in haskell.

Assumes

- No non-human sent messages contain :
- Group member names do not contain :
