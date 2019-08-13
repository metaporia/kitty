# kitty


## CSV format

Each line has three fields:

```csv
<sum>,<participants>,<payers>
```

The fields are as follows:
- amount payed (a shared expense),
- identifiers/names of those who partook of the item(s) purchased,
- identifiers/names of those who paid for the item(s).

It is assumed that all participants partook of the resource equally, and that
payers paid an equal share.


## Usage

Given `example.csv` with contents:

```csv
30,kl,k
120,akl,l
35,al,a
```

we can run the following to print a summary of debts:

```bash
 cat example.csv |  stack exec tallyKitty -- --stdin --names "a,l,k"
```

which produces:

```
a owes l: $40.0
k owes l: $40.0
l owes a: $17.5
l owes k: $15.0
```
