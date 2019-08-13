# kitty

Parse a ledger of shared expenses and summarize the resultant individual debts;
in essence, calculate how much each party owes each other party based on
entries describing group purchases. 

This was designed to facilitate low-overhead sharing of cash, credit/debit
cards, etc. when travelling with friends, so that whoever possesses the most
liquid/available purchasing power can apply it without fear of either losing
money to the whimsical sense duty of their buddies or having to maintain, e.g.,
a (more) complex spread-sheet based solution.

As yet there is no means of specifying neither the per-party proportions
for payment nor enjoyment/consumption of purchased goods. See the below field
specification for details. It's likely that proportions will be supported for
both payment and consumption in the near future.

## Installation

Clone and install with [stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/<Paste>):

```bash
git clone https://github.com/metaporia/kitty && cd kitty && stack install
```

N.B. this was tested only on Arch Linux.

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

we can run the following to print a summary of debts between three parties,
nicknamed `a`, `l`, and `k`:

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
