# LayawayX Smart Contract

A decentralized layaway system implemented in Clarity for the Stacks blockchain.

## Overview

LayawayX enables decentralized layaway purchases using STX tokens, allowing sellers to list items and buyers to make installment payments over time.

## Features

- **For Sellers:**
  - List items with price and deadline
  - Deactivate listings
  - Withdraw funds from failed layaways
  - Receive cancellation fees

- **For Buyers:**
  - Initiate layaway purchases
  - Make installment payments
  - Claim items when fully paid
  - Cancel layaway with 10% penalty fee

## Contract Functions

### Seller Functions
```clarity
(create-item (price uint) (deadline uint))
(deactivate-item (id uint))
(withdraw-failed (id uint))
```

### Buyer Functions
```clarity
(initiate-layaway (id uint))
(pay-installment (id uint) (amount uint))
(claim-item (id uint))
(cancel-layaway (id uint))
```

### Read-Only Functions
```clarity
(get-item (id uint))
(get-layaway (id uint))
(is-fully-paid? (id uint))
```
