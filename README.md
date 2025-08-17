# ⚖️ Justicebit - Judicial Escrow Smart Contract

A decentralized escrow system with built-in dispute resolution mechanism on the Stacks blockchain. Lock tokens safely and resolve disputes through community-driven judicial voting.

## 🌟 Features

- 🔒 **Secure Escrow**: Lock STX tokens between buyers and sellers
- ⚖️ **Dispute Resolution**: On-chain judicial system for conflict resolution
- 👨‍⚖️ **Judge Network**: Stake-based judge registration and voting
- 🎯 **Reputation System**: Track judge performance and accuracy
- ⏰ **Time-based Expiry**: Automatic refund mechanism for expired escrows
- 💰 **Reward Distribution**: Incentivize judges with dispute resolution fees

## 🚀 Getting Started

### Prerequisites

- [Clarinet](https://github.com/hirosystems/clarinet) installed
- Stacks wallet for testing

### Installation

```bash
git clone <your-repo>
cd justicebit
clarinet check
```

## 📖 Usage Guide

### 🏪 For Buyers & Sellers

#### Creating an Escrow
```clarity
(contract-call? .Justicebit create-escrow 'SP2J6ZY48GV1EZ5V2V5RB9MP66SW86PYKKNRV9EJ7 1000000 u1000)
```
- `seller`: Principal address of the seller
- `amount`: Amount in microSTX to escrow
- `duration`: Block duration before expiry

#### Releasing Funds (Buyer)
```clarity
(contract-call? .Justicebit release-funds u1)
```

#### Requesting Refund (Seller, after expiry)
```clarity
(contract-call? .Justicebit refund-escrow u1)
```

### ⚖️ Dispute Resolution

#### Raising a Dispute
```clarity
(contract-call? .Justicebit raise-dispute u1 "Product not as described")
```

#### Voting on Disputes (Judges only)
```clarity
(contract-call? .Justicebit vote-on-dispute u1 "buyer")
```

#### Resolving Disputes
```clarity
(contract-call? .Justicebit resolve-dispute u1)
```

