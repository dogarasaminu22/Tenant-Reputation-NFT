# 🏠 Tenant Reputation NFT Smart Contract

A decentralized system for maintaining verifiable rental history through NFTs, powered by Stacks blockchain.

## 🎯 Features

- ✨ Landlord registration system
- 📝 Creation of tenant reputation records as NFTs
- 🔍 Transparent rental history tracking
- ⭐ Rating and review system
- 🔒 Secure and immutable records

## 🚀 Usage

### For Landlords

1. Register as a landlord:
```clarity
(contract-call? .tenant-rep register-landlord)
```

2. Create tenant record:
```clarity
(contract-call? .tenant-rep create-tenant-record 
    'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM 
    "123 Crypto Street" 
    u1625097600 
    u1656633600 
    u5 
    "Excellent tenant, always paid on time")
```

### For Tenants

1. View your rental history:
```clarity
(contract-call? .tenant-rep get-tenant-history 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM)
```

2. View specific record:
```clarity
(contract-call? .tenant-rep get-tenant-record u1)
```

## 🔧 Technical Details

- NFT-based reputation system
- Maximum rating: 5
- Stores up to 10 most recent records per tenant
- Only registered landlords can create records
- Immutable reviews and ratings

## 🤝 Contributing

Feel free to submit issues and enhancement requests!