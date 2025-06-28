# Sheltra - Decentralized Shelter & Donor Matching Platform

A Clarity smart contract for connecting homeless shelters with donors on the Stacks blockchain, enabling transparent and efficient fundraising with built-in verification and matching algorithms.

## Overview

Sheltra revolutionizes homeless shelter funding by creating a trustless platform where:
- **Shelters** can register, set funding goals, and track occupancy
- **Donors** can find and support verified shelters based on specific needs
- **Matching system** connects donors with shelters using preference-based algorithms
- **Transparency** ensures all donations are tracked on-chain with minimal platform fees (2.5%)

## Features

### 🏠 Shelter Management
- Register shelters with capacity, location, and funding goals
- Track real-time occupancy and availability
- Specify needs (food, medical, clothing, education, transportation, legal aid)
- Owner verification system for trust and credibility
- Emergency withdrawal functionality for urgent situations

### 💰 Donor System  
- Register as a donor with preferences and donation limits
- Set preferred locations and types of assistance
- Direct donations to verified shelters
- Rate and provide feedback on shelter experiences
- Track donation history and impact

### 🤝 Intelligent Matching
- Algorithm-based matching between donors and shelters
- Preference-based filtering (location, needs, capacity)
- Pending and completed match tracking
- Multi-step donation process for security

### 📊 Analytics & Transparency
- Real-time platform statistics
- Shelter capacity and funding information
- Donation tracking and fee transparency
- Rating and feedback system

## Usage

### For Shelter Operators

#### 1. Register Your Shelter
```clarity
(contract-call? .Sheltra register-shelter
  "Downtown Shelter"           ;; name
  "123 Main St, City"         ;; location  
  u50                         ;; capacity
  u100000                     ;; funding goal (microSTX)
  "contact@shelter.org"       ;; contact info
  {                           ;; needs
    food: true,
    medical: true, 
    clothing: false,
    education: true,
    transportation: false,
    legal-aid: true
  })
```

#### 2. Update Occupancy
```clarity
(contract-call? .Sheltra update-occupancy u0 u35) ;; shelter-id, current occupancy
```

#### 3. Emergency Withdraw (if needed)
```clarity
(contract-call? .Sheltra emergency-withdraw u0) ;; shelter-id
```

### For Donors

#### 1. Register as Donor
```clarity
(contract-call? .Sheltra register-donor
  "John Doe"                  ;; name
  {                           ;; preferences
    preferred-location: (some "Downtown"),
    max-donation: u10000,
    preferred-needs: (list "food" "medical" "education"),
    recurring: false
  })
```

#### 2. Direct Donation
```clarity
(contract-call? .Sheltra donate-to-shelter u0 u5000) ;; shelter-id, amount
```

#### 3. Create Smart Match
```clarity
(contract-call? .Sheltra create-match u0 u0 u3000) ;; shelter-id, donor-id, amount
```

#### 4. Complete Match
```clarity
(contract-call? .Sheltra complete-match u0) ;; match-id
```

#### 5. Rate Shelter
```clarity
(contract-call? .Sheltra rate-shelter u0 u0 u5 "Excellent service and transparency")
```

### Query Functions

#### Get Shelter Information
```clarity
(contract-call? .Sheltra get-shelter u0)
(contract-call? .Sheltra get-shelter-capacity-info u0)
(contract-call? .Sheltra get-shelter-funding-info u0)
(contract-call? .Sheltra get-shelter-needs u0)
```

#### Get Platform Statistics
```clarity
(contract-call? .Sheltra get-platform-stats)
```

## Contract Architecture

### Data Structures

- **Shelters**: Core shelter information with funding and occupancy tracking
- **Donors**: Donor profiles with donation history and verification status  
- **Matches**: Smart matching system with pending/completed states
- **Shelter Needs**: Categorized assistance requirements
- **Donor Preferences**: Filtering criteria for intelligent matching
- **Ratings**: Feedback system for accountability

### Security Features

- Owner-only functions for verification
- Authorization checks for all sensitive operations
- Amount validation and capacity constraints
- Emergency withdrawal for urgent situations
- Platform fee calculation with transparent rates

## Error Codes

| Code | Description |
|------|-------------|
| u100 | Owner only operation |
| u101 | Resource not found |
| u102 | Resource already exists |
| u103 | Unauthorized access |
| u104 | Insufficient funds |
| u105 | Invalid capacity |
| u106 | Shelter at full capacity |
| u107 | Verification required |
| u108 | Invalid amount |

## Development

### Prerequisites
- [Clarinet](https://github.com/hirosystems/clarinet) for local development
- [Stacks CLI](https://docs.stacks.co/docs/cli) for deployment

### Testing
```bash
npm test
```

### Deployment
```bash
clarinet deploy --testnet
```

## Contributing

1. Fork the repository
2. Create a feature branch
3. Add comprehensive tests
4. Submit a pull request

## License

MIT License - see [LICENSE](LICENSE) for details.

## Support

For issues and questions:
- Create an issue on GitHub
- Contact: [Your contact information]

---

*Building a more transparent and efficient future for homeless shelter funding through blockchain technology.*
