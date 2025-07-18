# 📦 Dropbit - Decentralized Delivery Escrow

> Secure package delivery with GPS-confirmed delivery and smart contract escrow 🚚✨

## 🌟 Overview

Dropbit is a decentralized delivery platform built on Stacks blockchain that ensures secure package delivery through smart contract escrow and GPS verification. Funds are automatically released upon confirmed delivery at the correct location.

## 🚀 Features

- 🔒 **Escrow Protection**: Funds held securely until delivery confirmation
- 📍 **GPS Verification**: Delivery confirmed only when courier reaches correct location
- ⭐ **Courier Ratings**: Built-in rating system for delivery personnel
- 🛡️ **Dispute Resolution**: Fair dispute handling with admin oversight
- 💰 **Platform Fees**: Configurable fee structure for platform sustainability
- ⏰ **Expiration Handling**: Automatic refunds for expired deliveries

## 📋 Contract Functions

### Public Functions

#### `create-delivery`
Creates a new delivery request with escrow
```clarity
(create-delivery recipient pickup-lat pickup-lng delivery-lat delivery-lng description expires-in-blocks)
```

#### `accept-delivery`
Courier accepts a delivery request
```clarity
(accept-delivery delivery-id)
```

#### `pickup-package`
Courier confirms package pickup
```clarity
(pickup-package delivery-id)
```

#### `confirm-delivery`
Courier confirms delivery with GPS coordinates
```clarity
(confirm-delivery delivery-id actual-lat actual-lng)
```

#### `release-payment`
Releases escrowed funds to courier
```clarity
(release-payment delivery-id)
```

#### `dispute-delivery`
Initiates dispute resolution process
```clarity
(dispute-delivery delivery-id reason)
```

#### `cancel-delivery`
Cancels pending delivery and refunds sender
```clarity
(cancel-delivery delivery-id)
```

### Read-Only Functions

#### `get-delivery`
Retrieves delivery information
```clarity
(get-delivery delivery-id)
```

#### `get-courier-rating`
Gets courier's rating and delivery count
```clarity
(get-courier-rating courier-principal)
```

## 🔄 Delivery Status Flow

1. **pending** → Created, waiting for courier
2. **accepted** → Courier accepted the delivery
3. **in-transit** → Package picked up, in delivery
4. **delivered** → GPS-confirmed delivery
5. **completed** → Payment released
6. **disputed** → Under dispute resolution
7. **cancelled** → Cancelled by sender
8. **refunded** → Refunded after dispute


