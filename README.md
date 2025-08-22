# SkillSwap - Professional Skill Exchange Platform

A decentralized platform built on Stacks blockchain that enables professionals to exchange skills and mentorship opportunities in a peer-to-peer network.

## Features

- **Skill Listing**: Professionals can list their expertise with domain categorization
- **Mentorship Requests**: Users can request mentorship sessions from skilled professionals  
- **Experience Validation**: Built-in validation for experience levels and expertise
- **Session Management**: Complete workflow from request to completion
- **Domain Categories**: Organized by professional domains (Programming, Design, Marketing, etc.)

## Smart Contract Functions

### Public Functions
- `list-skill`: Add a new professional skill to the platform
- `remove-skill`: Remove your listed skill from availability
- `request-mentorship`: Request mentorship from a skill expert
- `approve-mentorship`: Accept a mentorship request
- `reject-mentorship`: Decline a mentorship request  
- `finish-mentorship`: Mark a mentorship session as completed

### Read-Only Functions
- `get-skill`: Retrieve skill details by ID
- `get-expert`: Get the expert principal for a skill
- `get-skill-exchange`: Retrieve exchange details by ID

## Getting Started

1. Deploy the contract to Stacks blockchain
2. List your professional skills using `list-skill`
3. Browse available skills and request mentorship
4. Manage your mentorship sessions through the platform

## License

MIT License
\`\`\`

```clarity file="project-2-artmarket/contracts/artmarket.clar"
;; ArtMarket: Digital Art Trading Platform
;; Version: 1.0.0
(define-constant ERR-NOT-AUTHORIZED (err u1))
(define-constant ERR-ARTWORK-NOT-FOUND (err u2))
(define-constant ERR-ALREADY-LISTED (err u3))
(define-constant ERR-INVALID-STATUS (err u4))
(define-constant ERR-INVALID-PRICE (err u5))
(define-constant ERR-INVALID-MEDIUM (err u6))
(define-constant ERR-INVALID-STYLE (err u7))
(define-constant ERR-INVALID-TITLE (err u8))
(define-constant ERR-INVALID-DESCRIPTION (err u9))
(define-constant ERR-OFFER-NOT-FOUND (err u10))
(define-constant ERR-SELF-OFFER (err u11))
(define-constant ERR-ARTWORK-UNAVAILABLE (err u12))
(define-constant ERR-OFFER-INVALID-STATUS (err u13))
(define-constant MIN-PRICE u1)

(define-data-var next-artwork-id uint u1)
(define-data-var next-offer-id uint u1)

(define-map digital-artworks
    uint
    {
        artist: principal,
        artwork-title: (string-utf8 50),
        description: (string-utf8 200),
        medium: (string-utf8 15),
        art-style: (string-utf8 20),
        listing-status: (string-utf8 15),
        price-stx: uint
    }
)

(define-map purchase-offers
    uint
    {
        buyer: principal,
        seller: principal,
        artwork-id: uint,
        offered-price: uint,
        offer-status: (string-utf8 15)
    }
)

(define-private (validate-medium (medium (string-utf8 15)))
    (or 
        (is-eq medium u"Digital")
        (is-eq medium u"Photography")
        (is-eq medium u"3D-Render")
        (is-eq medium u"Vector")
        (is-eq medium u"Pixel-Art")
        (is-eq medium u"Animation")
    )
)

(define-private (validate-art-style (art-style (string-utf8 20)))
    (or 
        (is-eq art-style u"Abstract")
        (is-eq art-style u"Realistic")
        (is-eq art-style u"Minimalist")
        (is-eq art-style u"Surreal")
        (is-eq art-style u"Contemporary")
    )
)

(define-private (validate-text-length (text (string-utf8 200)) (min-length uint) (max-length uint))
    (let 
        (
            (text-length (len text))
        )
        (and 
            (>= text-length min-length)
            (&lt;= text-length max-length)
        )
    )
)

(define-public (list-artwork 
    (artwork-title (string-utf8 50))
    (description (string-utf8 200))
    (medium (string-utf8 15))
    (art-style (string-utf8 20))
    (price-stx uint)
)
    (let
        (
            (artwork-id (var-get next-artwork-id))
        )
        (asserts! (validate-text-length artwork-title u3 u50) ERR-INVALID-TITLE)
        (asserts! (validate-text-length description u10 u200) ERR-INVALID-DESCRIPTION)
        (asserts! (>= price-stx MIN-PRICE) ERR-INVALID-PRICE)
        (asserts! (validate-medium medium) ERR-INVALID-MEDIUM)
        (asserts! (validate-art-style art-style) ERR-INVALID-STYLE)
        
        (map-set digital-artworks artwork-id {
            artist: tx-sender,
            artwork-title: artwork-title,
            description: description,
            medium: medium,
            art-style: art-style,
            listing-status: u"available",
            price-stx: price-stx
        })
        (var-set next-artwork-id (+ artwork-id u1))
        (ok artwork-id)
    )
)

(define-public (delist-artwork (artwork-id uint))
    (let
        (
            (artwork (unwrap! (map-get? digital-artworks artwork-id) ERR-ARTWORK-NOT-FOUND))
        )
        (asserts! (is-eq tx-sender (get artist artwork)) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get listing-status artwork) u"available") ERR-INVALID-STATUS)
        (ok (map-set digital-artworks artwork-id (merge artwork { listing-status: u"delisted" })))
    )
)

(define-public (make-offer (artwork-id uint) (offered-price uint))
    (let
        (
            (artwork (unwrap! (map-get? digital-artworks artwork-id) ERR-ARTWORK-NOT-FOUND))
            (offer-id (var-get next-offer-id))
        )
        (asserts! (is-eq (get listing-status artwork) u"available") ERR-ARTWORK-UNAVAILABLE)
        (asserts! (not (is-eq tx-sender (get artist artwork))) ERR-SELF-OFFER)
        (asserts! (&lt;= offered-price (get price-stx artwork)) ERR-INVALID-PRICE)
        
        (map-set purchase-offers offer-id {
            buyer: tx-sender,
            seller: (get artist artwork),
            artwork-id: artwork-id,
            offered-price: offered-price,
            offer-status: u"pending"
        })
        
        (map-set digital-artworks artwork-id (merge artwork { listing-status: u"offer-made" }))
        (var-set next-offer-id (+ offer-id u1))
        (ok offer-id)
    )
)

(define-public (accept-offer (offer-id uint))
    (let
        (
            (offer (unwrap! (map-get? purchase-offers offer-id) ERR-OFFER-NOT-FOUND))
            (artwork-id (get artwork-id offer))
            (artwork (unwrap! (map-get? digital-artworks artwork-id) ERR-ARTWORK-NOT-FOUND))
        )
        (asserts! (is-eq tx-sender (get seller offer)) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get offer-status offer) u"pending") ERR-OFFER-INVALID-STATUS)
        
        (map-set purchase-offers offer-id (merge offer { offer-status: u"accepted" }))
        (map-set digital-artworks artwork-id (merge artwork { listing-status: u"sold" }))
        
        (ok true)
    )
)

(define-public (decline-offer (offer-id uint))
    (let
        (
            (offer (unwrap! (map-get? purchase-offers offer-id) ERR-OFFER-NOT-FOUND))
            (artwork-id (get artwork-id offer))
            (artwork (unwrap! (map-get? digital-artworks artwork-id) ERR-ARTWORK-NOT-FOUND))
        )
        (asserts! (is-eq tx-sender (get seller offer)) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get offer-status offer) u"pending") ERR-OFFER-INVALID-STATUS)
        
        (map-set purchase-offers offer-id (merge offer { offer-status: u"declined" }))
        (map-set digital-artworks artwork-id (merge artwork { listing-status: u"available" }))
        
        (ok true)
    )
)

(define-public (finalize-sale (offer-id uint))
    (let
        (
            (offer (unwrap! (map-get? purchase-offers offer-id) ERR-OFFER-NOT-FOUND))
            (artwork-id (get artwork-id offer))
            (artwork (unwrap! (map-get? digital-artworks artwork-id) ERR-ARTWORK-NOT-FOUND))
        )
        (asserts! (or (is-eq tx-sender (get buyer offer)) (is-eq tx-sender (get seller offer))) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get offer-status offer) u"accepted") ERR-OFFER-INVALID-STATUS)
        
        (map-set purchase-offers offer-id (merge offer { offer-status: u"completed" }))
        (map-set digital-artworks artwork-id (merge artwork { listing-status: u"transferred" }))
        
        (ok true)
    )
)

(define-read-only (get-artwork (artwork-id uint))
    (ok (map-get? digital-artworks artwork-id))
)

(define-read-only (get-artist (artwork-id uint))
    (ok (get artist (unwrap! (map-get? digital-artworks artwork-id) ERR-ARTWORK-NOT-FOUND)))
)

(define-read-only (get-purchase-offer (offer-id uint))
    (ok (map-get? purchase-offers offer-id))
)
