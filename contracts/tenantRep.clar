
(define-non-fungible-token tenant-rep uint)

(define-constant contract-owner tx-sender)
(define-constant err-not-authorized (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))

(define-map tenant-records
    uint 
    {
        tenant: principal,
        landlord: principal,
        property-address: (string-ascii 100),
        start-date: uint,
        end-date: uint,
        rating: uint,
        review: (string-ascii 500)
    }
)

(define-map landlords principal bool)
(define-map tenant-history 
    principal 
    (list 10 uint)
)


(define-map landlord-ratings
    uint
    {
        rater: principal,
        landlord: principal,
        property-address: (string-ascii 100),
        responsiveness-rating: uint,
        maintenance-rating: uint,
        communication-rating: uint,
        overall-rating: uint,
        review: (string-ascii 500),
        rating-date: uint
    }
)

(define-map landlord-rating-history
    principal
    (list 50 uint)
)

(define-map landlord-reputation
    principal
    {
        total-ratings: uint,
        average-responsiveness: uint,
        average-maintenance: uint,
        average-communication: uint,
        overall-average: uint,
        total-properties-managed: uint
    }
)

(define-data-var last-rating-id uint u0)

(define-constant err-invalid-rating (err u200))
(define-constant err-already-rated (err u201))
(define-constant err-not-tenant (err u202))
(define-constant err-rating-not-found (err u203))

(define-data-var last-token-id uint u0)

(define-public (register-landlord)
    (begin
        (asserts! (is-none (map-get? landlords tx-sender)) err-already-exists)
        (ok (map-set landlords tx-sender true))
    )
)

(define-public (create-tenant-record 
    (tenant principal)
    (property-address (string-ascii 100))
    (start-date uint)
    (end-date uint)
    (rating uint)
    (review (string-ascii 500)))
    (let
        ((new-id (+ (var-get last-token-id) u1)))
        (asserts! (is-some (map-get? landlords tx-sender)) err-not-authorized)
        (asserts! (<= rating u5) (err u103))
        (try! (nft-mint? tenant-rep new-id tenant))
        (map-set tenant-records new-id
            {
                tenant: tenant,
                landlord: tx-sender,
                property-address: property-address,
                start-date: start-date,
                end-date: end-date,
                rating: rating,
                review: review
            }
        )
        (var-set last-token-id new-id)
        (match (map-get? tenant-history tenant)
            prev-history (map-set tenant-history tenant (unwrap-panic (as-max-len? (concat prev-history (list new-id)) u10)))
            (map-set tenant-history tenant (list new-id))
        )
        (ok new-id)
    )
)

(define-read-only (get-tenant-record (token-id uint))
    (ok (map-get? tenant-records token-id))
)

(define-read-only (get-tenant-history (tenant principal))
    (ok (map-get? tenant-history tenant))
)

(define-read-only (get-last-token-id)
    (ok (var-get last-token-id))
)

(define-read-only (get-token-uri (token-id uint))
    (ok none)
)

(define-read-only (get-owner (token-id uint))
    (ok (nft-get-owner? tenant-rep token-id))
)

(define-public (transfer (token-id uint) (sender principal) (recipient principal))
    (begin
        (asserts! (is-eq tx-sender sender) err-not-authorized)
        (nft-transfer? tenant-rep token-id sender recipient)
    )
)



(define-public (burn (token-id uint) (sender principal))
    (begin
        (asserts! (is-eq tx-sender sender) err-not-authorized)
        (nft-burn? tenant-rep token-id sender)
    )
)
(define-public (get-landlord (token-id uint))
    (ok (get landlord (map-get? tenant-records token-id)))
)
(define-public (get-property-address (token-id uint))
    (ok (get property-address (map-get? tenant-records token-id)))
)
(define-public (get-start-date (token-id uint))
    (ok (get start-date (map-get? tenant-records token-id)))
)
(define-public (get-end-date (token-id uint))
    (ok (get end-date (map-get? tenant-records token-id)))
)
(define-public (get-rating (token-id uint))
    (ok (get rating (map-get? tenant-records token-id)))
)


(define-map disputed-records 
    uint 
    {
        is-disputed: bool,
        dispute-reason: (string-ascii 200),
        dispute-date: uint
    }
)

(define-constant DISPUTE-WINDOW u604800)

(define-public (dispute-rating 
    (token-id uint) 
    (reason (string-ascii 200)))
    (let (
        (record (unwrap! (map-get? tenant-records token-id) err-not-found))
        (current-time stacks-block-height)
    )
        (asserts! (is-eq tx-sender (get tenant record)) err-not-authorized)
        (asserts! (< (- current-time (get end-date record)) DISPUTE-WINDOW) (err u104))
        (ok (map-set disputed-records token-id {
            is-disputed: true,
            dispute-reason: reason,
            dispute-date: current-time
        }))
    )
)

(define-read-only (get-dispute-status (token-id uint))
    (ok (map-get? disputed-records token-id))
)


(define-map tenant-stats
    principal
    {
        total-ratings: uint,
        average-rating: uint,
        total-properties: uint
    }
)

(define-public (calculate-tenant-stats (tenant principal))
    (let (
        (history (unwrap! (map-get? tenant-history tenant) err-not-found))
        (ratings-sum (fold + (map get-rating-for-token history) u0))
        (total-props (len history))
    )
        (asserts! (> total-props u0) err-not-found)
        (ok (map-set tenant-stats tenant {
            total-ratings: ratings-sum,
            average-rating: (/ ratings-sum total-props),
            total-properties: total-props
        }))
    )
)

(define-private (get-rating-for-token (token-id uint))
    (match (map-get? tenant-records token-id)
        record (get rating record)
        u0
    )
)

(define-read-only (get-tenant-stats (tenant principal))
    (ok (map-get? tenant-stats tenant))
)


(define-public (rate-landlord
    (landlord principal)
    (property-address (string-ascii 100))
    (responsiveness-rating uint)
    (maintenance-rating uint)
    (communication-rating uint)
    (review (string-ascii 500)))
    (let
        ((new-rating-id (+ (var-get last-rating-id) u1))
         (overall-rating (/ (+ responsiveness-rating maintenance-rating communication-rating) u3)))
        
        (asserts! (<= responsiveness-rating u5) err-invalid-rating)
        (asserts! (<= maintenance-rating u5) err-invalid-rating)
        (asserts! (<= communication-rating u5) err-invalid-rating)
        (asserts! (> responsiveness-rating u0) err-invalid-rating)
        (asserts! (> maintenance-rating u0) err-invalid-rating)
        (asserts! (> communication-rating u0) err-invalid-rating)
        
        (map-set landlord-ratings new-rating-id
            {
                rater: tx-sender,
                landlord: landlord,
                property-address: property-address,
                responsiveness-rating: responsiveness-rating,
                maintenance-rating: maintenance-rating,
                communication-rating: communication-rating,
                overall-rating: overall-rating,
                review: review,
                rating-date: stacks-block-height
            }
        )
        
        (var-set last-rating-id new-rating-id)
        
        (match (map-get? landlord-rating-history landlord)
            prev-history (map-set landlord-rating-history landlord 
                (unwrap-panic (as-max-len? (concat prev-history (list new-rating-id)) u50)))
            (map-set landlord-rating-history landlord (list new-rating-id))
        )
        
        (try! (update-landlord-reputation landlord))
        (ok new-rating-id)
    )
)

(define-public (update-landlord-reputation (landlord principal))
    (let
        ((rating-history (unwrap! (map-get? landlord-rating-history landlord) err-rating-not-found))
         (total-ratings (len rating-history))
         (responsiveness-sum (fold + (map get-responsiveness-rating rating-history) u0))
         (maintenance-sum (fold + (map get-maintenance-rating rating-history) u0))
         (communication-sum (fold + (map get-communication-rating rating-history) u0))
         (overall-sum (fold + (map get-overall-rating rating-history) u0)))
        
        (asserts! (> total-ratings u0) err-rating-not-found)
        
        (ok (map-set landlord-reputation landlord
            {
                total-ratings: total-ratings,
                average-responsiveness: (/ responsiveness-sum total-ratings),
                average-maintenance: (/ maintenance-sum total-ratings),
                average-communication: (/ communication-sum total-ratings),
                overall-average: (/ overall-sum total-ratings),
                total-properties-managed: total-ratings
            }
        ))
    )
)

(define-private (get-responsiveness-rating (rating-id uint))
    (match (map-get? landlord-ratings rating-id)
        rating (get responsiveness-rating rating)
        u0
    )
)

(define-private (get-maintenance-rating (rating-id uint))
    (match (map-get? landlord-ratings rating-id)
        rating (get maintenance-rating rating)
        u0
    )
)

(define-private (get-communication-rating (rating-id uint))
    (match (map-get? landlord-ratings rating-id)
        rating (get communication-rating rating)
        u0
    )
)

(define-private (get-overall-rating (rating-id uint))
    (match (map-get? landlord-ratings rating-id)
        rating (get overall-rating rating)
        u0
    )
)

(define-read-only (get-landlord-rating (rating-id uint))
    (ok (map-get? landlord-ratings rating-id))
)

(define-read-only (get-landlord-reputation (landlord principal))
    (ok (map-get? landlord-reputation landlord))
)

(define-read-only (get-landlord-rating-history (landlord principal))
    (ok (map-get? landlord-rating-history landlord))
)

(define-read-only (get-landlord-reviews (landlord principal))
    (let
        ((rating-history (default-to (list) (map-get? landlord-rating-history landlord))))
        (ok (map get-rating-details rating-history))
    )
)

(define-private (get-rating-details (rating-id uint))
    (map-get? landlord-ratings rating-id)
)

(define-read-only (is-landlord-highly-rated (landlord principal))
    (match (map-get? landlord-reputation landlord)
        reputation (ok (and (>= (get overall-average reputation) u4) (>= (get total-ratings reputation) u3)))
        (ok false)
    )
)

(define-read-only (get-last-rating-id)
    (ok (var-get last-rating-id))
)