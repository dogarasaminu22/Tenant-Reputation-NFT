;; (impl-trait 'SP2PABAF9FTAJYNFZH93XENAJ8FVY99RRM50D2JG9.nft-trait.nft-trait)

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