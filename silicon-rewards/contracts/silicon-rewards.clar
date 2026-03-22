;; Silicon Rewards DAO

;; Merit-based DAO with three-tier consensus:
;;   1. Proof of Merit  - verified skill assessment
;;   2. Contribution Tracking - code, reviews, governance
;;   3. Reputation Scoring   - time-decayed influence
;;
;; Influence Score:
;;   score = (code-contributions * RECENT-BOOST) + (peer-reviews * 2) + (gov-votes * 1) + reputation
;;   RECENT-BOOST = 3 if contributed within ~1 week, else 1
;;
;; Quadratic Funding weight = isqrt(individual-contribution)

;; ============================================================
;; CONSTANTS
;; ============================================================

(define-constant CONTRACT-OWNER tx-sender)

;; Error codes
(define-constant ERR-NOT-AUTHORIZED      (err u100))
(define-constant ERR-ALREADY-REGISTERED  (err u101))
(define-constant ERR-NOT-REGISTERED      (err u102))
(define-constant ERR-PROPOSAL-NOT-FOUND  (err u103))
(define-constant ERR-PROPOSAL-CLOSED     (err u104))
(define-constant ERR-ALREADY-VOTED       (err u105))
(define-constant ERR-COOLING-OFF         (err u106))
(define-constant ERR-INSUFFICIENT-MERIT  (err u107))
(define-constant ERR-INVALID-AMOUNT      (err u108))
(define-constant ERR-PROPOSAL-ACTIVE     (err u109))

;; Governance parameters
(define-constant MIN-PROPOSAL-MERIT       u10)
(define-constant COOLING-OFF-BLOCKS       u144)     ;; ~1 day on Stacks
(define-constant MAJOR-DECISION-THRESHOLD u1000000) ;; microSTX
(define-constant RECENCY-WINDOW-BLOCKS    u1008)    ;; ~1 week

;; On-chain reward units per action
(define-constant REWARD-CODE-CONTRIBUTION u30)
(define-constant REWARD-PEER-REVIEW       u20)
(define-constant REWARD-GOVERNANCE-VOTE   u10)

;; ============================================================
;; DATA VARS
;; ============================================================

(define-data-var proposal-nonce       uint u0)
(define-data-var qf-project-nonce     uint u0)
(define-data-var total-rewards-minted uint u0)
(define-data-var qf-pool-balance      uint u0)

;; ============================================================
;; DATA MAPS
;; ============================================================

(define-map members
  { address: principal }
  {
    registered-at:           uint,
    reputation:              uint,
    code-contributions:      uint,
    peer-reviews:            uint,
    governance-votes:        uint,
    reward-balance:          uint,
    skill-verified:          bool,
    last-contribution-block: uint
  }
)

(define-map proposals
  { id: uint }
  {
    proposer:        principal,
    title:           (string-ascii 64),
    description:     (string-ascii 256),
    funding-request: uint,
    yes-votes:       uint,
    no-votes:        uint,
    created-at:      uint,
    ends-at:         uint,
    executed:        bool,
    passed:          bool
  }
)

(define-map votes
  { proposal-id: uint, voter: principal }
  { in-favor: bool }
)

(define-map skill-verifications
  { address: principal, skill: (string-ascii 32) }
  { verified-at: uint, verifier: principal }
)

(define-map verifiers
  { address: principal }
  { active: bool }
)

(define-map qf-projects
  { id: uint }
  {
    title:        (string-ascii 64),
    owner:        principal,
    total-raised: uint,
    active:       bool
  }
)

(define-map qf-contributions
  { project-id: uint, contributor: principal }
  { amount: uint }
)

;; ============================================================
;; PRIVATE HELPERS
;; ============================================================

;; Integer square root (Babylonian, 8 iterations) - used for quadratic funding
(define-private (isqrt (n uint))
  (if (is-eq n u0)
    u0
    (let
      (
        (x1 (/ (+ n u1) u2))
        (x2 (/ (+ x1 (/ n x1)) u2))
        (x3 (/ (+ x2 (/ n x2)) u2))
        (x4 (/ (+ x3 (/ n x3)) u2))
        (x5 (/ (+ x4 (/ n x4)) u2))
        (x6 (/ (+ x5 (/ n x5)) u2))
        (x7 (/ (+ x6 (/ n x6)) u2))
        (x8 (/ (+ x7 (/ n x7)) u2))
      )
      (if (<= x8 x7) x8 x7)
    )
  )
)

;; Compute influence score for any principal
(define-private (calc-influence (addr principal))
  (match (map-get? members { address: addr })
    m
      (let
        (
          (blocks-since (- block-height (get last-contribution-block m)))
          (boost (if (< blocks-since RECENCY-WINDOW-BLOCKS) u3 u1))
        )
        (+
          (* (get code-contributions m) boost)
          (* (get peer-reviews m) u2)
          (get governance-votes m)
          (get reputation m)
        )
      )
    u0
  )
)

;; Mint reward tokens into a member's on-chain balance
(define-private (mint-reward (addr principal) (amount uint))
  (match (map-get? members { address: addr })
    m
      (begin
        (map-set members { address: addr }
          (merge m { reward-balance: (+ (get reward-balance m) amount) })
        )
        (var-set total-rewards-minted (+ (var-get total-rewards-minted) amount))
        true
      )
    false
  )
)

;; Return true if caller is owner or an active verifier
(define-private (is-authorized)
  (or
    (is-eq tx-sender CONTRACT-OWNER)
    (default-to false
      (get active (map-get? verifiers { address: tx-sender }))
    )
  )
)

;; ============================================================
;; MEMBER REGISTRATION
;; ============================================================

;; Register the transaction sender as a new DAO member
(define-public (register)
  (begin
    (asserts! (is-none (map-get? members { address: tx-sender })) ERR-ALREADY-REGISTERED)
    (map-set members { address: tx-sender }
      {
        registered-at:           block-height,
        reputation:              u5,
        code-contributions:      u0,
        peer-reviews:            u0,
        governance-votes:        u0,
        reward-balance:          u0,
        skill-verified:          false,
        last-contribution-block: block-height
      }
    )
    (ok true)
  )
)

(define-read-only (get-member (addr principal))
  (map-get? members { address: addr })
)

(define-read-only (get-influence-score (addr principal))
  (ok (calc-influence addr))
)

;; ============================================================
;; PROOF OF MERIT - SKILL VERIFICATION
;; ============================================================

;; Grant verifier status (owner only)
(define-public (add-verifier (addr principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set verifiers { address: addr } { active: true })
    (ok true)
  )
)

;; Revoke verifier status (owner only)
(define-public (revoke-verifier (addr principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set verifiers { address: addr } { active: false })
    (ok true)
  )
)

;; Issue a skill verification badge to a registered member
(define-public (verify-skill (addr principal) (skill (string-ascii 32)))
  (begin
    (asserts! (is-authorized) ERR-NOT-AUTHORIZED)
    (asserts! (is-some (map-get? members { address: addr })) ERR-NOT-REGISTERED)
    (map-set skill-verifications
      { address: addr, skill: skill }
      { verified-at: block-height, verifier: tx-sender }
    )
    (match (map-get? members { address: addr })
      m
        (begin
          (map-set members { address: addr }
            (merge m {
              skill-verified: true,
              reputation: (+ (get reputation m) u10)
            })
          )
          (ok true)
        )
      ERR-NOT-REGISTERED
    )
  )
)

(define-read-only (is-skill-verified (addr principal) (skill (string-ascii 32)))
  (is-some (map-get? skill-verifications { address: addr, skill: skill }))
)

;; ============================================================
;; CONTRIBUTION TRACKING
;; ============================================================

;; Record a verified code contribution - callable by owner or active verifier
(define-public (record-code-contribution (addr principal))
  (begin
    (asserts! (is-authorized) ERR-NOT-AUTHORIZED)
    (match (map-get? members { address: addr })
      m
        (begin
          (map-set members { address: addr }
            (merge m {
              code-contributions:      (+ (get code-contributions m) u1),
              last-contribution-block: block-height
            })
          )
          (mint-reward addr REWARD-CODE-CONTRIBUTION)
          (ok true)
        )
      ERR-NOT-REGISTERED
    )
  )
)

;; Record a verified peer review - callable by owner or active verifier
(define-public (record-peer-review (addr principal))
  (begin
    (asserts! (is-authorized) ERR-NOT-AUTHORIZED)
    (match (map-get? members { address: addr })
      m
        (begin
          (map-set members { address: addr }
            (merge m {
              peer-reviews:            (+ (get peer-reviews m) u1),
              last-contribution-block: block-height
            })
          )
          (mint-reward addr REWARD-PEER-REVIEW)
          (ok true)
        )
      ERR-NOT-REGISTERED
    )
  )
)

;; ============================================================
;; GOVERNANCE - PROPOSALS
;; ============================================================

;; Submit a governance proposal
(define-public (submit-proposal
    (title       (string-ascii 64))
    (description (string-ascii 256))
    (funding-req uint)
    (duration    uint))
  (let
    (
      (influence (calc-influence tx-sender))
      (pid       (var-get proposal-nonce))
      (is-major  (> funding-req MAJOR-DECISION-THRESHOLD))
    )
    (asserts! (is-some (map-get? members { address: tx-sender })) ERR-NOT-REGISTERED)
    (asserts! (>= influence MIN-PROPOSAL-MERIT) ERR-INSUFFICIENT-MERIT)
    ;; Major decisions must include at least one cooling-off period in their duration
    (asserts! (or (not is-major) (>= duration COOLING-OFF-BLOCKS)) ERR-COOLING-OFF)
    (map-set proposals { id: pid }
      {
        proposer:        tx-sender,
        title:           title,
        description:     description,
        funding-request: funding-req,
        yes-votes:       u0,
        no-votes:        u0,
        created-at:      block-height,
        ends-at:         (+ block-height duration),
        executed:        false,
        passed:          false
      }
    )
    (var-set proposal-nonce (+ pid u1))
    (ok pid)
  )
)

;; Cast an influence-weighted vote on an open proposal
(define-public (cast-vote (proposal-id uint) (in-favor bool))
  (let
    (
      (proposal (unwrap! (map-get? proposals { id: proposal-id }) ERR-PROPOSAL-NOT-FOUND))
      (weight   (let ((s (calc-influence tx-sender))) (if (> s u0) s u1)))
    )
    (asserts! (is-some (map-get? members { address: tx-sender })) ERR-NOT-REGISTERED)
    (asserts! (<= block-height (get ends-at proposal)) ERR-PROPOSAL-CLOSED)
    (asserts!
      (is-none (map-get? votes { proposal-id: proposal-id, voter: tx-sender }))
      ERR-ALREADY-VOTED
    )
    (map-set votes
      { proposal-id: proposal-id, voter: tx-sender }
      { in-favor: in-favor }
    )
    (if in-favor
      (map-set proposals { id: proposal-id }
        (merge proposal { yes-votes: (+ (get yes-votes proposal) weight) })
      )
      (map-set proposals { id: proposal-id }
        (merge proposal { no-votes: (+ (get no-votes proposal) weight) })
      )
    )
    ;; Increment governance-votes counter and issue participation reward
    (match (map-get? members { address: tx-sender })
      m
        (map-set members { address: tx-sender }
          (merge m { governance-votes: (+ (get governance-votes m) u1) })
        )
      false
    )
    (mint-reward tx-sender REWARD-GOVERNANCE-VOTE)
    (ok true)
  )
)

;; Finalize a proposal once its voting window has elapsed
(define-public (finalize-proposal (proposal-id uint))
  (let ((proposal (unwrap! (map-get? proposals { id: proposal-id }) ERR-PROPOSAL-NOT-FOUND)))
    (asserts! (> block-height (get ends-at proposal)) ERR-PROPOSAL-ACTIVE)
    (asserts! (not (get executed proposal)) ERR-PROPOSAL-CLOSED)
    (let ((passed (> (get yes-votes proposal) (get no-votes proposal))))
      (map-set proposals { id: proposal-id }
        (merge proposal { executed: true, passed: passed })
      )
      (ok passed)
    )
  )
)

(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals { id: proposal-id })
)

(define-read-only (get-vote (proposal-id uint) (voter principal))
  (map-get? votes { proposal-id: proposal-id, voter: voter })
)

(define-read-only (get-proposal-count)
  (var-get proposal-nonce)
)

;; ============================================================
;; QUADRATIC FUNDING
;; ============================================================

;; Register a community project eligible for quadratic funding
(define-public (register-qf-project (title (string-ascii 64)))
  (let ((pid (var-get qf-project-nonce)))
    (asserts! (is-some (map-get? members { address: tx-sender })) ERR-NOT-REGISTERED)
    (map-set qf-projects { id: pid }
      {
        title:        title,
        owner:        tx-sender,
        total-raised: u0,
        active:       true
      }
    )
    (var-set qf-project-nonce (+ pid u1))
    (ok pid)
  )
)

;; Contribute STX to an active quadratic funding project
(define-public (contribute-qf (project-id uint) (amount uint))
  (let ((project (unwrap! (map-get? qf-projects { id: project-id }) ERR-PROPOSAL-NOT-FOUND)))
    (asserts! (is-some (map-get? members { address: tx-sender })) ERR-NOT-REGISTERED)
    (asserts! (get active project) ERR-PROPOSAL-CLOSED)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (let
      (
        (prev-amount
          (default-to u0
            (get amount
              (map-get? qf-contributions
                { project-id: project-id, contributor: tx-sender }))
          )
        )
      )
      (map-set qf-contributions
        { project-id: project-id, contributor: tx-sender }
        { amount: (+ prev-amount amount) }
      )
      (map-set qf-projects { id: project-id }
        (merge project { total-raised: (+ (get total-raised project) amount) })
      )
      (ok true)
    )
  )
)

;; Deposit STX into the quadratic matching pool (owner only)
(define-public (fund-qf-pool (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (var-set qf-pool-balance (+ (var-get qf-pool-balance) amount))
    (ok true)
  )
)

;; Return the quadratic funding weight (sqrt of contribution) for one contributor
(define-read-only (get-qf-weight (project-id uint) (contributor principal))
  (match (map-get? qf-contributions { project-id: project-id, contributor: contributor })
    c (ok (isqrt (get amount c)))
    (ok u0)
  )
)

(define-read-only (get-qf-project (project-id uint))
  (map-get? qf-projects { id: project-id })
)

(define-read-only (get-qf-pool)
  (var-get qf-pool-balance)
)

;; ============================================================
;; REPUTATION MANAGEMENT
;; ============================================================

;; Reduce a member's reputation for bad-faith behaviour (owner only)
(define-public (slash-reputation (addr principal) (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (match (map-get? members { address: addr })
      m
        (begin
          (map-set members { address: addr }
            (merge m {
              reputation:
                (if (>= (get reputation m) amount)
                  (- (get reputation m) amount)
                  u0)
            })
          )
          (ok true)
        )
      ERR-NOT-REGISTERED
    )
  )
)

;; ============================================================
;; READ-ONLY UTILITIES
;; ============================================================

(define-read-only (get-total-rewards)
  (var-get total-rewards-minted)
)

(define-read-only (get-reward-balance (addr principal))
  (match (map-get? members { address: addr })
    m (ok (get reward-balance m))
    ERR-NOT-REGISTERED
  )
)
